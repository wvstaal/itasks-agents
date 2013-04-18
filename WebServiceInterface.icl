implementation module WebServiceInterface

import AgentController
import Sleep
import MersenneTwister
import Time
import JSON
import HttpClient
import Map
import StdEnv
import StdDebug

:: ServiceResponse = 
	{ sessionId :: String
	, task		:: TaskServiceRep
	}

derive JSONDecode ServiceResponse, TaskServiceRep

iTasksRequest :: SimpleHttpRequest
iTasksRequest = {SimpleHttpRequest
			   | req_method 	= "GET"
			   , req_path   	= "/?format=json-service"
			   , req_headers 	= newMap
			   , req_data		= ""
			   , arg_cookies	= newMap
			   }

taskRepToAgentTasks :: TaskServiceRep -> [AgentTask]
taskRepToAgentTasks task
	| isJust task.TaskServiceRep.tag 
		=	[{AgentTask
		   	|tag	  	= fromJust task.TaskServiceRep.tag
			,value	  	= task.TaskServiceRep.value
			,repValue 	= task.TaskServiceRep.repValue
			,enabled	= True
			,taskId		= fromJust task.TaskServiceRep.taskId
			}:otherTasks]
	= otherTasks
where
	actionToTask (taskId, enabled, tag)
					= {AgentTask|tag=tag, value=Nothing, repValue=Nothing, enabled=enabled, taskId = taskId}
	actions    		= map actionToTask task.TaskServiceRep.actions
	otherTasks 		= actions ++ foldr ((++) o taskRepToAgentTasks) [] task.parts						

webServiceTaskReceiver :: String Int -> TaskReceiver (Maybe [AgentTask])
webServiceTaskReceiver server port = TaskReceiver recv
where
	request recvState
		#mbSessionId = recvState.TaskReceiverState.sessionId
		| isNothing mbSessionId = (iTasksRequest, recvState)
		= ({SimpleHttpRequest|iTasksRequest & req_path = iTasksRequest.SimpleHttpRequest.req_path +++ "&session=" +++ (fromJust mbSessionId) +++ "&"},
			 {TaskReceiverState|recvState&sessionId = mbSessionId}) //? type checker fails if this is not done
	recv recvState
	#(req, recvState) 		= request recvState
	#(mbResp, world)   		= executeHttpRequest server port req recvState.TaskReceiverState.world
	| isNothing mbResp 		= (Nothing, {TaskReceiverState|recvState & world = world})
	#resp 					= fromJust mbResp
	#mbServResp 			= fromJSON (trace_n (resp.SimpleHttpResponse.rsp_data) (fromString resp.SimpleHttpResponse.rsp_data) )
	| isNothing mbServResp 	= (Nothing, {TaskReceiverState|recvState & world = world})
	#servResp				= fromJust mbServResp
	#tasks 					= taskRepToAgentTasks (servResp.task) 
	= (Just tasks, {TaskReceiverState|recvState & world=world, sessionId = Just servResp.ServiceResponse.sessionId})
	
webServiceActionHandler :: String Int -> ActionHandler Bool
webServiceActionHandler server port = ActionHandler act
where
	act actions actState 
		#!(success, world) = foldl (doAction actState.ActionHandlerState.sessionId) (True, actState.ActionHandlerState.world) actions
		= (success, {ActionHandlerState|actState&world=world})
	doAction sessionId (success, world) act
		#(mbPath, world) 	= handleAction act world
		| isNothing mbPath 	= (success, world)
		#req =	{SimpleHttpRequest
				|iTasksRequest&
				req_path = (fromJust mbPath) +++ "&session=" +++ sessionId
				}
		#!(resp, world) = (executeHttpRequest server port req world)
		= (if success (isJust resp) False, world)
	handleAction (ActAction taskId val) world	
		#path = iTasksRequest.SimpleHttpRequest.req_path +++ "&actionEvent=" +++ (toString o toJSON) [toJSON (toString taskId), toJSON val]
		= (Just path, world)
	handleAction (EditAction taskId val) world	 
		#path = iTasksRequest.SimpleHttpRequest.req_path +++ "&editEvent=" +++ (toString o toJSON) [toJSON (toString taskId), toJSON "", toJSON val]
		= (Just path, world)
	handleAction (WaitAction min max) world 
		#(Timestamp t, world) = time world
		#rnd 				  = hd (genRandInt t)
		#wait				  = min + ((abs rnd) rem (max-min))   
		= (Nothing, sleep wait world)