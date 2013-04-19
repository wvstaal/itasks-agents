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

:: TaskServiceRep = 
	{ taskId   :: Maybe TaskId
	, parts	   :: [TaskServiceRep]
	, tag      :: Maybe String
	, value	   :: Maybe JSONNode
	, repValue :: Maybe JSONNode
	, actions  :: [(TaskId, Bool, String)]
	}	
	
:: ServiceResponse = 
	{ sessionId :: String
	, task		:: TaskServiceRep
	}

derive JSONDecode ServiceResponse, TaskServiceRep, TaskId

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

webServiceTaskReceiver :: String Int -> TaskProducer
webServiceTaskReceiver server port = recv
where
	request prodState
		#mbSessionId = prodState.TaskProducerState.sessionId
		| isNothing mbSessionId = (iTasksRequest, prodState)
		= ({SimpleHttpRequest|iTasksRequest & req_path = iTasksRequest.SimpleHttpRequest.req_path +++ "&session=" +++ (fromJust mbSessionId) +++ "&"},
			 {TaskProducerState|prodState&sessionId = mbSessionId}) //? type checker fails if this is not done
	recv prodState
	#(req, prodState) 		= request prodState
	#(mbResp, world)   		= executeHttpRequest server port req prodState.TaskProducerState.world
	| isNothing mbResp 		= trace_n "no response" (Nothing, {TaskProducerState|prodState & world = world})
	#resp 					= fromJust mbResp
	#mbServResp 			= fromJSON (trace_n (resp.SimpleHttpResponse.rsp_data) (fromString resp.SimpleHttpResponse.rsp_data) )
	| isNothing mbServResp 	= trace_n "no json" (Nothing, {TaskProducerState|prodState & world = world})
	#servResp				= fromJust mbServResp
	#tasks 					= taskRepToAgentTasks (servResp.task) 
	= (Just tasks, {TaskProducerState|prodState & world=world, sessionId = Just servResp.ServiceResponse.sessionId})
	
urlEncode :: String -> String
urlEncode s = foldl (+++) "" (map (replace o toString) [c \\ c <-: s])
where
	replace " " = "%20"
	replace s = s
	
webServiceActionHandler :: String Int -> ActionConsumer
webServiceActionHandler server port = act
where
	act actState 
		#!(success, world) = foldl (doAction actState.ActionConsumerState.sessionId) (True, actState.ActionConsumerState.world) actState.ActionConsumerState.actions
		= {ActionConsumerState|actState&world=world}
	doAction sessionId (success, world) act
		#(mbPath, world) 	= handleAction act world
		| isNothing mbPath 	= (success, world)
		#req =	{SimpleHttpRequest
				|iTasksRequest&
				req_path = urlEncode ((fromJust mbPath) +++ "&session=" +++ sessionId)
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