implementation module AgentController

import Agent
import Time
import StdEnv
import Sleep
import StdDebug

instance Monad TaskReceiver
where
	ret :: a -> TaskReceiver a
	ret x = TaskReceiver (\s. (x, s))
	(>>>) infixr 5 :: (TaskReceiver a) (a -> TaskReceiver b) -> TaskReceiver b
	(>>>) (TaskReceiver tr) f = TaskReceiver (\s. (\(a, s). let (TaskReceiver f`) = f a in f` s) (tr s))
	
instance Monad ActionHandler
where
	ret :: a -> ActionHandler a
	ret x = ActionHandler (\aa s. (x, s))
	(>>>) infixr 5 :: (ActionHandler a) (a -> ActionHandler b) -> ActionHandler b
	(>>>) (ActionHandler ah) f = ActionHandler (\aa s. (\(a, s). let (ActionHandler f`) = f a in f` aa s) (ah aa s))
	
run :: (Agent s) (TaskReceiver (Maybe [AgentTask])) (ActionHandler a) *World -> *World
run agent=:{initial, activity, id} (TaskReceiver recv) (ActionHandler act) world
 	= execute initial {world=world, sessionId=Nothing, lastTime = Timestamp 0, agentId = id}
where
	execute agentState recvState 
		#!(mbTasks, recvState) 	= recv recvState
		| isNothing mbTasks   	= recvState.TaskReceiverState.world
		#!tasks = fromJust mbTasks
		#!mbActions = activity agentState tasks
		#!(t, world) = time recvState.TaskReceiverState.world
		| isNothing mbActions = execute agentState {TaskReceiverState|recvState & world = world, lastTime = t}
		#!(Just (agentState, actions)) = mbActions
		#!(t, world) = time recvState.TaskReceiverState.world
		#!(_, {ActionHandlerState|world}) = act actions {ActionHandlerState|startTime = t, agentId=id, world=world, sessionId=fromJust recvState.TaskReceiverState.sessionId}
		#!(t, world) = time world
		= execute agentState {TaskReceiverState|recvState & world = world, lastTime = t}
		
delayTaskReceiver :: Int -> TaskReceiver Timestamp
delayTaskReceiver secs = TaskReceiver recv
where
	recv recvState=:{world,lastTime}
	#(Timestamp lastTime) = lastTime
	#(Timestamp t, world) = time world
	| t - lastTime < secs
		#world = sleep ((secs - (t - lastTime)) * 1000) world
		= (Timestamp t, {TaskReceiverState|recvState&world=world})
	= (Timestamp t, {TaskReceiverState|recvState&world=world})
	
simpleTaskReceiver :: ([AgentTask] -> TaskReceiver [AgentTask])
simpleTaskReceiver = ret


	
		
	

	

	