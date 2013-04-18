implementation module AgentController

import Agent
import Time
import StdEnv

//FIXME: platform dependence
from _WinDef  import :: DWORD
from _WinBase import sleep //ouch, we require sleep.

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
	
run :: (Agent s) (TaskReceiver [AgentTask]) (ActionHandler a) *World -> *World
run agent=:{initial, activity, id} (TaskReceiver recv) (ActionHandler act) world
 	= execute initial {world=world, sessionId=Nothing, lastTime = Timestamp 0, agentId = id}
where
	execute agentState recvState 
		#(tasks, recvState) 	= recv recvState
		| isEmpty tasks   	= recvState.TaskReceiverState.world
		#mbActions = activity agentState tasks 
		| isNothing mbActions = execute agentState recvState
		#(Just (agentState, actions)) = mbActions
		#(_, {TaskSenderState|world}) = act actions {TaskSenderState|agentId=id, world=recvState.TaskReceiverState.world, sessionId=fromJust recvState.TaskReceiverState.sessionId}
		= execute agentState {TaskReceiverState|recvState & world = world}
		
delayTaskReceiver :: Int -> TaskReceiver Timestamp
delayTaskReceiver secs = TaskReceiver recv
where
	recv recvState=:{world,lastTime}
	#(Timestamp lastTime) = lastTime
	#(Timestamp t, world) = time world
	| t - lastTime < secs
		#world = sleep (t - lastTime) world
		= (Timestamp t, {TaskReceiverState|recvState&world=world})
	= (Timestamp t, {TaskReceiverState|recvState&world=world})
	
simpleTaskReceiver :: ([AgentTask] -> TaskReceiver [AgentTask])
simpleTaskReceiver = ret


	
		
	

	

	