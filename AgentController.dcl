definition module AgentController

import Agent
import Task
import Monad
from Time				import :: Timestamp

:: *TaskReceiverState = 
	{ world 	:: *World
	, sessionId :: Maybe SessionId
	, agentId	:: String
	, lastTime	:: Timestamp
	}
	
:: *TaskSenderState = 
	{ world 	:: *World
	, sessionId :: SessionId
	, agentId	:: String
	}
		
:: TaskReceiver   a = TaskReceiver  (*TaskReceiverState -> *(a, *TaskReceiverState))
:: ActionHandler  a = ActionHandler ([AgentAction] *TaskSenderState -> *(a, *TaskSenderState))

instance Monad TaskReceiver
instance Monad ActionHandler

run :: (Agent s) (TaskReceiver [AgentTask]) (ActionHandler a) *World -> *World

delayTaskReceiver 		:: Int -> TaskReceiver Timestamp

//simpleTaskReceiver 		:: [AgentTask] -> TaskReceiver [AgentTask]


