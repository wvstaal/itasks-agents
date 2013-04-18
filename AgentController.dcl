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
	
:: *ActionHandlerState = 
	{ world 	 :: *World
	, sessionId  :: SessionId
	, agentId	 :: String
	, startTime  :: Timestamp 
	}
		
:: TaskReceiver   a = TaskReceiver  (*TaskReceiverState -> *(a, TaskReceiverState))
:: ActionHandler  a = ActionHandler ([AgentAction] *ActionHandlerState -> *(a, ActionHandlerState))

instance Monad TaskReceiver
instance Monad ActionHandler

run :: (Agent s) (TaskReceiver (Maybe [AgentTask])) (ActionHandler a) *World -> *World

delayTaskReceiver 		:: Int -> TaskReceiver Timestamp

//simpleTaskReceiver 		:: [AgentTask] -> TaskReceiver [AgentTask]


