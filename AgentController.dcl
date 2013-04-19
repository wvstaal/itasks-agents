definition module AgentController

import Agent
from Time				import :: Timestamp

:: *TaskProducerState = 
	{ world 	:: *World
	, sessionId :: Maybe SessionId
	, agentId	:: String
	, lastTime	:: Timestamp
	}
	
:: *ActionConsumerState = 
	{ world 	 :: *World
	, sessionId  :: SessionId
	, agentId	 :: String
	, startTime  :: Timestamp 
	, actions	 :: [AgentAction] 
	}
		 
:: TaskProducer    :== *TaskProducerState 	-> *(Maybe [AgentTask], TaskProducerState)
:: ActionConsumer  :== *ActionConsumerState -> *ActionConsumerState

run :: (Agent s) TaskProducer ActionConsumer *World -> *World

//delayTaskProducer 		:: Int -> TaskProducer Timestamp

//simpleTaskProducer 		:: [AgentTask] -> TaskProducer [AgentTask]


