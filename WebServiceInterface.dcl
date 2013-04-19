definition module WebServiceInterface

import AgentController, Agent

webServiceTaskReceiver 	:: String Int -> TaskProducer
webServiceActionHandler :: String Int -> ActionConsumer