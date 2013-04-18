definition module WebServiceInterface

import AgentController, Agent

webServiceTaskReceiver 	:: String Int -> TaskReceiver [AgentTask]
webServiceActionHandler :: String Int -> ActionHandler Bool