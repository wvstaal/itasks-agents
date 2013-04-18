definition module WebServiceInterface

import AgentController, Agent

webServiceTaskReceiver 	:: String Int -> TaskReceiver (Maybe [AgentTask])
webServiceActionHandler :: String Int -> ActionHandler Bool