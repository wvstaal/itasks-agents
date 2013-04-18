implementation module Token
import Threading

import JSON
import StdEnv
import Agent, AgentController, WebServiceInterface

tokenAgentActivity :: Int Int -> AgentActivity Void
tokenAgentActivity id target = login <|> passToken <|> enterTarget
where
	login 
		=	task "agentId"
		 	-&&-
			task "login"
			==> \(agentId, login) s. (s, [edit agentId id, act login])
	passToken
		= 	task "Pass token"
			-|- (\passToken _. enabled passToken)
			==> \passToken s. (s, [act passToken])
	enterTarget
		= 	task "inputReceiver"
			-&&-
			task "done"
			==> \(inputReceiver, done) s. (s, [edit inputReceiver target, act done])
			
mkAgent :: Int Int -> Agent Void
mkAgent id target = { id = "tokenAgent", activity = tokenAgentActivity id target, initial = Void }

agents :: [Agent Void]
agents = map (\(n1, n2). mkAgent n1 n2) (zip2 [1..10] ([2..10]++[1]))

runAgents :: *World -> *World
runAgents world = forkAndWait (map (\a. run a receiver actionHandler) agents) world
where
	actionHandler 	= webServiceActionHandler server port
	receiver 		= delayTaskReceiver 5 >>>| webServiceTaskReceiver server port
	server 			= "localhost"
	port   			= 80
			

