implementation module Token

import JSON
import StdEnv
import Agent, AgentController, WebServiceInterface
import StdDebug
import Void

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
			
mkAgent id target = { id = "foobar", activity = tokenAgentActivity id target, initial = Void }

runAgent :: Int Int *World -> *World
runAgent id target world = run (mkAgent id target) receiver actionHandler world
where
	actionHandler 	= webServiceActionHandler server port
	receiver 		= webServiceTaskReceiver server port
	server 			= "localhost"
	port   			= 80
			

