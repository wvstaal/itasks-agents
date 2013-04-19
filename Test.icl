module Test
import Maybe
import HttpClient
import TaskRep, Monad
from Map import qualified :: Map
import Agent, AgentController
import WebServiceInterface
import Time
import Token
import ArgEnv
import Void
from Process			import qualified ::ProcessHandle, runProcess, checkProcess,callProcess
from Process			import :: ProcessHandle(..)
import Date

:: CoordinatorState = { favoriteDate :: Date }

coordinator :: Agent CoordinatorState
coordinator = { id = "Coordinator", activity = coordinatorActivity, initial = { favoriteDate = {Date|day=1,mon=1,year=2013} } }

coordinatorActivity :: AgentActivity CoordinatorState
coordinatorActivity = enterInitialDates <|> makeDecision	  
where
	enterInitialDates = 
			  task "enterDates"
			  -&&-
			  task "enterDatesContinue"
			  ==> (\(enterDates, continueButton) s. 
			  		(s, [
			  			  edit enterDates initialDates
			  		    , act continueButton
			  		    ]))
	makeDecision = 
			task "makeDecision"
			-&&-
			task "monitorDateTimes"
			-|- (\(_, monitor) s. any ((==)s.favoriteDate) (value monitor))
			==> \(decide, _) s. (s, [act decide])
	initialDates = [{Date|day=1, mon=1, year=2013}, {Date|day=2, mon=1, year=2013}]
			  
chooser1 :: Agent Void
chooser1 = { id = "DateChooser1", activity = dateChooserActivity {Date|day=1, mon=1, year=2013}, initial = Void }

chooser2 :: Agent Void
chooser2 =  { id = "DateChooser1", activity = dateChooserActivity {Date|day=2, mon=1, year=2013}, initial = Void }

dateChooserActivity :: Date -> AgentActivity Void
dateChooserActivity dt = task "chooseDate"
				 		-&&-
				 		task "chooseDateDone"
				 		==> \(chooseDate,done) s. (s, [edit chooseDate dt, act done])

:: Test = E. a: Test a

testAgent = { id = "testAgent", activity = testAgentActivity, initial = Void }
			
testAgentActivity :: AgentActivity Void
testAgentActivity = (task "enterString1"
					-&&-
					task "done"
					==> \(enterString1, done) s. (s, [edit enterString1 "foobar1", act done]) )
					<|>
					(task "enterString2"
					-&&-
					task "done"
					==> \(enterString1, done) s. (s, [edit enterString1 "foobar2", act done])) 

Start world 
	#cmd = [x \\x <-: getCommandLine]
	| length cmd == 3 = runAgent (toInt (cmd!!1)) (toInt (cmd!!2)) world
	= foldl (spawn (cmd!!0)) world (zip2 [1..10] ([2..10]++[1]))
where
	spawn app world (id,target) 
	#(_, world) = 'Process'.runProcess app [toString id, toString target] Nothing world
	= world
	
	
//Start :: *World -> *World
//Start world = startEngine [publish "/" WebApp (\_-> mainTask)] world