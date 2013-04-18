module Test
import StdMaybe, StdEnv
import HttpClient
import TaskRep, Monad
import Map, Agent, AgentController
import WebServiceInterface
import SystemTypes
import Time
import Token

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
chooser2 = { id = "DateChooser1", activity = dateChooserActivity {Date|day=2, mon=1, year=2013}, initial = Void }

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
					task "enterString2"
					-&&-
					task "done"
					==> \(enterString1, done) s. (s, [edit enterString1 "foobar2", act done]) 

Start world = runAgents
	
	
//Start :: *World -> *World
//Start world = startEngine [publish "/" WebApp (\_-> mainTask)] world