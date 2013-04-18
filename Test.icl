module Test
import StdMaybe, StdEnv
import HttpClient
import TaskRep, Monad
import Map, Agent, AgentController

import SystemTypes

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

Start world = [Test 1, Test "fop"]

response = "HTTP/1.1 200 OK\r\nFoobar: Hax\r\n\r\ntestttt"
//Start :: *World -> *World
//Start world = startEngine [publish "/" WebApp (\_-> mainTask)] world