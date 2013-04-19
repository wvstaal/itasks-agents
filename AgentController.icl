implementation module AgentController

import Agent
import Time
import StdEnv
import Sleep
import StdDebug
import State

run :: (Agent s) TaskProducer ActionConsumer *World -> *World
run agent=:{initial, activity, id} prod cons world
 	= execute initial {world=world, sessionId=Nothing, lastTime = Timestamp 0, agentId = id}
where
	execute agentState prodState 
		#(mbTasks, prodState) 	= prod prodState
		| isNothing mbTasks   	= prodState.TaskProducerState.world
		#tasks = fromJust mbTasks
		#(Pattern activity) = activity
		#mbActions = activity agentState tasks
		#(t, world) = time prodState.TaskProducerState.world
		| isNothing mbActions = execute agentState {TaskProducerState|prodState & world = world, lastTime = t}
		#(Just (agentState, actions)) = mbActions
		#(t, world) = time prodState.TaskProducerState.world
		#{ActionConsumerState|world} = cons {ActionConsumerState|actions=actions,startTime = t, agentId=id, world=world, sessionId=fromJust prodState.TaskProducerState.sessionId}
		#(t, world) = time world
		= execute agentState {TaskProducerState|prodState & world = world, lastTime = t}
		
delayTaskReceiver :: Int -> TaskProducer
delayTaskReceiver secs = recv
where
	recv recvState=:{world,lastTime}
	#(Timestamp lastTime) = lastTime
	#(Timestamp t, world) = time world
	| t - lastTime < secs
		#world = sleep ((secs - (t - lastTime)) * 1000) world
		= (Nothing, {TaskProducerState|recvState&world=world})
	= (Nothing, {TaskProducerState|recvState&world=world})
	
simpleTaskReceiver :: [AgentTask] -> TaskProducer
simpleTaskReceiver tsk = \s. (Just tsk, s)


	
		
	

	

	