definition module Agent

import SystemTypes, Task, Monad, JSON

:: Agent s = 
	{ id       :: String
	, activity :: AgentActivity s
	, initial  :: s
	}
		    
:: AgentActivity s :== s [AgentTask] -> Maybe (s, [AgentAction])

:: AgentTask = 
	{ taskId :: TaskId
	, tag    :: String
	, value  :: JSONNode
	, repValue :: JSONNode
	, enabled :: Bool
	}

:: AgentAction = EditAction TaskId JSONNode | ActAction TaskId String

:: Pattern s a = Pattern (s [AgentTask] -> Maybe a)
 
instance Monad (Pattern s)
				  				 			
nil :: Pattern s a

(-&&-) infixr 5 :: (Pattern s a) (Pattern s b) -> Pattern s (a, b)

(-||-) infixr 4 :: (Pattern s a) (Pattern s a) -> Pattern s a
					
(-|-) infixr 3 :: (Pattern s a) (a s -> Bool) -> Pattern s a 

task :: String -> Pattern s AgentTask
	
(==>) infixr 2 :: (Pattern s a) (a s -> (s, [AgentAction])) -> AgentActivity s
                   
(<|>) infixr 1 :: (AgentActivity s) (AgentActivity s) -> AgentActivity s

value :: AgentTask -> a | JSONDecode{|*|} a

edit :: AgentTask a -> AgentAction | JSONEncode{|*|} a

act :: AgentTask -> AgentAction