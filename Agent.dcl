definition module Agent

import Monad, JSON

:: TaskId		= TaskId !InstanceNo !TaskNo
:: InstanceNo	:== Int
:: TaskNo		:== Int
:: SessionId	:== String

:: Agent s = 
	{ id       :: String
	, activity :: AgentActivity s
	, initial  :: s
	}
		
:: Pattern s a = Pattern (s [AgentTask] -> Maybe a)
    
:: AgentActivity s :== Pattern s (s, [AgentAction])
 
:: AgentTask =  
	{ taskId 	:: TaskId  
	, tag    	:: String
	, value  	:: Maybe JSONNode
	, repValue 	:: Maybe JSONNode
	, enabled 	:: Bool
	}

:: AgentAction = EditAction TaskId JSONNode | ActAction TaskId String | WaitAction Int Int
 
instance toString	TaskId

instance Monad (Pattern s)
				  				 			
nil :: Pattern s a

(-&&-) infixr 5 :: (Pattern s a) (Pattern s b) -> Pattern s (a, b)

(-||-) infixr 4 :: (Pattern s a) (Pattern s a) -> Pattern s a
					
(-|-) infixr 3 :: (Pattern s a) (a s -> Bool) -> Pattern s a 

task :: String -> Pattern s AgentTask
	
(==>) infixr 2 :: (Pattern s a) (a s -> (s, [AgentAction])) -> AgentActivity s
                   
(<|>) infixl 1 :: (AgentActivity s) (AgentActivity s) -> AgentActivity s

value :: AgentTask -> a | JSONDecode{|*|} a

edit :: AgentTask a -> AgentAction | JSONEncode{|*|} a

enabled :: AgentTask -> Bool

act :: AgentTask -> AgentAction

wait :: Int Int -> AgentAction