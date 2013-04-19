implementation module Agent

import TaskRep, Maybe, Monad, StdEnv, JSON
 
instance Monad (Pattern s)
where
	return :: a -> Pattern s a
	return x = Pattern (\_ _. Just x)
	
	(>>=) infixr 5 :: (Pattern s a) (a -> Pattern s b) -> Pattern s b
	(>>=) (Pattern pa) f = Pattern (\s ts. case (pa s ts) of
				  				 			Just a  = let (Pattern pb) = f a in pb s ts
				  				 			Nothing = Nothing)
				  				 			
nil :: Pattern s a
nil = Pattern (\_ _. Nothing)

(-&&-) infixr 5 :: (Pattern s a) (Pattern s b) -> Pattern s (a, b)
(-&&-) pa pb = pa >>= \a. pb >>= \b. return (a,b)

(-||-) infixr 4 :: (Pattern s a) (Pattern s a) -> Pattern s a
(-||-) (Pattern pa) (Pattern pb) = Pattern (\s ts. case (pa s ts) of
										  			Nothing 	= pb s ts
										  			x			= x)
					
(-|-) infixr 3 :: (Pattern s a) (a s -> Bool) -> Pattern s a 
(-|-) pa f = Pattern (\s ts. let (Pattern p) = (pa >>= \a. if (f a s) (return a) nil) in p s ts)

task :: String -> Pattern s AgentTask
task tag = Pattern (\s ts. case (filter (\at. at.AgentTask.tag == tag) ts) of
							[]     = Nothing
							[x:xs] = Just x)
	
(==>) infixr 2 :: (Pattern s a) (a s -> (s, [AgentAction])) -> AgentActivity s
(==>) (Pattern pa) f = Pattern (\s ts. case (pa s ts) of
                    			Just a  = Just (f a s)
                    			Nothing = Nothing)
                   
(<|>) infixl 1 :: (AgentActivity s) (AgentActivity s) -> AgentActivity s
(<|>) aa1 aa2 = aa1 -||- aa2

value :: AgentTask -> a | JSONDecode{|*|} a
value t = (fromJust o fromJSON o fromJust) t.AgentTask.value

edit :: AgentTask a -> AgentAction | JSONEncode{|*|} a
edit t x = EditAction t.AgentTask.taskId (toJSON x)

act :: AgentTask -> AgentAction
act t = ActAction t.AgentTask.taskId t.AgentTask.tag

enabled :: AgentTask -> Bool
enabled t = t.AgentTask.enabled

wait :: Int Int -> AgentAction
wait min max = WaitAction min max

instance toString TaskId
where
	toString (TaskId topNo taskNo)		= toString topNo +++ "-" +++ toString taskNo