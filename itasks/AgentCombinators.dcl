definition module AgentCombinators

import iTasks

(<<:) infixl 2 :: (Task a) String -> Task a

(>>:) infixl 2 :: String (Task a) -> Task a