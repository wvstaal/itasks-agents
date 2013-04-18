implementation module AgentCombinators

import iTasks
from Map import qualified put

(<<:) infixl 2 :: (Task a) String -> Task a
(<<:) t id = updateIdAttr id t

(>>:) infixl 2 :: String (Task a) -> Task a
(>>:) id t = updateIdAttr id t

updateIdAttr :: String (Task a) -> Task a
updateIdAttr id (Task t) = Task eval
where
	addElement [(a, JSONObject xs):tl] n v = [(a, JSONObject (xs ++ [(n, toJSON v)])):tl]
	addElement xs n v = xs
	eval event repOpts tt iworld =
		case (t event repOpts tt iworld) of
		r=:(ValueResult tv ti (TaskRep ui serv) tt, world) 
			= (ValueResult tv ti (TaskRep ui {TaskServiceRep|serv& tag = Just id}) tt, world)
		x = x
	