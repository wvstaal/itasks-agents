module foobar

import Monad, State, Identity, File

:: Einz = { foo :: World }

:: F a :== State Einz a


test :: Einz (F a) -> Einz
test einz f 
	#(r, world) = runState f einz
	= world
	
test2 :: (F a)
test2 = gets (\w.
			  let (a, world) = readFile "a" world in put world 
	 
Start world = test { foo = world} (return 1)

