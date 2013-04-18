implementation module Threading

import Threads
import StdEnv

forkAndWait :: [*World -> *World] *World -> *World
forkAndWait fs world
	#(threads, world) = foldl frk ([], world) fs
	= foldr waitForThread world threads
where
	frk (ts, world) f 
		#(tid, world) = fork f world
		=([tid:ts], world)