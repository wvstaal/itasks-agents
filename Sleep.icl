implementation module Sleep

//FIXME: platform dependence
from _WinDef  import :: DWORD
from _WinBase import qualified sleep //ouch, we require sleep.

sleep :: Int *World -> *World
sleep msecs world = '_WinBase'.sleep msecs world