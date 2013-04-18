implementation module Task

import StdClass, StdArray, StdTuple, StdInt, StdList, StdFunc, StdBool, StdMisc, HTML, SystemTypes, GenRecord, HTTP, Map, Util
import GenVisualize, iTaskClass, IWorld
from TaskState			import :: TaskTree(..), :: DeferredJSON(..), :: TIMeta(..)
from LayoutCombinators	import :: Layout(..), autoLayout
from iTasks				import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode

derive gDefault TaskServiceRep

mkInstantTask :: (TaskId *IWorld -> (!MaybeError (Dynamic,String) a,!*IWorld)) -> Task a | iTask a
mkInstantTask iworldfun = Task (evalOnce iworldfun)
where
	evalOnce f _ repOpts (TCInit taskId ts) iworld = case f taskId iworld of	
		(Ok a,iworld)							= (ValueResult (Value a True) {lastEvent=ts} (finalizeRep repOpts rep) (TCStable taskId ts (DeferredJSON a)), iworld)
		(Error (e,s), iworld)					= (ExceptionResult e s, iworld)

	evalOnce f _ repOpts state=:(TCStable taskId ts enc) iworld = case fromJSONOfDeferredJSON enc of
		Just a	= (ValueResult (Value a True) {lastEvent=ts} (finalizeRep repOpts rep) state, iworld)
		Nothing	= (exception "Corrupt task result", iworld)

	evalOnce f _ _ (TCDestroy _) iworld	= (DestroyedResult,iworld)

	rep = TaskRep (UIControlGroup {UIControlGroup|attributes=newMap,controls=[],direction=Vertical,actions=[]}) defaultValue

fromJSONOfDeferredJSON :: !DeferredJSON -> Maybe a | TC a & JSONDecode{|*|} a
fromJSONOfDeferredJSON (DeferredJSON v)
	= case make_dynamic v of
		(v :: a^)
			-> Just v
fromJSONOfDeferredJSON (DeferredJSONNode json)
	= fromJSON json

make_dynamic v = dynamic v

derive gGetRecordFields	TaskValue
derive gPutRecordFields	TaskValue

JSONEncode{|Task|} _ tt = [dynamicJSONEncode tt]		
JSONDecode{|Task|} _ [tt:c] = (dynamicJSONDecode tt,c)
JSONDecode{|Task|} _ c = (Nothing,c)

derive JSONDecode TaskServiceRep
	
gUpdate{|Task|} _ _ target upd val = basicUpdate (\Void t -> t) target upd val

gVerify{|Task|} _ _ um _ = alwaysValid um

gVisualizeText{|Task|} _ _ _ = ["<Task>"]
gVisualizeEditor{|Task|} _ _ _ _ _ vst = (NormalEditor [(stringDisplay "<Task>",newMap)],vst)

gHeaders{|Task|} _ _ = ["Task"]
gGridRows{|Task|} _ _ _ _	= Nothing	
gEq{|Task|} _ _ _			= True // tasks are always equal??

gDefault{|Task|} gDefx _ = Task (\_ -> abort error)
where
	error = "Creating default task functions is impossible"
	
gGetRecordFields{|Task|} _ _ _ fields = fields
gPutRecordFields{|Task|} _ t _ fields = (t,fields)

exception :: !e -> TaskResult a | TC, toString e
exception e = ExceptionResult (dynamic e) (toString e)

repLayout :: !TaskRepOpts -> Layout
repLayout {TaskRepOpts|useLayout,modLayout}	= (fromMaybe id modLayout) (fromMaybe autoLayout useLayout)

afterLayout :: !TaskRepOpts -> (UIDef -> UIDef)
afterLayout {TaskRepOpts|afterLayout} = fromMaybe id afterLayout

finalizeRep :: !TaskRepOpts !TaskRep -> TaskRep
finalizeRep repOpts=:{TaskRepOpts|appFinalLayout=True} rep=:(TaskRep def parts) = TaskRep (UIFinal ((repLayout repOpts).Layout.final def)) parts
finalizeRep repOpts rep = rep

