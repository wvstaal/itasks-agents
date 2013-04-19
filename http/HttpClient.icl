implementation module HttpClient

import List, StdEnv, Map, ParserCombinators, TCPIP, Maybe, StdDebug, Text

ticksPerSecond = 10000000

getResponse :: String Int d *World -> (Maybe String, *World) | toString d 
getResponse server port req world 
	#	(addr, world)			= lookupIPAddress server world
	|	isNothing addr
		= (Nothing, world)
	#	(result, mbDuplexChan, world)
									= connectTCP_MT (Just (15*ticksPerSecond))
									                (fromJust addr, port) world
	|	result<>TR_Success
		= (Nothing, world)
	#{ sChannel=sc, rChannel=rc } = fromJust mbDuplexChan
	#(sc, world)				= send (toByteSeq (toString req)) sc world
	#(tReport,data,rc,world)	= recv rc "" world
	#world						= closeRChannel rc world
	#world						= closeChannel sc world
	= case tReport of
		TR_Success 	= (Just (toString data), world)
		_			= (Nothing, world)
where
	finished data  
	#firstIndex = indexOf "Content-Length:" data
	| firstIndex == -1 = False
	#secondIndex = indexOfAfter firstIndex "\r\n" data
	| secondIndex == -1 = False
	#length = ((toInt o trim) (data % (firstIndex+16, secondIndex)))
	#beginDataIndex = indexOf "\r\n\r\n" data
	| beginDataIndex == -1 = False
	= (textSize data - beginDataIndex - 4 == length)
	
	recv rc data world 
	#(tReport, mbBs, rc, world)	= receive_MT (Just (20*ticksPerSecond)) rc world
	| finished (data+++toString (fromJust mbBs)) = (tReport,data +++ toString (fromJust mbBs) , rc, world) 
	= recv rc (data+++toString (fromJust mbBs)) world
	
	
executeHttpRequest :: String Int SimpleHttpRequest *World -> (Maybe SimpleHttpResponse, *World)
executeHttpRequest server port req world
	#(mbResp, world) 	= getResponse server port req world
	| isNothing mbResp  = (Nothing, world)
	#resp = parseResponse (fromJust mbResp)
	= (resp, world)

instance toString SimpleHttpRequest
where
	toString req =  req.req_method +++ " " +++ req.req_path +++ " HTTP/1.0\r\n"
				+++ cookie
				+++ headers
				+++ "Content-Length: " +++ toString (size req.req_data) +++ "\r\n\r\n"
				+++ req.req_data
	where
		headers = foldl (+++) "" [k +++ ": " +++ v +++ "\r\n" \\(k, v) <- toList req.req_headers]
		cookie = case (toList req.arg_cookies) of
				 [] = ""
				 xs = "Cookie: " +++ (foldl (+++) "" (intersperse "; " [k +++ "=" +++ v \\(k, v) <- xs])) +++ "\r\n"
				 

parseResponse :: String -> Maybe SimpleHttpResponse
parseResponse s = case (begin1 parser (fromString s)) of
				  [(_, r):_]	= Just r
				  []			= Nothing
where
	parser = (token ['HTTP/1.'] &> digit &> ws &> nat <& ws <& skipTo '\r' <& endl)
				  <&> \nr. 
				  		(<*?> ((headerText <& symbol ':' <& ws) <&> \k. headerText <& skipTo '\r' <& endl <@ \v. (toString k, toString v)))
				  		<&> \hdrs. ws &> (<+?> (satisfy (\_. True)))  <@ \data. {rsp_headers = fromList hdrs, rsp_responseCode = nr, rsp_data = toString data}
	ws 		= (<+?> (satisfy isSpace))
	skipTo t = (<*?> (satisfy ((<>)t)))
	endl 	= token ['\r\n']
	headerText = (<*?> (satisfy (\c. c <> ':' && c <> '\r')))
	defaultResponse = { rsp_headers = newMap, rsp_responseCode = 0, rsp_data = "" }