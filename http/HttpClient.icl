implementation module HttpClient

import List, StdEnv, Map, Parsers, TCPIP, StdMaybe

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
	#(tReport, mbBs, rc, world)	= receive_MT (Just (20*ticksPerSecond)) rc world
	#world						= closeRChannel rc world
	#world						= closeChannel sc world
	= case tReport of
		TR_Success 	= (Just (toString (fromJust mbBs)), world)
		_			= (Nothing, world)
		
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
parseResponse s = case (parse parser (fromString s) "line" "character") of
				  (Succ [r:rs])	= Just r
				  (Err symbolTypes hypotheses position)
							= Nothing
where
	parser = (tokenH ['HTTP/1.'] &> digit &> ws &> number <& ws <& skipTo endl <& endl)
				  <&> \nr. 
				  		(<!*> ((headerText <& symbol ':' <& ws) <&> \k. headerText <& skipTo endl <& endl <@ \v. (toString k, toString v)))
				  		<&> \hdrs. ws &> (<!*> anySymbol)
				  				   <@ \data. {rsp_headers = fromList hdrs, rsp_responseCode = nr, rsp_data = toString data}
	ws 		= (<!*> (satisfy isSpace))
	endl 	= tokenH ['\r\n']
	headerText = (<!*> (satisfy (\c. c <> ':' && c <> '\r')))
	defaultResponse = { rsp_headers = newMap, rsp_responseCode = 0, rsp_data = "" }