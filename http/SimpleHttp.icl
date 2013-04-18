implementation module SimpleHttp

import List, StdEnv, Map, Parsers

instance toString SimpleHTTPRequest
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
				 

parseResponse :: String -> Maybe SimpleHTTPResponse
parseResponse s = case (parse parser (fromString s) "line" "character") of
				  (Succ [r:rs])	= Just r
				  (Err symbolTypes hypotheses position)
							= Nothing
where
	parser = (tokenH ['HTTP/1.'] &> digit &> ws &> number <& ws <& skipTo endl <& endl)
				  <&> \nr. 
				  		(<*> ((headerText <& symbol ':' <& ws) <&> \k. headerText <& endl <@ \v. (toString k, toString v)))
				  		<&> \hdrs. (<!*> anySymbol)
				  				   <@ \data. {rsp_headers = fromList hdrs, rsp_responseCode = nr, rsp_data = toString data}
	ws 		= (<!*> (satisfy isSpace))
	endl 	= tokenH ['\r\n']
	headerText = (<!*> (satisfy (\c. c <> ':' && c <> '\r')))
	defaultResponse = { rsp_headers = newMap, rsp_responseCode = 0, rsp_data = "" }
	
	