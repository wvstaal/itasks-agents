definition module SimpleHttp

from Map import :: Map
import StdMaybe

:: SimpleHTTPRequest	= {	req_method		:: 	String					// The HTTP request method (eg. GET, POST, HEAD)
							,	req_path		::	String					// The requested location (eg. /foo)
							,	req_headers		::	Map String String		// The headers sent with the request parsed into name/value pairs
							,	req_data		::	String					// The raw data of the request (without the headers)
							,	arg_cookies		::	Map String String		// The cookies in the set-cookie header
						  }
				
:: SimpleHTTPResponse	= {	
				rsp_headers		::	Map String String		// Extra return headers that should be sent (eg. ("Content-Type","text/plain"))
				, rsp_responseCode :: Int
				,	rsp_data		::	String					// The body of the response. (eg. html code or file data)
				}
				
				
instance toString SimpleHTTPRequest

parseResponse :: String -> Maybe SimpleHTTPResponse