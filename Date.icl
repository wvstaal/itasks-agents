implementation module Date
import StdEnv
import JSON, Maybe, StdString

JSONEncode{|Date|} d		= [JSONString (toString d)]

JSONDecode{|Date|} [JSONString s:c] 	= (Just (fromString s), c)
JSONDecode{|Date|} c					= (Nothing, c)

JSONEncode{|Time|} t					= [JSONString (toString t)]
JSONDecode{|Time|} [JSONString s:c]		= (Just (fromString s), c)
JSONDecode{|Time|} c					= (Nothing, c)

instance toString Time
where
	toString {Time|hour,min,sec}	= (pad 2 hour) +++ ":" +++ (pad 2 min) +++ ":" +++ (pad 2 sec)

instance fromString Time
where
	fromString s					= {Time|hour = toInt (s %(0,1)), min = toInt (s %(3,4)), sec = toInt (s %(6,7)) }

instance + Time // Second time is treated as an interval
where
	(+) x y = normHours (addHours y.Time.hour (normMinutes (addMinutes y.Time.min (normSeconds (addSeconds y.Time.sec x)))))
	where
		addSeconds s t	= {Time|t & sec = t.Time.sec + s}
		normSeconds t	= {Time|t & min = t.Time.min + (t.Time.sec / 60), sec = t.Time.sec rem 60}
		addMinutes m t	= {Time|t & min = t.Time.min + m}
		normMinutes t	= {Time|t & hour = t.Time.hour + (t.Time.min / 60), min = t.Time.min rem 60}
		addHours h t	= {Time|t & hour = t.Time.hour + h}
		normHours t		= {Time|t & hour = t.Time.hour rem 24}
		
instance - Time
where
	(-) x y = normHours (subHours y.Time.hour (normMinutes (subMinutes y.Time.min (normSeconds (subSeconds y.Time.sec x)))))
	where
		subSeconds s t	= {t & sec = t.sec - s}
		normSeconds t
			# ns = t.Time.sec rem 60
			| ns < 0	= {Time|t & min = t.Time.min + (t.Time.sec / 60) - 1, sec = ns + 60}
						= {Time|t & min = t.Time.min + (t.Time.sec / 60), sec = ns}
		subMinutes m t	= {Time|t & min = t.Time.min - m}
		normMinutes t	
			# nm = t.Time.min rem 60
			| nm < 0	= {Time|t & hour = t.Time.hour + (t.Time.min / 60) - 1, min = nm + 60}
						= {Time|t & hour = t.Time.hour + (t.Time.min / 60), min = nm}
		subHours h t	= {Time|t & hour = t.Time.hour - h}
		normHours t	
			# nh = t.Time.hour rem 24
			| nh < 0	= {Time|t & hour = nh + 24}
						= {Time|t & hour = nh}
		
instance == Time
where
	(==) x y = x.Time.hour == y.Time.hour && x.Time.min == y.Time.min && x.Time.sec == y.Time.sec
	
instance < Time
where
	(<) x y
		| x.Time.hour < y.Time.hour															= True
		| x.Time.hour == y.Time.hour && x.Time.min < y.Time.min								= True
		| x.Time.hour == y.Time.hour && x.Time.min == y.Time.min && x.Time.sec < y.Time.sec	= True
		| otherwise																			= False

JSONEncode{|DateTime|} dt	= [JSONString (toString dt)]

JSONDecode{|DateTime|} [JSONString s:c]	= (Just (fromString s), c)
JSONDecode{|DateTime|} c				= (Nothing, c)


instance toString DateTime
where
	toString (DateTime d t) = toString d +++ " " +++ toString t

instance fromString DateTime
where
	fromString s	= DateTime
						{Date|day = toInt (s %(8,9)), mon = toInt (s %(5,6)), year = toInt (s %(0,3))}
						{Time|hour = toInt (s %(11,12)), min = toInt (s %(14,15)), sec = toInt (s %(17,18)) }

instance + DateTime
where
	(+) (DateTime dx tx) (DateTime dy ty)
			| tn >= tx	= DateTime dn tn
			| otherwise	= DateTime (dn + {year = 0, mon = 0, day = 1}) tn	//We've passed midnight
	where
		dn = dx + dy
		tn = tx + ty
		
instance - DateTime
where
	(-) (DateTime dx tx) (DateTime dy ty) = DateTime (dx - dy) (tx - ty)

instance == DateTime
where
	(==) (DateTime dx tx) (DateTime dy ty)	= dx == dy && tx == ty

instance < DateTime
where
	(<) (DateTime dx tx) (DateTime dy ty)
		| dx < dy	= True
		| dx == dy	= (tx < ty)
		| otherwise	= False
		
instance toString Date
where
	toString {Date|year,mon,day}	= (pad 4 year) +++ "-" +++ (pad 2 mon) +++ "-" +++ (pad 2 day)

instance fromString Date
where
	fromString s					= {Date|day = toInt (s %(8,9)), mon = toInt (s %(5,6)), year = toInt (s %(0,3))}

instance + Date //Second date is treated as an interval to be added
where
	(+) x y = normDays (addYears y.Date.year (normMonths (addMonths y.Date.mon (normDays (addDays y.Date.day x)))))
		// last normDays to remove 29-2 in non leap year
	where
		addDays days date		= {Date|date & day = date.day + days}
		addMonths months date	= {Date|date & mon = date.mon + months}
		addYears years date		= {Date|date & year = date.year + years}

		normDays date
			# monthLength = monthLengthOfDate date
			| date.day <= monthLength
				= date
				= normDays (normMonths {date & mon = date.mon + 1, day = date.day - monthLength})

		normMonths date
			| date.mon <= 12
				= date
				= normMonths {date & year = date.year + 1, mon = date.mon - 12}

		monthLengthOfDate date=:{mon}
			| mon==2
				| isLeapYear date.year
					= 29
					= 28
			| mon==4 || mon==6 || mon==9 || mon==11
				= 30
				= 31

		isLeapYear year
			| year rem 4<>0
				= False
				= year rem 100 <> 0 || year rem 400 == 0

instance - Date
where
	(-) x y = {Date|year = x.Date.year - y.Date.year, mon = x.Date.mon - y.Date.mon, day = x.Date.day - y.Date.day}

instance == Date
where
	(==) x y	= x.Date.year == y.Date.year && x.Date.mon == y.Date.mon && x.Date.day == y.Date.day
		
instance < Date
where
	(<) x y 
		| x.Date.year < y.Date.year															= True
		| x.Date.year == y.Date.year && x.Date.mon < y.Date.mon								= True
		| x.Date.year == y.Date.year && x.Date.mon == y.Date.mon && x.Date.day < y.Date.day	= True
		| otherwise		
		
pad :: !Int !Int -> String
pad len num
	# nums = toString num
	| size nums >= len
		= nums
		= createArray (len - size nums) '0' +++ nums		
