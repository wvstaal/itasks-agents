definition module Date

import StdEnv, JSON
//* (Local) date and time
:: Date	=
	{ day	:: !Int // 1..31
	, mon	:: !Int // 1..12
	, year	:: !Int
	}

:: Time =
	{ hour	:: !Int
	, min	:: !Int
	, sec	:: !Int
	}

:: DateTime = DateTime !Date !Time

//Date addition" righthand argument is treated as interval (days are added first)
//Time addition: righthand argument is treated as interval (seconds are added first)
//Time subtraction: righthand argument is treated as interval (seconds are subtracted first)
instance toString	Date, Time, DateTime
instance fromString	Date, Time, DateTime
instance +			Date, Time, DateTime
instance -			Date, Time, DateTime
instance ==			Date, Time, DateTime
instance <			Date, Time, DateTime

derive JSONEncode Date, Time, DateTime
derive JSONDecode Date, Time, DateTime
