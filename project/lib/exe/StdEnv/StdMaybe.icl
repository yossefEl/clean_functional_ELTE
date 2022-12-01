implementation module StdMaybe

//	********************************************************************************
//	Clean StdLib library module, version 3.0
//	********************************************************************************

from StdOverloaded import class ==(..);
import _SystemStrictMaybes

:: Maybe x :== ?^ x
Just x :== ?^Just x
Nothing :== ?^None

isJust m 	:== m =: ?|Just _
fromJust m	:== let (?|Just x) = m in x
isNothing m	:== m =: ?|None
isNone m	:== m =: ?|None

isJustU :: !u:(m v:a) -> .(!Bool, !u:(m v:a)) | Maybe m a, [u <= v]
isJustU nothing =: ?|None
	= (False, nothing)
isJustU just
	= (True, just)

isNothingU :: !u:(m v:a) -> .(!Bool, !u:(m v:a)) | Maybe m a, [u <= v]
isNothingU nothing =: ?|None
	= (True, nothing)
isNothingU just
	= (False,just)

isNoneU :: !u:(m v:a) -> .(!Bool, !u:(m v:a)) | Maybe m a, [u <= v]
isNoneU none =: ?|None
	= (True, none)
isNoneU just
	= (False,just)

mapMaybe :: .(v:a -> w:b) !u:(m v:a) -> u:(m w:b) | Maybe m a & Maybe m b, [u<=v, u<=w]
mapMaybe f (?|Just x) = ?|Just (f x)
mapMaybe _ _          = ?|None

instance == (?x) | == x
where
	(==) ?None     maybe = maybe =: ?None
	(==) (?Just a) maybe = case maybe of
		?Just b -> a==b
		?None   -> False

instance == (?^x) | == x
where
	(==) ?^None     maybe = maybe =: ?^None
	(==) (?^Just a) maybe = case maybe of
		?^Just b -> a==b
		?^None   -> False

instance == (?#x) | UMaybe x & == x
where
	(==) ?#None     maybe = maybe =: ?#None
	(==) (?#Just a) maybe = case maybe of
		?#Just b -> a==b
		?#None   -> False

maybeToList :: !u:(m v:a) -> .[v:a] | Maybe m a, [u<=v]
maybeToList ?|None     =  []
maybeToList (?|Just a) =  [a]

listToMaybe :: !u:[v:a] -> w:(m v:a) | Maybe m a, [w u <= v]
listToMaybe []         = ?|None
listToMaybe [a:_]      = ?|Just a

catMaybes :: ![u:(m v:a)] -> .[v:a] | Maybe m a, [u<=v]
catMaybes ms           =  [ m \\ ?|Just m <- ms ]
