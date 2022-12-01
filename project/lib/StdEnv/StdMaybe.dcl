definition module StdMaybe

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
	special m= ?; m= ?^
isNothingU :: !u:(m v:a) -> .(!Bool, !u:(m v:a)) | Maybe m a, [u <= v]
	special m= ?; m= ?^
isNoneU :: !u:(m v:a) -> .(!Bool, !u:(m v:a)) | Maybe m a, [u <= v]
	special m= ?; m= ?^

mapMaybe :: .(v:a -> w:b) !u:(m v:a) -> u:(m w:b) | Maybe m a & Maybe m b, [u<=v, u<=w]
	special m= ?; m= ?^
// mapMaybe f (?|Just x) = ?|Just (f x)
// mapMaybe f ?|None     = ?|None

instance == (?x) | == x
instance == (?^x) | == x
instance == (?#x) | UMaybe x & == x

maybeToList :: !u:(m v:a) -> .[v:a] | Maybe m a, [u<=v]
	special m= ?; m= ?^
//	returns list with no or one element

listToMaybe :: !u:[v:a] -> w:(m v:a) | Maybe m a, [w u <= v]
	special m= ?; m= ?^
//	returns Just head of list if possible

catMaybes :: ![u:(m v:a)] -> .[v:a] | Maybe m a, [u<=v]
	special m= ?; m= ?^
//	catMaybes ms =  [ m \\ Just m <- ms ]
