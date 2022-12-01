module value

import StdGeneric, StdEnv

:: Value
	= VInt Int
	| VReal Real 
	| VUnit
	| VPair Value Value
	| VLeft Value
	| VRight Value
	| VFun (Value -> Value)
	| VCons Value
	| VField Value
	| VObject Value

:: Bimap a b = { map_to :: .(a -> b), map_from :: .(b -> a) }

generic gConvertValue a :: Bimap a Value
gConvertValue {|Int|} = {map_to = map_to, map_from = map_from }
where
	map_to x = VInt x
	map_from (VInt x) = x
	map_from _ = abort "gConvertValue failed on Int"
gConvertValue {|Real|} = {map_to = map_to, map_from = map_from }
where
	map_to x = VReal x
	map_from (VReal x) = x
	map_from _ = abort "gConvertValue failed on Real"
gConvertValue {|UNIT|} = {map_to = map_to, map_from = map_from }
where
	map_to UNIT = VUnit
	map_from VUnit = UNIT
	map_from _ = abort "gConvertValue failed on UNIT"
	
gConvertValue {|PAIR|} cx cy = {map_to = map_to, map_from = map_from }
where
	map_to (PAIR x y) = VPair (cx.map_to x) (cy.map_to y)
	map_from (VPair x y) = PAIR (cx.map_from x) (cy.map_from y)
	map_from _ = abort "gConvertValue failed on PAIR"

gConvertValue {|EITHER|} cl cr = {map_to = map_to, map_from = map_from }
where
	map_to (LEFT x) = VLeft (cl.map_to x)
	map_to (RIGHT x) = VRight (cr.map_to x)
	map_from (VLeft x) = LEFT (cl.map_from x)
	map_from (VRight x) = RIGHT (cr.map_from x)
	map_from _ = abort "gConvertValue failed on EITHER"

gConvertValue {|(->)|} ca cr = {map_to = map_to, map_from = map_from }
where
	map_to f = VFun (cr.map_to o f o ca.map_from)
	map_from (VFun f) = (cr.map_from o f o ca.map_to)
	map_from _ = abort "gConvertValue failed on (->)"

gConvertValue {|CONS|} ca = {map_to = map_to, map_from = map_from }
where
	map_to (CONS x) = VCons (ca.map_to x)
	map_from (VCons x) = CONS (ca.map_from x)
	map_from _ = abort "gConvertValue failed on CONS"

gConvertValue {|FIELD|} ca = {map_to = map_to, map_from = map_from }
where
	map_to (FIELD x) = VField (ca.map_to x)
	map_from (VField x) = FIELD (ca.map_from x)
	map_from _ = abort "gConvertValue failed on FIELD"
gConvertValue {|OBJECT|} ca = {map_to = map_to, map_from = map_from }
where
	map_to (OBJECT x) = VObject (ca.map_to x)
	map_from (VObject x) = OBJECT (ca.map_from x)
	map_from _ = abort "gConvertValue failed on OBJECT"

convertTo :: (a -> Value) | gConvertValue{|*|} a
convertTo = gConvertValue{|*|}.map_to

convertFrom :: (Value -> a) | gConvertValue{|*|} a
convertFrom = gConvertValue{|*|}.map_from

//--------------------------------------------------------------------------------

applyValue :: Value Value -> Value
applyValue (VFun f) x = f x
applyValue _ x = abort "applyValue: not a function\n"

Start :: Int
Start = convertFrom (applyValue (convertTo ((+) 1)) (convertTo 1))
