module eq

import StdGeneric, StdEnv

generic gEq a b :: a b -> Bool
gEq{|Int|} 	x y 							= x == y
gEq{|Char|} x y 							= x == y
gEq{|Bool|} x y 							= x == y
gEq{|Real|} x y 							= x == y
gEq{|UNIT|} UNIT UNIT 						= True
gEq{|PAIR|} fx fy (PAIR x1 y1) (PAIR x2 y2) = fx x1 x2 && fy y1 y2
gEq{|EITHER|} fl fr (LEFT x) (LEFT y) 		= fl x y
gEq{|EITHER|} fl fr (RIGHT x) (RIGHT y) 	= fr x y
gEq{|EITHER|} fl fr 	_ _ 				= False
gEq{|CONS|} f (CONS x) (CONS y) 			= f x y
gEq{|FIELD|} f (FIELD x) (FIELD y) 			= f x y
gEq{|OBJECT|} f (OBJECT x) (OBJECT y)		= f x y

(===) infix 4 :: a a -> Bool | gEq{|*|} a
(===) x y = gEq{|*|} x y

(=!=) infix 4 :: a a -> Bool | gEq{|*|} a
(=!=) x y = not (x === y)

:: Tree a b = Tip a | Bin b (Tree a b) (Tree a b)
:: Rose a = Rose a .[Rose a]
:: Fork a = Fork a a
:: Sequ a = SequEmpty | SequZero .(Sequ .(Fork a)) | SequOne a .(Sequ .(Fork a))

derive gEq 	[], (,), Tree, Rose, Fork, Sequ

Start =
	[ [1,2,3] === [1,2,3]
	, [1,2,3] =!= [1,2,3,4]
	, [1,2,3] =!= [1,2,4] 
	, gEq{|*->*|} (\i r -> toReal i == r) [1,2,3] [1.0, 2.0, 3.0]
	, Bin 1 (Tip 2.0) (Tip 3.0) === Bin 1 (Tip 2.0) (Tip 3.0)
	, Rose 1 [Rose 2 [], Rose 3 []] === Rose 1 [Rose 2 [], Rose 3 []]
	, SequZero (SequOne (Fork 1 2) (SequOne (Fork (Fork 3 4) (Fork 5 6)) SequEmpty)) === SequZero (SequOne (Fork 1 2) (SequOne (Fork (Fork 3 4) (Fork 5 6)) SequEmpty))
 	]

/*
// generated code:

class gEq{|*|} t where
	gEq{|*|} :: t t -> Bool
class gEq{|*->*|} t where
	gEq{|*|} :: (a b -> Bool) (t a) (t b) -> Bool
class gEq{|*->*->*|} t where
	gEq{|*->*->*|} :: (a1 b1 -> Bool) (a2 b2 -> Bool) (t a1 a2) (t b1 b2) -> Bool

instance gEq{|*|} Int where 
	gEq{|*|} x y = x == y
instance gEq{|*|} UNIT where 
	gEq{|*|} UNIT UNIT = True
instance gEq{|*->*->*|} PAIR where 
	gEq{|*->*->*|} eqx eqy (PAIR x1 y1) (PAIR x2 y2) = eqx x1 x2 && eqy y1 y2

instance gEq{|*->*|} [] where
	gEq{|*->*|} eqa xs ys = adaptor (eqListG eqa) xs ys
	where
		adaptor :: ((ListG a) -> (ListG b) -> Bool) -> ([a]->[b] -> Bool) 
		adaprot f = (bimap_gEq isoList isoList).map_from 
		eqListG :: (a -> b -> Bool) -> [a] -> [b] -> Bool
		eqListG eqa = gEqEITHER (gEqPAIR eqa (gEqList eqa)) gEqUNIT 
instance gEq{|*|} [a] | gEq{|*|} a	where
	gEq{|*|} xs ys 	= gEq{|*->*|} gEq{|*|} xs ys 
*/ 