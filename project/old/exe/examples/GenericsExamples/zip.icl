module zip

import StdGeneric, StdEnv

generic gZip a b c :: .a .b -> .c
gZip{|Int|} x y 								= if (x == y) x (abort "zip Int failed\n")
gZip{|Bool|} x y 								= if (x == y) x (abort "zip Bool failed\n")
gZip{|Char|} x y 								= if (x == y) x (abort "zip Char failed\n")
gZip{|Real|} x y 								= if (x == y) x (abort "zip Real failed\n")
gZip{|UNIT|} UNIT UNIT 							= UNIT
gZip{|PAIR|} fx fy (PAIR x1 y1) (PAIR x2 y2) 	= PAIR (fx x1 x2) (fy y1 y2)
gZip{|EITHER|} fl fr (LEFT x) (LEFT y) 			= LEFT (fl x y) 
gZip{|EITHER|} fl fr (RIGHT x) (RIGHT y) 		= RIGHT (fr x y) 
gZip{|EITHER|} fl fr _ _ 						= abort "gZip EITHER failed\n" 
gZip{|CONS|} f (CONS x) (CONS y) 				= CONS (f x y)
gZip{|FIELD|} f (FIELD x) (FIELD y) 			= FIELD (f x y)
gZip{|OBJECT|} f (OBJECT x) (OBJECT y) 			= OBJECT (f x y)

// zip{|*->*|}

:: Tree a b = Tip a | Bin b (Tree a b) (Tree a b)
:: Rose a = Rose a .[Rose a]
:: Fork a = Fork a a
:: Sequ a = SequEmpty | SequZero .(Sequ .(Fork a)) | SequOne a .(Sequ .(Fork a))

derive gZip [], Tree, Rose, Fork, Sequ

fzip :: u:((.a -> .(.b -> .c)) -> v:(w:(d .a) -> x:(w:(d .b) -> w:(d .c)))) | gZip{|*->*|} (d a) (d b) (d c), [v <= u,x <= v,x <= w]
fzip = gZip{|*->*|}

bfzip :: u:((.a -> .(.b -> .c)) -> v:((.d -> .(.e -> .f)) -> w:(x:(g .a .d) -> y:(x:(g .b .e) -> x:(g .c .f))))) | gZip{|*->*->*|} (g a d) (g b e) (g c f), [v <= u,w <= v,y <= w,y <= x]
bfzip = gZip{|*->*->*|}

Start =
	( fzip (+) [1,2,3] [4,5,6]
	, fzip (+) (Rose 1 [Rose 2 [], Rose 3 []]) (Rose 4 [Rose 5 [], Rose 6 []])
	, fzip (+) (SequZero (SequOne (Fork 1 2) (SequOne (Fork (Fork 3 4) (Fork 5 6)) SequEmpty))) 
		(SequZero (SequOne (Fork 7 8) (SequOne (Fork (Fork 9 10) (Fork 11 12)) SequEmpty)))
	, bfzip (+) (-) (Bin 1 (Tip 2.0) (Tip 3.0)) (Bin 4 (Tip 5.0) (Tip 6.0))
	)