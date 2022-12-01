module mapSt 

import StdGeneric, StdEnv

// mapStList :: (a -> st -> (b, st)) [a] st -> ([b], st)

generic gMapRSt a b :: .a .st -> (.b, .st)
gMapRSt{|c|} x st 						= (x, st)
gMapRSt{|UNIT|} x st					= (x, st)
gMapRSt{|PAIR|} fx fy (PAIR x y) st 	=	
	let 
		(y1, st1) = fy y st
		(x1, st2) = fx x st1
	in
	 	(PAIR x1 y1, st2)	
gMapRSt{|EITHER|} fl fr (LEFT x) st 	= let (x1, st1) = fl x st in (LEFT x1, st1)
gMapRSt{|EITHER|} fl fr (RIGHT x) st 	= let (x1, st1) = fr x st in (RIGHT x1, st1)
gMapRSt{|CONS|} f (CONS x) st 			= let (x1, st1) = f x st in (CONS x1, st1)
gMapRSt{|FIELD|} f (FIELD x) st 		= let (x1, st1) = f x st in (FIELD x1, st1)
gMapRSt{|OBJECT|} f (OBJECT x) st 		= let (x1, st1) = f x st in (OBJECT x1, st1)

generic gMapLSt a b :: .a .st -> (.b, .st)
gMapLSt{|c|} x st 						= (x, st)
gMapLSt{|UNIT|} x st 					= (x, st)
gMapLSt{|PAIR|} fx fy 	(PAIR x y) st	= 
	let 
		(x1, st1) = fx x st
		(y1, st2) = fy y st1
	in
	 	(PAIR x1 y1, st2)	
gMapLSt{|EITHER|} fl fr (LEFT x) st 	= let (x1, st1) = fl x st in (LEFT x1, st1)
gMapLSt{|EITHER|} fl fr (RIGHT x) st	= let (x1, st1) = fr x st in (RIGHT x1, st1)
gMapLSt{|CONS|} f (CONS x) st 			= let (x1, st1) = f x st in (CONS x1, st1)
gMapLSt{|FIELD|} f (FIELD x) st 		= let (x1, st1) = f x st in (FIELD x1, st1)
gMapLSt{|OBJECT|} f (OBJECT x) st 		= let (x1, st1) = f x st in (OBJECT x1, st1)
 
fmapLSt :: u:((.a -> .(.st -> (.b,.st))) -> v:(w:(t .a) -> x:(.st -> (w:(t .b),.st)))) | gMapLSt{|*->*|} (t a) (t b), [v <= u,x <= v,x <= w]
fmapLSt = gMapLSt{|*->*|}

fmapRSt :: u:((.a -> .(.st -> (.b,.st))) -> v:(w:(t .a) -> x:(.st -> (w:(t .b),.st)))) | gMapRSt{|*->*|} (t a) (t b), [v <= u,x <= v,x <= w]
fmapRSt = gMapRSt{|*->*|}

bfmapLSt :: u:((.a1 -> .(.st -> (.b1,.st))) -> v:((.a2 -> .(.st -> (.b2,.st))) -> w:(x:(t .a1 .a2) -> y:(.st -> (x:(t .b1 .b2), .st))))) | gMapLSt{|*->*->*|} (t a1 a2) (t b1 b2), [v <= u,w <= v,y <= w,y <= x]
bfmapLSt = gMapLSt{|*->*->*|}

bfmapRSt :: u:((.a1 -> .(.st -> (.b1,.st))) -> v:((.a2 -> .(.st -> (.b2,.st))) -> w:(x:(t .a1 .a2) -> y:(.st -> (x:(t .b1 .b2), .st))))) | gMapRSt{|*->*->*|} (t a1 a2) (t b1 b2), [v <= u,w <= v,y <= w,y <= x]
bfmapRSt = gMapRSt{|*->*->*|}

:: Tree a b = Tip a | Bin b (Tree a b) (Tree a b)
:: Rose a = Rose a .[Rose a]
:: Fork a = Fork a a
:: Sequ a = SequEmpty | SequZero .(Sequ .(Fork a)) | SequOne a .(Sequ .(Fork a))

derive bimap (,)
derive gMapLSt [], Tree, Rose, Fork, Sequ
derive gMapRSt [], Tree, Rose, Fork, Sequ

Start =
	( fmapLSt (\x st -> (inc x, [x:st])) [1,2,3] []
	, fmapRSt (\x st -> (inc x, [x:st])) [1,2,3] []
	, bfmapRSt (\x st -> (inc x, [x:st])) (\x st -> (inc x, [toReal x:st])) (Bin 1 (Tip 2.0) (Tip 3.0)) []
	, fmapRSt (\x st -> (inc x, [x:st])) (Rose 1 [Rose 2 [], Rose 3 []]) []
	, fmapRSt (\x st -> (inc x, [x:st])) (SequZero (SequOne (Fork 1 2) (SequOne (Fork (Fork 3 4) (Fork 5 6)) SequEmpty))) []
	)
	