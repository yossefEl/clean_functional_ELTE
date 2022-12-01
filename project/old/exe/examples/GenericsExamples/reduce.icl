module reduce

import StdGeneric, StdEnv

// or crush
generic gReduce t :: (a a -> a) a  t -> a
gReduce{|c|} op e x 					= e
gReduce{|PAIR|} fx fy op e (PAIR x y) 	= op (fx op e x) (fy op e y)
gReduce{|EITHER|} fl fr op e (LEFT x) 	= fl op e x
gReduce{|EITHER|} fl fr op e (RIGHT x) 	= fr op e x
gReduce{|CONS|} f op e (CONS x) 		= f op e x 
gReduce{|FIELD|} f op e (FIELD x) 		= f op e x
gReduce{|OBJECT|} f op e (OBJECT x) 	= f op e x

generic gReduceRSt t :: .t .st -> .st
gReduceRSt{|c|} x st 					= st
gReduceRSt{|UNIT|} x st 				= st
gReduceRSt{|PAIR|} fx fy (PAIR x y) st 	= fx x (fy y st)
gReduceRSt{|EITHER|} fl fr x st 		= reduceEITHER fl fr x st
gReduceRSt{|CONS|} f (CONS x) st 		= f x st
gReduceRSt{|FIELD|} f (FIELD x) st 		= f x st
gReduceRSt{|OBJECT|} f (OBJECT x) st 	= f x st

generic gReduceLSt t :: .t .st -> .st
gReduceLSt{|c|} x st 					= st
gReduceLSt{|UNIT|} x st 				= st
gReduceLSt{|PAIR|} fx fy (PAIR x y) st 	= fy y (fx x st)
gReduceLSt{|EITHER|} fl fr x st 		= reduceEITHER fl fr x st
gReduceLSt{|CONS|} f (CONS x) st 		= f x st
gReduceLSt{|FIELD|} f (FIELD x) st 		= f x st
gReduceLSt{|OBJECT|} f (OBJECT x) st 	= f x st

reduceEITHER fl fr (LEFT x) st 			= fl x st
reduceEITHER fl fr (RIGHT x) st 		= fr x st

fldr :: (.a -> .(.b -> .b)) .b .(t .a) -> .b | gReduceRSt{|*->*|} (t a)
fldr op e xs = gReduceRSt{|*->*|} op xs e  

fldl :: (.b -> .(.a -> .b)) .b .(t .a) -> .b | gReduceLSt{|*->*|} (t a)
fldl op e xs = gReduceLSt{|*->*|} (flip op) xs e

:: Tree a b = Tip a | Bin b (Tree a b) (Tree a b)
:: Rose a = Rose a .[Rose a]
:: Fork a = Fork a a
:: Sequ a = SequEmpty | SequZero .(Sequ .(Fork a)) | SequOne a .(Sequ .(Fork a))

derive gReduceRSt [], Tree, Rose, Fork, Sequ
derive gReduceLSt [], Tree, Rose, Fork, Sequ

Start = 
	( fldr cons [] [1,2,3]
	, fldl (flip cons) [] [1,2,3]
	, fldr cons [] (Rose 1 [Rose 2 [], Rose 3 []])
	, fldr cons [] (SequZero (SequOne (Fork 1 2) (SequOne (Fork (Fork 3 4) (Fork 5 6)) SequEmpty)))	
	, gReduceRSt{|*->*->*|} cons cons (Bin 1 (Tip 2) (Tip 3)) []
	)
where
	cons x xs = [x:xs]	