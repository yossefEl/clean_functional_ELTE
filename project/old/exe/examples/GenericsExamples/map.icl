module map

import StdGeneric, StdEnv

generic gMap a b :: .a -> .b
gMap{|c|} x 					= x
gMap{|UNIT|} x					= x
gMap{|PAIR|} fx fy (PAIR x y) 	= PAIR (fx x) (fy y) 
gMap{|EITHER|} fl fr (LEFT x) 	= LEFT (fl x)
gMap{|EITHER|} fl fr (RIGHT x) 	= RIGHT (fr x)
gMap{|CONS|} f (CONS x) 		= CONS (f x)
gMap{|FIELD|} f (FIELD x) 		= FIELD (f x)
gMap{|OBJECT|} f (OBJECT x) 	= OBJECT (f x)

// class gMap* t where gMap* :: t -> t
// class gMap*->* (t a) (t b) where gMap* :: (a -> b) (t a) -> (t b)
// class gMap*->*->* (t a1 a2) (t b1 b2) where gMap*->*->* :: (a1 -> b1) (a2 -> b2) (t a1 a2) -> (t b1 b2)

fmap :: (.a -> .b) .(f .a) -> .(f .b) | gMap{|*->*|} (f a) (f b)
fmap f xs = gMap{|*->*|} f xs

bfmap :: (.a1 -> .b1) (.a2 -> .b2) .(f .a1 .a2) -> .(f .b1 .b2) | gMap{|*->*->*|} (f a1 a2) (f b1 b2)
bfmap f1 f2 xs = gMap{|*->*->*|} f1 f2 xs

:: Tree a b = Tip a | Bin b (Tree a b) (Tree a b)
:: Rose a = Rose a .[Rose a]
:: Fork a = Fork a a
:: Sequ a = SequEmpty | SequZero .(Sequ .(Fork a)) | SequOne a .(Sequ .(Fork a))
:: Id a = Id a

derive gMap [], Tree, Rose, Fork, Sequ

Start = 
	( fmap inc [1,2,3]
	//, fmap toString (Rose 1 [Rose 2 [], Rose 3 []])
	//, fmap inc (SequZero (SequOne (Fork 1 2) (SequOne (Fork (Fork 3 4) (Fork 5 6)) SequEmpty)))
	//, bfmap inc dec (Bin 1 (Tip 2.0) (Tip 3.0))
	, fmap Id 			[[1,2], [3,4]]
	, fmap (fmap Id) 	[[1,2], [3,4]]
	)
