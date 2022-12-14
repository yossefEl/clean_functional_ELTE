implementation module Data.GenLexOrd

import StdEnv
import StdGeneric, Data.GenEq

:: LexOrd = LT |EQ | GT
derive gEq LexOrd

generic gLexOrd a :: !a !a -> LexOrd
gLexOrd{|Int|} x y
	| x == y = EQ
	| x < y	 = LT
			 = GT
gLexOrd{|Bool|} True True = EQ
gLexOrd{|Bool|} False True = LT
gLexOrd{|Bool|} True False = GT
gLexOrd{|Bool|} False False = EQ
gLexOrd{|Real|} x y
	| x == y = EQ
	| x < y	 = LT
			 = GT
gLexOrd{|Char|} x y
	| x == y = EQ
	| x < y	 = LT
			 = GT
gLexOrd{|String|} x y
	| x == y = EQ
	| x < y	 = LT
			 = GT			 
gLexOrd{|UNIT|} UNIT UNIT = EQ
gLexOrd{|PAIR|} fx fy (PAIR x1 y1) (PAIR x2 y2) = case fx x1 x2 of
	EQ -> fy y1 y2
	LT -> LT
	GT -> GT
	 		
gLexOrd{|EITHER|} fl fr (LEFT x) (LEFT y) = fl x y
gLexOrd{|EITHER|} fl fr (LEFT x) (RIGHT y) = LT
gLexOrd{|EITHER|} fl fr (RIGHT x) (LEFT y) = GT
gLexOrd{|EITHER|} fl fr (RIGHT x) (RIGHT y) = fr x y
gLexOrd{|CONS|} f (CONS x) (CONS y) = f x y
gLexOrd{|FIELD|} f (FIELD x) (FIELD y) = f x y
gLexOrd{|OBJECT|} f (OBJECT x) (OBJECT y) = f x y
gLexOrd{|RECORD|} f (RECORD x) (RECORD y) = f x y

// Instance on standard lists is needed because
// standard lists have unnatural internal ordering of constructors: Cons < Nil,
// i.e Cons is LEFT and Nil is RIGHT in the generic representation.
// We want ordering Nil < Cons
gLexOrd{|[]|}   f xs ys = lexOrdList f xs ys
gLexOrd{|[!]|}  f xs ys = lexOrdList f xs ys
gLexOrd{|[ !]|} f xs ys = lexOrdList f xs ys
gLexOrd{|[!!]|} f xs ys = lexOrdList f xs ys
gLexOrd{|[#]|}  f xs ys = lexOrdList f xs ys
gLexOrd{|[#!]|} f xs ys = lexOrdList f xs ys

gLexOrd{|{}|} f xs ys 	= lexOrdArray f xs ys 
gLexOrd{|{!}|} f xs ys 	= lexOrdArray f xs ys 

// standard types
derive gLexOrd (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

lexOrdList :: !(a a -> LexOrd) !(l a) !(l a) -> LexOrd | List l a
lexOrdList _ [|] ys =
	case ys of
		[|] = EQ
		_   = LT
lexOrdList f [|x:xs]  ys =
	case ys of
		[|] = GT
		[|y:ys] =
			case f x y of
				EQ -> lexOrdList f xs ys
				LT -> LT
				GT -> GT

lexOrdArray f xs ys
	#! size_xs = size xs
	#! size_ys = size ys
	| size_xs < size_ys	= LT
	| size_xs > size_ys = GT
	| otherwise			= lexord 0 size_xs xs ys
where
	lexord i n xs ys
		| i == n 		= EQ
		| otherwise 	= case f xs.[i] ys.[i] of
				LT -> LT
				GT -> GT	
				EQ -> lexord (inc i) n xs ys
