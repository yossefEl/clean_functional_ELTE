module t

import StdDynamic
//import StdDynamicFileIO
import StdEnv

// repack the value/type in a dynamic in a tuple
//   example:
//		tupleDynamic (dynamic id A.a: a->a)
//	==> dynamic (id,id) A.a: (a->a, a->a)
tupleDynamic :: Dynamic -> Dynamic
tupleDynamic (x :: a)
	=	dynamic (x, x) :: (a, a)

// repack the value/type in a dynamic in a tuple
// create fresh type variables for each tuple element
//   example:
//		tupleDynamicFresh (dynamic id A.a: a->a)
//	==> dynamic (id,id) A.a b: (a->a, b->b)
tupleDynamicFresh :: Dynamic -> Dynamic
tupleDynamicFresh (x :: a)
	=	tuple x x
	where
		tuple :: a b -> Dynamic | TC a & TC b
		tuple a b
			=	dynamic (a, b) :: (a^, b^)

dynamicWithBigType
	=	iterate tupleDynamic (dynamic id :: A.a: a -> a) !!  10
dynamicWithBigTypeAndManyTypeVariables
	=	iterate tupleDynamicFresh (dynamic id :: A.a: a -> a) !! 10

:: Double a
	:==	(a, a)

:: Big a
	:== Double (Double (Double (Double a)))

:: Enormous a
	:== (Big (Big (Big a)))


dEnormous
	=	dynamic undef :: A.a: Enormous a


Start
//	=	[(i, d ::>= d) \\ d <- iterate doubleDynamic (dynamic undef :: A.a: a) & i <- [1..]]
//	=	dynamicWithBigTypeAndManyTypeVariables ::>= dynamicWithBigTypeAndManyTypeVariables
	=	dEnormous ::>= dEnormous
//	=	(dynamic undef :: A.a: a) ::>= (dynamic undef :: A.a: a)
//	=	(dynamic undef :: Bool) ::>= (dynamic undef :: Int)

f (x:: A.a: a -> a)
	=	True

loop :: !Int !Dynamic !Dynamic !Bool -> Bool
loop 0 d1 d2 b
	=	b
loop n d1 d2 b
	=	loop (n-1) d1 d2 (d1 ::>= d2 && b)

loop2 :: !Int !Int !Int !Bool -> Bool
loop2 0 d1 d2 b
	=	b
loop2 n d1 d2 b
	=	loop2 (n-1) d1 d2 (d1 >= d2 && b)

dInc
	=	dynamic inc :: Int -> Int

dId
	=	dynamic id :: A.a: a->a

wrap x
	=	dynamic x

compare :: Dynamic Dynamic -> {#Char}
compare d1 d2
	| d1 ::== d2
		=	showComparison d1 " ::== " d2
	| d1 ::>= d2
		=	showComparison d1 " ::>= " d2
	| d2 ::>= d1
		=	showComparison d1 " ::<= " d2
	// otherwise
		=	showComparison d1 " ::=/= " d2
	where
		showComparison :: Dynamic {#Char} Dynamic -> {#Char}
		showComparison d1 cmp d2
			=	typeOfDynamicToString d1 +++ cmp +++ typeOfDynamicToString d2

		typeOfDynamicToString
			=	toString o typeCodeOfDynamic

(::==) infix  4 :: Dynamic Dynamic -> Bool
(::==) d1 d2
	=	d1 ::>= d2 && d2 ::>= d1

(::>=) infix  4 :: Dynamic Dynamic -> Bool
(::>=) d (v :: a)
	=	isInstance d v
	where
		isInstance :: Dynamic a -> Bool | TC a
		isInstance (_ :: a^) _
			=	True
		isInstance _ _
			=	False
