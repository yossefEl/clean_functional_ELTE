module marco

// examples from Marco Pil's thesis (version April 14 2002)

import StdEnv, StdDynamic

// There should only be Trues in the output
Start
	=	[example_2_2_2, example_2_4_1, paragraph_2_5, example_2_5_1, example_2_5_2, example_3_4_6]

:: Test = E.result: Test {#Char} result

(==>) infix 0
(==>) name result
	:==	Test name result

example_2_2_2
	=	"example_2_2_2" ==> length some_dynamics == 4
	where
		some_dynamics
			=	[	dynamic True :: Bool
				,	dynamic fib :: Int -> Int
				,	dynamic reverse :: A.a:[a] -> [a]	// explicit quantifier required
				,	dynamic 5
				]

example_2_4_1
	=	"example_2_4_1" ==> [ex_a, ex_b, ex_c]
	where
		dynamicApply :: Dynamic Dynamic -> Dynamic
		dynamicApply (f :: a -> b) (x :: a)
			=	dynamic (f x) :: b
		dynamicApply df dx
			=	dynamic "Error" :: String
		
		ex_a
			=	test (dynamicApply (dynamic fib :: Int -> Int) (dynamic 7 :: Int))
			where
				test (21 :: Int)
					=	True
				test _
					=	False
		ex_b
			// explicit quantifier required
			=	test (dynamicApply (dynamic reverse :: A.a:[a] -> [a]) (dynamic [1,2,3] :: [Int]))
			where
				test ([3,2,1] :: [Int])
					=	True
				test _
					=	False
		ex_c
			=	test (dynamicApply (dynamic reverse :: A.a:[a] -> [a]) (dynamic 7 :: Int))
			where
				test ("Error" :: {#Char})
					=	True
				test _
					=	False


paragraph_2_5
	=	"paragraph_2_5" ==> [ex_a, ex_b, ex_c, ex_d]
	where
		ex_a
			=	lookup dynlist == 17
			where
				lookup :: [Dynamic] -> Int
				lookup []
					=	abort "No integer in this list"
				lookup [x :: Int : xs]
					=	x
				lookup [dy : xs]
					=	lookup xs
		ex_b
			=	lookup dynlist == 0.0
			where
				lookup :: [Dynamic] -> Real
				lookup []
					=	abort "No Real in this list"
				lookup [x :: Real : xs]
					=	x
				lookup [dy : xs]
					=	lookup xs

		ex_c
			=	lookup dynlist == True
			where
				lookup :: [Dynamic] -> Bool
				lookup []
					=	abort "No boolean in this list"
				lookup [x :: Bool : xs]
					=	x
				lookup [dy : xs]
					=	lookup xs

		ex_d
			=	(lookup dynlist + 5) == 22 && sin (lookup dynlist) == 0.0
			where
				lookup :: [Dynamic] -> a | TC a
				lookup []
					=	abort "No element of the correct type in this list"
				lookup [(x :: a^) : xs]	// different syntax
					=	x
				lookup [dy : xs]
					=	lookup xs
		dynlist
			=	[dynamic "string", dynamic 17, dynamic 0.0, dynamic True]

example_2_5_1
	=	"example_2_5_1" ==> [unwrap (dynamic 17) == Yes 17, unwrap (dynamic 3) == noChar]
	where
		unwrap (x :: a^)
			=	Yes x
		unwrap dx
			=	No
		
		noChar :: Maybe3 Char
		noChar
			=	No

example_2_5_2
	=	"example_2_5_2" ==> test (wrap 17)
	where
		wrap :: a -> Dynamic | TC a
		wrap x
			=	dynamic x :: a^ // different syntax

		test (17 :: Int)
			=	True
		test _
			=	False


example_3_4_6
	=	"example_3_4_6" ==> [ex_a, ex_b, ex_c]
	where
		ex_a
			=	g (dynamic id :: A.beta: beta -> beta) == 3
			where
				g (f :: Int -> Int)
					=	f 3

		ex_b
			=	(ap (g (dynamic id :: A.beta: beta -> beta)) 17)  == 17
			where
				g (f :: a -> a)
					=	dynamic f o f :: a -> a

				ap :: Dynamic a -> a | TC a
				ap (f :: a^ -> a^) x
					=	f x


		ex_c
			=	test (g (dynamic hd :: A.beta: [beta] -> beta))
			where
				g (f :: A.alpha: [alpha] -> a)
					=	dynamic f [1,2] :: a
				g _
					=	dynamic "No match"

				test ("No match" :: {#Char})
					=	True
				test _
					=	False


fib :: Int -> Int
fib 0
	=	1
fib 1
	=	1
fib n
	=	fib (n-1) + fib (n-2)


:: Maybe3 a
	=	No
	|	Yes a

instance == (Maybe3 a) | == a where
	(==) No No
		=	True
	(==) (Yes a) (Yes b)
		=	a == b
	(==) _ _
		=	False
	
