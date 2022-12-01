module int

import StdEnv
import Data.Int
import Gast

pInt1 :: Int -> Property
pInt1 i = i <> LargestInt ==> i+1>i

pInt2 :: Int -> Property
pInt2 i = i <> SmallestInt ==> i-1<i

pInt3 :: Int Int -> Bool
pInt3 x y = x+y == y+x

pInt4 :: Int Int -> Bool
pInt4 x y = x*y == y*x

pInt5 :: Int Int -> Bool
pInt5 x y = x-y == 0-(y-x)

pInt6 :: Int Int Int -> Bool
pInt6 x y z = (x+y)+z == x+(y+z)

Start =
	[ test pInt1
	, test pInt2
	, test pInt3
	, test pInt4
	, test pInt5
	, test pInt6
	]
