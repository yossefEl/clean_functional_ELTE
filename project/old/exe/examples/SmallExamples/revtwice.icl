module revtwice

/*
Reversing a list a number of times using Twice.

Increase stack size to 1m and heap size to 2m to run this program.

A list containing 25 integers is reversed 65536 times by means
of four applications of the higher order function Twice.
*/

import StdInt, StdEnum

Revv:: [Int] -> [Int]
Revv l  =  Rev l []
where
	Rev::[Int] [Int] -> [Int]
	Rev [x:r] list	=  Rev r [x : list]
	Rev []    list	=  list

Twice::(x -> x) x -> x
Twice f x =  f (f x)

Start::[Int]
Start = Twice Twice Twice Twice Revv [1..25]

