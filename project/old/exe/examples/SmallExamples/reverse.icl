module reverse

//	A list containing n elements will be reversed n times.

import StdEnv

NrOfTimes :== 1000
	
//	Reversing a list of n integers n times.

MyReverse::Int -> Int
MyReverse n =  last (Rev_n n [1..n])
where
	Rev_n::Int [Int] -> [Int]
	Rev_n 1 list	=  Rev list []
	Rev_n n list	=  Rev_n (n - 1) (Rev list [])

	Rev::[Int] [Int] -> [Int]
	Rev [x:r]	list	=  Rev r [x : list]
	Rev []		list	=  list


//	The Start rule.

Start::Int
Start = MyReverse NrOfTimes

