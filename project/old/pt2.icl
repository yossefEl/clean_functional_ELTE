module pt2
import StdEnv

// NEPTUN : XXXXXX

/*
	Given a list of integer and an integer, x. 
	
	If the element of the list is even, then add with x and if odd, then subtract by x.
	
	(Add x to all the even number and subtract x to all the odd number)
	
	And then the result list should contains only elements less than 0.
	
	Let parameters be [0, -2, -3, 1] and 4  then after transforming [4, 2, -7, -3], and then the result should be [-7, -3]

*/


// onlyNegative 
onlyNegative :: [Int] -> [Int]
onlyNegative [] = []
onlyNegative [x:xs]
| x < 0 = [x : onlyNegative xs]
| otherwise = onlyNegative xs


PT2 :: [Int] Int -> [Int]
PT2 [] _ = []
PT2 [x:xs] y 
| isEven x = [x+y : PT2 xs y]
| otherwise = [x-y : PT2 xs y]

Start = PT2 [0, -2, -3, 1] 4
//Start = PT2 [3, 2, 0, -2, -1, 10] 0 		// [-2, -1]
//Start = PT2 [-10, -15, -7, -8, -11] 8 	// [-2,-23,-15,-19]
//Start = PT2 [5, 8, -8, -5, 0, 2] 3 		// [-5, -8]
//Start = PT2 [5, 9, 7, 4, 6, 10] 2 		// []
//Start = PT2 [] 6 							// []
