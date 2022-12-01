module hw3
import StdEnv

/* 
    Neptun Code : XXXXXX
    Name : Youssef ELMOUMEN
*/

// Try to use higher order functions if you can

/*
1. Given a list of numbers return the index of the number that including that
number the sum of all previous numbers equals 0.
Example:
equalsZero [1,2,3,-3,-2,-1,3,4,1,-1] = 5
which is the index of -1. The sum of [1,2,3,-3,-2,-1] is zero.
If there is no such number return -1
*/


equalsZero :: [Int] -> Int
equalsZero [] = -1
equalsZero  list
| foldr (+) 0 (list) == 0 = (length ( list)) - 1
= equalsZero (init list)
// Start = equalsZero [1,2,3,-3,-2,-1,3,4,1,-1] //5


/* 2. Accountants are always interested in finding numbers that contain the digit 7. 
Implement a function that filters if the number does not contain digit 7
Example: 
hasSeven [3, 13, 27, 771, 674, 301] = [27, 771, 674]
*/

// this function convert an integer to a list of digits
digitHasSeven :: Int -> Bool 
digitHasSeven x 
| x < 10 = 7==x
| x==0 = False
=digitHasSeven (x / 10 ) || digitHasSeven (x rem 10)
 
// Start = digitHasSeven 771 // True
hasSeven :: [Int] -> [Int]
hasSeven [] = []
hasSeven list = filter digitHasSeven list

// Start = hasSeven [10,3,2,45] //[]
Start = hasSeven [3, 13, 27, 771, 674, 301] // [27, 771, 674]
