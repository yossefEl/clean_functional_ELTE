module hw2
import StdEnv


/* Neptun code XXXXXX */

/* 1.  keep the middle element of every sublist, if the length of the sublist is odd.
If it is even ignore that sublist 
 e.g. [[1, 2, 3], [3, 4], [5, 7, 8, 9], [11,12,13] -> [2, 12] */
keepMiddleIfOdd ::  [[Int]] -> [Int]
keepMiddleIfOdd [] = []
keepMiddleIfOdd [x:xs] 
| isEven (length x) = keepMiddleIfOdd xs
= [ x!!((length x)/2)  : keepMiddleIfOdd xs] 

// Start = keepMiddleIfOdd [[1, 2, 3], [3, 4], [5, 7, 8, 9], [11,12,13]] // [2, 12]
// Start = keepMiddleIfOdd [[1,27, 56, 44], [4,22, 29], [8,16, 29]]// [22, 16]
// Start = keepMiddleIfOdd [[1,5], [2,4], [3,3]] // []
// Start = keepMiddleIfOdd [] // []


/* 2. Write a function gaps that gives all the possibilities to take out one element from a list. 
For example:
gaps [1,2,3,4,5] = [[2,3,4,5], [1,3,4,5], [1,2,4,5], [1,2,3,5], [1,2,3,4]] */


removeAtIndex :: [Int] Int -> [Int]
removeAtIndex [] _ = []
removeAtIndex [x:xs] 0 = xs
removeAtIndex [x:xs] n =[ x : removeAtIndex xs (n-1)]
// Start = removeAtIndex [12,3,5,3,6] 2 // [12,3,3,6]

contructPossibilities :: [Int]  Int -> [[Int]]
contructPossibilities [] _ = []
contructPossibilities original index
| index == length original = []
= [removeAtIndex  original index : contructPossibilities original (index+1)]

gaps :: [Int] -> [[Int]]
gaps [] = []
gaps list = contructPossibilities list 0
Start = gaps [1,2,3,4,5] // [[2,3,4,5], [1,3,4,5], [1,2,4,5], [1,2,3,5], [1,2,3,4]]