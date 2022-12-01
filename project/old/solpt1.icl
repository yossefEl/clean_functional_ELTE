module solpt1
import StdEnv

/* Write the function my_remainder which given two integers returns their remainder, 
the functon should not use any arithmetic operators except substraction, addition and comparison operators) 

You can assume that both numbers are positive */

/* Your neptun code : XXXXXX        */


my_remainder :: Int Int -> Int
my_remainder a b
    | a < b = a
    | otherwise = my_remainder (a - b) b


Start = my_remainder 20 5 // 0
//Start = my_remainder 13 3 // 1
// [[ ],[]]
// falattern 
falattern []= []
falattern (x:xs) = x : falattern xs
