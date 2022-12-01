module pt5
import StdEnv

/*
    Youssef ELMOUMEN
    XXXXXX
*/

/*Write a function sums_to_n that takes an argument n,
    and returns all two-element tuples that sum to n.
*/
    
sums_to_n:: Int -> [(Int, Int)]
sums_to_n n = [(x, y) \\ x <- [0..n], y <- [0..n] | x + y == n]

// Start = sums_to_n 5 // [(0,5), (1,4), (2,3), (3,2), (4,1), (5,0)]
Start = sums_to_n 0 // [(0,0)]
Start = sums_to_n 10 //[(0,10), (1,9), (2,8), (3,7), (4,6), (5,5), (6,4), (7,3), (8,2), (9,1), (10, 0)]