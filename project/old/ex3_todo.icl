module ex3_todo
import StdEnv

// Exercises

// 1. Write a recursive function that computes the n-th multiple of an x plus 10 (n*x+10).
f1 :: Int Int -> Int
f1 x n 
| n == 0 = x + 10
| otherwise = x + f1 (n-1) x 
// Start = f1 5 2 // 20


// isOdd and isEven are built-in functions
// 2. Add 2 to every odd number of a list, and subtract 2 from every even number.
f2 :: [Int] -> [Int]
f2 [] = []
f2 [x:xs]
| x rem 2 == 0 =[ x - 2 : f2 xs]
| otherwise =[ x + 2 : f2 xs]
// Start = f2 [1..5] // [3,0,5,2,7]



// 3. Compute the triple of the negative elements of a list up to the first positive number.
f3 :: [Int] -> [Int]
f3 [x:xs]
| x < 0 = [x * 3 : f3 xs]
= []
// with map and takeWhile
// Start = f3 [-1..5] // [-3,0,-9,-6,-15]
// f3v2 :: [Int] -> [Int]
// f3v2 xs = map ((*)3) (takeWhile ((<)0 xs)//TODO fix this
// Start = f3v2 [-1..5] // [-3,0,-9,-6,-15]

// Start = f3 [-1,-3,-5,-5,2,-4,-5] // [-3, -9, -15, -15]



// 4. Write a function that keeps the non-zero elements of a list and then multiply by 2 every element.
f4 :: [Int] -> [Int]
f4 [] = []
f4 [x:xs]
| x <> 0 = [x * 2 : f4 xs]
=f4 xs

// f4fltr :: [Int] -> [Int]
// f4fltr xs = map ((*) 2) (filter ((<>) 0 xs)

// Start = f4 [1,2,3,0,5,0,6,0,0,0,0] // [2,4,6,10,12]



// 5. Write a function for the square, the cube, and so on up to the n-th power of a number,
// so that increasing powers of a number are obtained in a list.
f5 :: Int Int -> [Int]
f5 x n
| n == 0 = [x]
| otherwise = [x : f5 (x*x) (n-1)]
// Start = f5 5 2  // [4,8,16,32] //TODO fix this


// 6. Replicate n>0 times a list.
f6 :: Int [Int] -> [[Int]]
f6 n xs
| n == 0 = []
| otherwise = [xs : f6 (n-1) xs]
// Start = f6 3 [1..5] // [[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]



// 7. Insert 0 at the middle of each sublist.
f7 :: [[Int]] -> [[Int]]
f7 [] = []
f7 [x:xs]
| length x == 0 = [x : f7 xs]
// | otherwise = [x : 0 : f7 xs]

// Start = f7 [[1..10], [1..11], [], [1], [1,2]] // [[1,2,3,4,5,0,6,7,8,9,10],[1,2,3,4,5,0,6,7,8,9,10,11],[0],[0,1],[1,0,2]]



// 8. Extract the elements smaller then the head element of a list.
f8a :: Int [Int] -> [Int]
// returns the elements smaller than the head element of a list
f8a x xs
| x == 0 = []
| otherwise = [x : f8a xs]

f8 :: [Int] -> [Int]
f8 [] = []
f8 [x:xs]
| x < head xs = [x : f8 xs]
= f8 xs

Start = f8 [5,1,2,3,4,5,3,6,7,1,8] // [1,2,3,4,3,1]



// 9. Eliminate in a list the sublists that are longer or equal 10.
//cond9 :: [Int] -> Bool

//f9 :: [[Int]] -> [[Int]]

//Start = f9 [[1..10], [1..11], [1..5], []] // [[1,2,3,4,5],[]]



// 10. Compute the greatest common divisor in a recursive function.
//f10 :: Int Int -> Int

//Start = f10 24 12


// 11. Given a list of Ints, remove the element at the given position.
//remElemAt :: Int [Int] -> [Int]
//remElemAt i list = removeAt i list

//Start = remElemAt 6 [1..7] // [1,2,3,4,5,6]
//Start = remElemAt 2 [1..7] // [1,2,4,5,6,7]
//Start = remElemAt 9 [1..7] // [1,2,3,4,5,6,7]



// 12. Given a list of integers, find the minimum of a list (assume the list is not empty). 
//minimum :: [Int] -> Int 
//minimum x = minList x

/*
[1,0,3,4,5]
minimum [0,3,4,5]
minimum [0,4,5]
minimum [0,5]
minimum [0]
*/

//Start = minimum [1..5] // 1
//Start = minimum [10,9,8,7,6] // 6
//Start = minimum [8,6,4,10,12] // 4

//minimum2 :: [Int] -> Int 

/*
minimum2 [1,2,0,-1] -> min 1 (min 2 (min 0 (-1)))
minimum2 [1,2,0,-1] -> min 1 (min 2 -1)
minimum2 [1,2,0,-1] -> min 1 -1
minimum2 [1,2,0,-1] -> -1
*/

//Start = minimum2 [1..5] // 1
//Start = minimum2 [10,9,8,7,6] // 6
//Start = minimum2 [8,6,4,10,12] // 4


// 13. Check if an element of a logical list is true.
//ifOneTrue :: [Bool] -> Bool

//Start = ifOneTrue [False, False, False] // False
//False || False || False || False



// 14. Check if all elements of a logical list are true.
//ifAllTrue :: [Bool] -> Bool

//Start = ifAllTrue [True, False, True] // False
//True && False && True  && True



// 15. Write a function that checks if at least one of the elements in a list is even.
//is_one_Even :: [Int] -> Bool 

//Start = is_one_Even [1,1,3] // False
//Start = is_one_Even [1..9] // True
//Start = is_one_Even [2,4..14] // True
//Start = is_one_Even [] // False



// 16. Write a function that checks if all of the elements in a list are even.
//allEven :: [Int] -> Bool
  
//Start = allEven [2,4,6]
// [2,4,6] -> isEven 2 && isEven 4 && isEven 6 && True 
//Start = allEven [1..9] // False
//Start = allEven [2,4..14] // True
//Start = allEven [] // True



// 17. Collect the divisors of a number in a list.
//divisorsAux :: Int Int [Int] -> [Int]
//divisorsAux inc stop accum
//| inc > stop = accum
//| stop rem inc  == 0 = divisorsAux (inc+1) stop (accum ++ [inc]) 
//= divisorsAux (inc+1) stop accum


//divisors :: Int -> [Int] 
//divisors number = divisorsAux 1 number [] 

//Start = divisors 18 // [1,2,3,6,9,18]



//Divisors :: Int Int -> [Int] 
//Divisors inc stop
//| inc > stop = []
//| stop rem inc == 0 = [inc] ++ Divisors (inc+1) stop
//= Divisors (inc+1) stop 


/*
Divisors 9 1 
/*| 1 == 9  = False */
| 9 / 1 == 0  True ->  [1] ++ [3] ++ [] = [1,3] 
Divisors 9 2
/*| 9 / 2 == 1 (False ) ->*/
Divisors 9 3 
Divisors 9 4 
Divisors 9 5 
.. etc 
Divisors 9 9 = []
*/

//divisors2 :: Int -> [Int] 
//divisors2 number = Divisors 1 number

//Start = divisors2 18 // [1,2,3,6,9,18]




// 18. Delete every second elemetn form a list.
//del2 :: [Int] -> [ Int]

//Start = del2 [1..10] // [1,3,5,7,9]
//Start = del2 [1..11] // [1,3,5,7,9,11]



// 19.* (bonus) Compute the Euler number aproximation in n steps: e = 1/0! + 1/1! + 1/2! + 1/3! + ... 
// do not compute factorial 
//f11 :: Int -> Real

// Start = f11 1000