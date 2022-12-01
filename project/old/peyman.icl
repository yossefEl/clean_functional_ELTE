module peyman
import StdEnv
// 29. rewrite map using foldr
// mymap :: (a -> b) [a] -> [b]
// mymap f [] = []
// mymap f x = [foldr f 0 [element] \\ element <- x]
// Start = mymap inc [1..10]
 

// indexOf :: Int [Int] -> Int
// indexOf n [] = 0
// indexOf n list = foldr () 0 list

// Start = indexOf 1 [1,4,100]
// generate the list [[1],[2,2],[3,3,3],[4,4,4,4],...,[10,..,10]]
generate ::[[Int]]
generate = [repeatn n n \\ n <- [1..10]]


Start =generate 


// 30. Compute the average of a list of float point numbers using the foldr function
// in one line code using one lambda function.
// avg :: [Real] -> Real
// avg list = (foldr (+) 0.0 list )/ toReal(length list)
// Start = avg [16.2, 17.8, 11.5] // 15.1666666666667
//Start = avg [13.0, 40.9] // 26.95



// 31. Write a function that takes a list of numbers and adds the first element,
// subtracts the second element, adds the third element, subtracts the fourth element, so on, 
// in this alternating repetition.
// For example: [2,3,4,5,6,7] -> 2-3+4-5+6-7 = -3

// alternatingSum :: [Int] -> Int
// alternatingSum list = [x]

//Start = alternatingSum [2..7] //-3
//Start = alternatingSum [45,-5,63,46,-345,4321] //-4599
//Start = alternatingSum [] //0
