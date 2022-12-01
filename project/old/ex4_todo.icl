module ex4_todo

import StdEnv


// Earlier exemples rewritten with map or fold.

// 1. Operations with lists: write functions for the followings
// keep the head of every sublist (sublist are not empty)
// e.g. [[1, 2, 3], [3, 4], [5, 7, 8, 9]] -> [1, 3, 5]
//heads :: [[Int]] -> [Int]
//heads x = map

//Start = heads [[1, 2, 3], [3, 4], [5, 7, 8, 9]] // [1,3,5]



// 2. Keep the tails of a list in 2 versions 
// e.g. [[1, 2, 3], [3, 4], [5, 7, 8, 9]] -> [[2, 3], [4], [7, 8, 9]] 
//tails :: [[Int]] -> [[Int]]
//tails x = map

//Start = tails [[1, 2, 3], [3, 4], [5, 7, 8, 9]] // [[2, 3], [4], [7, 8, 9]] 



// 3. Triple the elements of a list
//triples :: [Int] -> [Int]
//triples x = map

//Start = triples [1..5] // [3,6,9,12,15]

//triples2 :: [Int] -> [Int]
//triples2 x = map

//Start = triples2 [1..5] // [3,6,9,12,15]



// 4. Check if the numbers of a list are odd.
//isoddnrs :: [Int] -> [Bool]
//isoddnrs x = map 
//isoddnrs2 x = map 

//Start = isoddnrs [1..5] // [True,False,True,False,True]



// 5. Add 100 to the numbers of a list.
//g :: Int -> Int
//g x = x + 100
//add100 :: [Int] -> [Int]
//add100 x = map 
//add1002 x = map   

//Start = add100 [1..8] // [101,102,103,104,105,106,107,108]



// 6. Check if the numbers of a list are multiple of 10.
ismult10 :: [Int] -> [Bool]
ismult10 x = map 



//Start = ismult10 [1..20]
// [False,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,False,True]



// 7. Collect in a list the last digits of the numbers of a list.
//lastdigits :: [Int] -> [Int]
//lastdigits x = map 

//Start = lastdigits [1..35]
// [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5]



// 8. Compute the cube of the numbers of a list.
//cubes :: [Int] -> [Int]
//cubes x = map 
//cubes2 x = map 

//Start = cubes [1..10] // [1,8,27,64,125,216,343,512,729,1000]
//Start = cubes2 [1..10] // [1,8,27,64,125,216,343,512,729,1000]
//Start = cubes [] // []

// do not confuse with powers of 3
//powersof3 x = map (\x = 3^x) x
//powersof32 x = map ((^)3) x

//Start = powersof3 [1..5] // [3,9,27,81,243]
//Start = powersof32 [1..5] // [3,9,27,81,243]



// 9.  Add the numbers from 1..N (N positive and not 0) using foldr.
//addn :: Int -> Int
//addn n = foldr 

//Start = addn 5 // 15
//Start = addn 0 // 0
//Start = addn -2  // 0
//Start = addn 10



// 10. Reverse every sublist of a list
//revsub :: [[Int]] ->  [[Int]]
//revsub x = map 

//Start = revsub [[1,2,3],[5,6],[],[7,8,9,10]] // [[3,2,1],[6,5],[],[10,9,8,7]]



// 11. Keep the last elements of the sublists of a list in one list 
// (the sublists are not empty).
// [[1,2,3],[5,6],[1],[7,8,9,10]] -> [3,6,1,10]
//lasts :: [[Int]] -> [Int]
//lasts x = map 

//Start = lasts [[1,2,3],[5,6],[1],[7,8,9,10]] // [3,6,1,10]



// 12. Instert 0 in front of every sublist of a list.
// E.g. for [[1,2,3],[5,6],[],[7,8,9,10]] the result is 
// [[0,1,2,3],[0,5,6],[0],[0,7,8,9,10]] 
ins0 :: [[Int]] -> [[Int]]
ins0 x = map 

//Start = ins0 [[1,2,3],[5,6],[],[7,8,9,10]] // [[0,1,2,3],[0,5,6],[0],[0,7,8,9,10]]



// 13. Delete the last element of each sublist of a list.
// E.g. for [[1,2,3],[5,6],[],[7,8,9,10]] the result is [[1,2],[5],[],[7,8,9]]
//lastdel :: [[Int]] -> [[Int]]
//lastdel x = map 

//Start = lastdel [[1,2,3],[5,6],[],[7,8,9,10]] // [[1,2],[5],[],[7,8,9]]



// review foldr
//Start = foldr (+) 1 [4,5,6]  //(4 + (5 + (6 + 1))) 16

// 14. Compute the product of the elements of a list using foldr.
//prodf :: [Int] -> Int
//prodf x = foldr 

prodf2 x = prod x

//Start = prodf [1,5,2,4] // 40



// 15. compute the squares of the elements of a list using map
// [1, 2, 3] -> [1, 4, 9]
sq :: Int -> Int
sq x = x*x

//sqrs :: [Int] -> [Int]
//sqrs x = map 

//Start = sqrs [1, 2, 3] // [1,4,9]



// 16. same as 1. with lambda expression 
//sqrs_lambda :: [Int] -> [Int]
//sqrs_lambda y = map 

//Start = sqrs_lambda [1, 2, 3] // [1,4,9]



// 17. Compute 1*1 + 2*2 + ... + n*n  for n positive using map and foldr.
//sumsqr :: Int -> Int
//sumsqr n = 

//Start = sumsqr 5 // 55
//Start = sumsqr 0 // 0
//Start = sumsqr -4 // 0



// 18. compute the double of the positive elements of a list [1, 2, -2, 3, -4] -> [2, 4, 6]
// hints: first filter it then use map 
//f2 :: [Int] -> [Int]
//f2 x = 


//Start = f2 [1, 2, -2, 3, -4] // [2, 4, 6]



// 19. write a function that keeps the integers of a list up to the first 0 encounterred 
// and then divides by 2 every element [1, 2, -2, 3, 0, -4] -> [0, 1, -1, 1]
// hints: use takeWhile then map
//f3 :: [Int] -> [Int]
//f3 x = 

//Start = f3 [1, 2, -2, 3, 0, -4] // [0, 1, -1, 1]



// 20. write a function for the square of every element of a list and sublists
// [[1,2],[3,4,5,6],[7,8]]  -> [[1,4],[9,16,25,36],[49,64]]  
// hint: map in map
fa :: [Int]-> [Int]
fa x = map (\x = x*x) x

//Start = fa [1..5]

//f4 :: [[Int]] -> [[Int]]
//f4 x = map 

f42 x = map (map (\x = x*x)) x

//Start = f42 [[1,2],[3,4,5,6],[7,8]] // [[1,4],[9,16,25,36],[49,64]]



// 21. Replicate n>0 times the element of a list e.g. n=3 [3..6] ->
// [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
rep :: Int Int -> [Int]
rep 0 x = []
rep n x = [x : rep (n-1) x]

//Start = rep 3 7

//f5 :: Int [Int] -> [[Int]]
//f5 n x = map (\ x 
//f52 n x = map 

//Start = f5 3 [3..6] // [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]



// 22. filter the elements smaller then n, e.g. n=3 [1,5,3,2,1,6,4,3,2,1] -> [1,2,1,2,1]
//f7 :: Int [Int] -> [Int]
//f7 n x = filter

//Start = f7 3 [1,5,3,2,1,6,4,3,2,1]  // [1,2,1,2,1]




// 23. using notempty eliminate the empty lists: 
// [[1,2,3],[],[3,4,5],[2,2],[],[],[]] -> [[1,2,3], [3,4,5], [2,2]]

notempty :: [Int] -> Bool
notempty x = not (x == [])

//f8 :: [[Int]] -> [[Int]]
//f8 x = filter 

//Start = f8 [[1,2,3],[],[3,4,5],[2,2],[],[],[]] // [[1,2,3],[3,4,5],[2,2]]



// 24. compute the sum of the sublist using foldr [[1,2,3], [3,4,5], [2,2]] -> [6, 12, 4]
//f9 :: [[Int]] -> [Int]
//f9 x = map (\ x = foldr 
//f9 x = map (foldr 
//f9 x = map sum x

//Start = f9 [[1,2,3], [3,4,5], [2,2]]



// 25. Insert the sum of elements of the sublist as last element in every sublist of a list.
insLast :: [Int] -> [Int] 
insLast list = list ++ [sum list] 

//insSum :: [[Int]] -> [[Int]]
//insSum lists = map 
//insSum2 lists = map (\x = 

//Start = insSum [[1,2], [3,4,5], [6,5,9,7], [], [8]] // [[1,2,3],[3,4,5,12],[6,5,9,7,27],[0],[8,8]]



// 26. Write a function that checks if each elements in the list appear even times.
//     e.g. checkEven [1,1,2,2,2,2,3,5,3,5] = True  
checkAux :: [Int] Int -> Bool
checkAux list number = isEven (length (filter ((==) number) list)) 

//checkEven :: [Int] -> Bool
//checkEven list = 

//Start = checkEven [1,1,2,2,2,2,3,5,3,5] // True
//Start = checkEven [1,1,2,2,1] // False
//Start = checkEven [] // True



// 27. Insert x as second element in every sublist of a list.
// if the sublist was empty then x will be the only element in the new sublist. 
// [[1,2], [3,4,5], [6,5,9,7], [], [8]] 10 -> [[1,10,2], [3,10,4,5], [6,10,5,9,7], [10], [8,10]]
insAux :: Int [Int]  -> [Int]
insAux e [] = [e]
insAux e [x:xs] = [x,e:xs]

//insertAtTwo :: [[Int]] Int -> [[Int]]
//insertAtTwo lists number = map 

//insertAtTwo2 lists number = map (insertAt 1 number) lists  

//Start = insertAtTwo2 [[1,2], [3,4,5], [6,5,9,7], [], [8]] 10 
// [[1,10,2],[3,10,4,5],[6,10,5,9,7],[10],[8,10]]



// 28. Given a list of lists, for each list, extract the first, middle and last element.
//extract3 :: [[Int]] -> [(Int, Int, Int)]
//extract3 x = map (\x = 

//Start = extract3 [[1..9], [2..6], [3..11], [1..10]] // [(1,5,9),(2,4,6),(3,7,11),(1,6,10)]
//Start = extract3 [] //[]



// 29. (bonus point) rewrite map using foldr
//mymap :: (a -> b) [a] -> [b]

// Start = mymap inc [1..10]

 
    
// 30. (bonus point) Compute the average of a list of float point numbers using the foldr function
// in one line code using one lambda function.
//avg :: [Real] -> Real

//Start = avg [16.2, 17.8, 11.5] // 15.1666666666667
//Start = avg [13.0, 40.9] // 26.95



// 31. (bonus point) Write a function that takes a list of numbers and adds the first element,
// subtracts the second element, adds the third element, subtracts the fourth element, so on, 
// in this alternating repetition.
// For example: [2,3,4,5,6,7] -> 2-3+4-5+6-7 = -3

//alternatingSum :: [Int] -> Int

//Start = alternatingSum [2..7] //-3
//Start = alternatingSum [45,-5,63,46,-345,4321] //-4599
//Start = alternatingSum [] //0
