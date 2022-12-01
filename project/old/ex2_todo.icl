module ex2_todo


import StdEnv


l1 :: [Int]
l1 = [1, 2, 3, 4, 5]
l2 :: [Bool]
l2 = [True, False, True]
l3 :: [Real->Real]
l3 = [sin, cos, sin]
l4 :: [[Int]]
l4 = [[1, 2, 3], [8, 9]]
l5 :: [a]
l5 = []
l6 :: [Int]
l6 = [1..10]
l7 :: [Int]
l7 = [1..]

//Start = l4


// Generator examples

/*Start = [1..10]     
  Start = [1,2..10]   
  Start = [1,0.. -10] 
  Start = [1.. -10]   
  Start = [1..0]      
  Start = [1..1]      
  Start = [1,3..4]  
  Start = [1..]       
  Start = [1,3..]     
  Start = [100,80..]  */

// Start = [1,2..10]

// Built-in funtions
 
// Start = hd [1, 2, 3, 4, 5]       
// Start = tl [1, 2, 3, 4, 5]      
// Start = drop 2 [1, 2, 3, 4, 5]   
// Start = take 2 [1, 2, 3, 4, 5]   
// Start = [1, 2, 3] ++ [6, 7]      
// Start = reverse [1, 2, 3]        
// Start = length [1, 2, 3, 4]      
// Start = last [1, 2, 3]           
// Start = init [1, 2, 3]           
// Start = isMember 2 [1, 2, 3]     
// Start = isMember 5 [1, 2, 3]     
// Start = flatten [[1,2], [3, 4, 5], [6, 7]]  

// Start = take 2 []                
// Start = drop 5 [1,2,3]           
// Start = take 2 [1 .. 10]         
// Start = drop ([1..5]!!2) [1..5]  

// Start = reverse [1,3..10]           
// Start = reverse [5,4 .. -5]         
// Start = isMember 0 []               
// Start = isMember -1 [1..10]         
// Start = isMember ([1..5]!!1) [1..5] 


// Exercises
// All the exercises are to be solved without map



// 1. Evaluate the following expressions:

//Start = (take 3 [1..10]) ++ (drop 3 [1..10])

//Start = length (flatten [[1,2], [3], [4, 5, 6, 7], [8, 9]])

//Start = isMember (length [1..5]) [7..10]

//Start = [1..5] ++ [0] ++ reverse [1..5]



// 2. Generate lists 

// from -10 to 10
//Start = 


// even numbers from 1 to 100
//Start = 


// every 10th element from 0 to 100
//Start = 


// Generate a list with every fifth element form 0 to 500.
//Start = 


// Generate a list with every 500th element form -10000 to 10000.
//Start =



// 3. sumsq n returns 1*1 + 2*2 + ... + n*n - with a pattern for 0
//sumsq :: Int -> Int

//Start = sumsq 3 // 14


// version 2. - without pattern for 0
//sums :: Int -> Int

//Start = sums 3
  


// 4. Compute for a given positive n the sum of 2i*(2i+1), for i from 1 to n. E.g. for n=3 the sum is 68.
//f :: Int -> Int

//Start = f 3

// f 3
// 2*3*(2*3+1) + f 2
// 2*3*(2*3+1) + 2*2*(2*2+1) + f 1
// 2*3*(2*3+1) + 2*2*(2*2+1) + 2*1*(2*1+1) + f 0
// 2*3*(2*3+1) + 2*2*(2*2+1) + 2*1*(2*1+1) + 0
// 68



// 5. Compute the sum 1+ 2*2+ 3*3*3+ 4*4*4*4+ 5*5*5*5*5+ ...+n*n*n*...*n 
// where n is a positive number.
//sumpowers :: Int -> Int

//Start = sumpowers 3

// sumpowers 3
// 3^3 + sumpowers 2
// 3^3 + 2^2 + sumpowers 1
// 3^3 + 2^2 + 1^1 + sumpowers 0
// 3^3 + 2^2 + 1^1 + 0
// 27+4+1
// 32
//Start = sumpowers 5
//Start = sumpowers 0



// 6. write a funtion which returns true if a is divisible by b
//div_by :: Int Int -> Bool

//Start = div_by 16 4      // True



// 7. write a funtion which returns true if a is divisible by b or vice versa
//div_any :: Int Int -> Bool

//a rem b == 0 || b rem a == 0

//Start = div_any 4 16     // True



// 8. Cut a list in two parts at the middle. 
// E.g. cut [1..10] -> [[1,2,3,4,5],[6,7,8,9,10]]
// and for cut [1..11] the result is [[1,2,3,4,5],[6,7,8,9,10,11]].
//cut :: [Int] -> [[Int]]

//Start = cut [1..10]
//Start = cut [1..11]
//Start = cut []
//Start = cut [1]



// 9. Test if a list is symmetrical
//sim :: [Int] -> Bool

//Start = sim [1, 2, 1]
//Start = sim [1, 2, 3,4,5]



// 10. Extract the middle element of a non-empty list. E.g. for [1..5] is 3, for [1..4] is 3.
//middle :: [Int] -> Int

//Start = middle [1..5] 
//Start = middle [1..4] 
//Start = middle [1]
//Start = middle []



// 11. add 3 to every element of a list
//f1 :: [Int] -> [Int]

//Start = f1 [1,5,3,1,6]  // [4,8,6,4,9]  



// 12. compute the double of the positive elements of a list [1, 2, -2, 3, -4] -> [2, 4, 6]
//f2 :: [Int] -> [Int]

//Start = f2 [1, 2, -2, 3, -4] // [2, 4, 6]



// 13. write a function that keeps the integers of a list up to the first 0 encounterred 
// and then divides by 2 every element [1, 2, -2, 3, 0, -4] -> [0, 1, -1, 1]
// hints: use takeWhile then map
//f3 :: [Int] -> [Int]

//Start = f3 [1, 2, -2, 3, 0, -4] // [0, 1, -1, 1]



// 14. write a function for the square of every element of a list and sublists
// [[1,2],[3,4,5,6],[7,8]]  -> [[1,4],[9,16,25,36],[49,64]]  
//sq :: [Int] -> [Int]

//Start = sq [1..5]

//f4 :: [[Int]] -> [[Int]]

//Start = f4 [[1,2],[3,4,5,6],[7,8]] // [[1,4],[9,16,25,36],[49,64]]



// 15. Replicate n>0 times the element of a list e.g. n=3 [3..6] ->
// [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
//faux :: Int Int -> [Int]

//Start = faux 3 4

//f5 :: Int [Int] -> [[Int]]

//Start = f5 3 [3..6]



// 16. Compute the product of the elements of a list
//product :: [Int] -> Int

//Start = product [1..5] // 120



// 17. delete the elements equal to 5
//not_five :: [Int] -> [Int]

//Start = not_five [5,4,5,4,3]  // [4,4,3]

//not_five2 :: [Int] -> [Int]

//Start = not_five2 [5,4,5,4,3]  // [4,4,3]

// not_five2 [5,4,5,4,3]
//            x  xs
// not_five2 [4,5,4,3]
// [4 : not_five2 [5,4,3]]
// [4 : not_five2 [4,3]]
// [4 : [ 4: not_five2 [3]]
// [4 : [ 4: [3 : not_five2 []]]
// [4 : [ 4: [3 :  []]]
// [4,4,3]



// 18. Delete an element n from a list
//del :: Int [Int] -> [Int]

//Start = del 5 [1, 5, 6, 7, 5, 8, 5] // [1, 6, 7, 8]



// 19. Keep the first 2 and the last 2 elements of a list
//droptake2 :: [Int] -> [Int]

//Start = droptake2 [1, 2, 3, 4, 5, 6, 7, 8, 9]
//Start = droptake2 [1, 2]



// 20. Delete the first and the last element of a list.
//del_firstlast :: [Int] -> [Int]

//Start = del_firstlast [1..10]

//del_firstlast2 :: [Int] -> [Int]

//Start = del_firstlast2 [1..10]
//Start = del_firstlast2 []
//Start = del_firstlast2 [1]



// 21. Rewrite flatten with ++
//Start = flatten [[1, 2, 3], [3, 4], [5, 7, 8, 9]]

//lc :: [[Int]] -> [Int]

//Start = lc [[1, 2, 3], [3, 4], [5, 7, 8, 9]]
// lc [[1, 2, 3], [3, 4], [5, 7, 8, 9]]
//      x         xs
// [1, 2, 3] ++ lc [[3, 4], [5, 7, 8, 9]]
// [1, 2, 3] ++ [3, 4] ++ lc [[5, 7, 8, 9]]
// [1, 2, 3] ++ [3, 4] ++ [5, 7, 8, 9] ++ lc [] 
// [1, 2, 3] ++ [3, 4] ++ [5, 7, 8, 9] ++ [] 
// [1, 2, 3, 3, 4, 5, 7, 8, 9]




// 22. Operations with lists: write functions for the followings
// keep the head of every sublist e.g. [[1, 2, 3], [3, 4], [5, 7, 8, 9]] -> [1, 3, 5]
//heads :: [[Int]] -> [Int]

//Start = heads [[1, 2, 3], [3, 4], [5, 7, 8, 9]]

//heads [[1, 2, 3], [3, 4], [5, 7, 8, 9]]
//         x          xs
// [1 : heads [[3, 4], [5, 7, 8, 9]]]
// [1 : [3 : heads [[5, 7, 8, 9]]]]
// [1 : [3 : [5 : heads []]]]
// [1 : [3 : [5 : []]]]
// [1,3,5]





// 23. Keep the tails of a list in 2 versions 
// e.g. [[1, 2, 3], [3, 4], [5, 7, 8, 9]] -> [[2, 3], [4], [7, 8, 9]] 
//tails :: [[Int]] -> [[Int]]

//Start = tails [[1, 2, 3], [3, 4], [5, 7, 8, 9]]

//tailsd :: [[Int]] -> [[Int]]

//Start = tailsd [[1, 2, 3], [3, 4], [5, 7, 8, 9]]



// 24. Reverse every sublist of a list 
//revsub :: [[Int]] ->  [[Int]]

//Start = revsub [[1,2,3],[5,6],[],[7,8,9,10]]



// 25. Keep the last elements of the sublists of a list in one list (the sublists are not empty).
// [[1,2,3],[5,6],[1],[7,8,9,10]] -> [3,6,1,10]
//lasts :: [[Int]] -> [Int]

//Start = lasts [[1,2,3],[5,6],[1],[7,8,9,10]]



// 26. Insert 0 in front of every sublist of a list.
// E.g. for [[1,2,3],[5,6],[],[7,8,9,10]] the result is [[0,1,2,3],[0,5,6],[0],[0,7,8,9,10]]
//ins0 :: [[Int]] -> [[Int]]

//Start = ins0 [[1,2,3],[5,6],[],[7,8,9,10]]

//ins02 :: [[Int]] -> [[Int]]

//Start = ins02 [[1,2,3],[5,6],[],[7,8,9,10]]



// 27. Delete the last element of each sublist of a list.
// E.g. for [[1,2,3],[5,6],[],[7,8,9,10]] the result is [[1,2],[5],[],[7,8,9]]
lastdel :: [[Int]] -> [[Int]]
lastdel [] = []
lastdel [x:xs] = [init x : lastdel xs]
// Start = lastdel [[1,2,3],[5,6],[],[7,8,9,10]]



// 28. write a funtion with the patterns depending on the parameter:
// if the param is [] then is equal to 20, if is a two element list starting with 4 then is 30
// if is a two element list ending with 5 then is 40, in all other cases is 50, 
// the order of the patterns is important
gp :: [Int] -> Int
gp [] = 20
gp [4, _] = 30
gp [_, 5] = 40
gp _ = 50

//Start = gp [4, 6] // 30
//Start = gp [4, 5] // 30
//Start = gp [1..10]



// 29. check if a number is palindrom e.g.12321
p :: Int -> [Int]
p 0 = []
p x = [x rem 10 : p (x / 10)]

// Start = p 12321 // [1,2,3,2,1]

//digits :: Int [Int] -> [Int]

//Start = digits 12321 []
// digits 12321 []
// digits 1232 [1:[]]
// digits 123 [2:[1:[]]]
// digits 12 [3:[2:[1:[]]]]
// digits 1  [2:[3:[2:[1:[]]]]]
// digits 0  [1:[2:[3:[2:[1:[]]]]]]
// digits 0 [1,2,3,2,1]
// [1,2,3,2,1]

pali :: Int -> Bool
pali x = p x == reverse (p x)
// Start = pali 12321 // True


//Start = pali 12321 // True
//Start = pali 12345


// 30. filter the elements smaller then n, e.g. n=3 [1,5,3,2,1,6,4,3,2,1] -> [1,2,1,2,1]
f7 :: Int [Int] -> [Int]
f7 n [] = []
f7 n [x:xs] 
| x < n = [x : f7 n xs]
| otherwise = f7 n xs
// f72 using filter 
f72 :: Int [Int] -> [Int]
f72 n xs = filter (\ x =x< n) xs


// Start = f72 3 [1,5,3,2,1,6,4,3,2,1] 



// 31. using notempty eliminate the empty lists: 
// [[1,2,3],[],[3,4,5],[2,2],[],[],[]] -> [[1,2,3], [3,4,5], [2,2]]
notempty :: [Int] -> Bool
notempty x = not (x == [])



f8 :: [[Int]] -> [[Int]]
f8 [] = []
f8 [x:xs]
| notempty x = [x : f8 xs]
| otherwise = f8 xs
Start = f8 [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
