module sample_endterm_todo

import StdEnv


 
//1.
//Define a tree type and use the followings for testing your solution.

tree1 = Node 10 (Node 7 (Node 3 Leaf Leaf) (Node 15 Leaf Leaf)) (Node 5 Leaf (Node 10 Leaf Leaf))
tree2 = Node 9 (Node 1 (Node 0 (Node 7 Leaf Leaf) Leaf) (Node 15 Leaf Leaf)) (Node 4 (Node 4561 Leaf Leaf) (Node 8 (Node 1663 Leaf Leaf) Leaf))
unitTree = Node 1337 Leaf Leaf
noTree = Leaf

//Start = tree2

:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf

//Write a function that takes a tree as a parameter and returns 
//a list of nodes which have at least one prime child.
//An empty tree will return [].

isPrime :: Int -> Bool
isPrime n =  length ([x \\ x <- [1..n] | n rem x == 0]) == 2

isPrimeChild :: (Tree Int) -> Bool
isPrimeChild Leaf = False
isPrimeChild (Node x l r) 
| isPrime x = True
=False

primeChildren :: (Tree Int) -> [Int]
primeChildren Leaf = []
primeChildren (Node v l r)
|( isPrimeChild l) || (isPrimeChild r) = [v ] ++ (primeChildren l) ++ (primeChildren r)
| otherwise = (primeChildren l) ++( primeChildren r)


// Start = primeChildren tree1 //[7,10]
// Start = primeChildren tree2 //[0,4,8]
//Start = primeChildren unitTree //[]
//Start = primeChildren noTree //[]


//2.
//Given a tuple of arrays, representing sets of integers, return a list containing the result of their symmetric-difference.
//The symmetric-difference between two sets is equivalent to the difference between their union and their intersection.


symmetricDiff :: ({Int}, {Int}) -> [Int]
symmetricDiff (a,b) = [x \\ x <- listA | not(isMember x listB)] ++ [x \\ x <- listB | not(isMember x listA)]
where
    listA =  [x \\ x<-:a]
    listB = [x \\ x<-:b]




// Start = symmetricDiff ({1,2,3,4},{3,4,5,6}) //[1,2,5,6]
// Start = symmetricDiff ({1,2,3,4},{-2,-4,13,0}) //[1,2,3,4,-2,-4,13,0]
//Start = symmetricDiff ({1,2,3,4},{1,2,3,4}) //[]
//Start = symmetricDiff ({1,2,3,4},{}) //[1,2,3,4]
//Start = symmetricDiff ({},{1,2,3,4}) //[1,2,3,4]
//Start = symmetricDiff ({},{}) //[]



//3.
//Define a Q type for rational numbers.
//Write a function that receives two fractions and calculates their division. 
//Simplify the fraction before returning.
//In case the nominator is zero, set the denominator to zero as well.
simplify :: Q -> Q
simplify {nom=n,den=d}
  | d == 0 = abort " denominator is 0"
  | d < 0  = { nom = ~n/g, den = ~d/g}
  | otherwise =  { nom = n/g, den = d/g}
  where g = gcd n d

:: Q = {nom :: Int, den :: Int}

instance (/) Q
    where
    (/) {nom=n1,den=d1} {nom=n2,den=d2} = simplify {nom=n1*d2,den=d1*n2}
fracDivision :: Q Q -> Q
fracDivision a b = a / b

Start = fracDivision {nom=5, den=1} {nom=6, den=5} //(Q 25 6)
// Start = fracDivision {nom=10, den=2} {nom=3, den=4} //(Q 20 3)
//Start = fracDivision {nom=0, den=2} {nom=100, den=4} //(Q 0 1)
//Start = fracDivision {nom=15, den=2} {nom=3, den=12} //(Q 30 1)

/*
half = { nom=1, den=2 }
third = { nom=1, den=3 }
fourth = { nom=1, den=4 }
fifth = { nom=1, den=5 }
sixth = { nom=1, den=6 }
threehalf = { nom=3, den=2 }
twothird = { nom=2, den=3 }
ninefourth = { nom=9, den=4 }
threefifth = { nom=3, den=5 }

miniTree = Node fifth (Node sixth Leaf Leaf)(Node third (Node fourth Leaf Leaf) Leaf)			
smallTree = Node half (Node fourth Leaf Leaf) (Node ninefourth Leaf Leaf)
bigTree = Node half (Node fifth (Node sixth Leaf Leaf)(Node third (Node fourth Leaf Leaf) Leaf))(Node threehalf (Node threefifth Leaf (Node twothird Leaf Leaf))(Node ninefourth Leaf Leaf))
badTree = Node third (Node fourth Leaf Leaf)(Node ninefourth (Node sixth Leaf Leaf) Leaf)
*/


//4.
//Write a function that will return the sum of a tree's nodes.
//Return the sum as a simplified Q.

//sumTree :: (Tree Q) -> Q

//Start = sumTree smallTree //(Q 3 1)
//Start = sumTree bigTree //(Q 97 15)
//Start = sumTree miniTree //(Q 19 20)



//5.
//Write a function that will check if a tree of Q is a Binary Search Tree.

//checkTree :: (Tree Q) -> Bool

//Start = checkTree bigTree //True
//Start = checkTree smallTree //True
//Start = checkTree badTree //False



:: Color = Red | Yellow | Green | Blue | Purple | Violet
:: ColorCombo = { color1 :: Color, color2 :: Color}

instance == Color
where
    (==) Red Red = True
    (==) Yellow Yellow = True
    (==) Green Green = True
    (==) Blue Blue = True
    (==) Purple Purple = True
    (==) Violet Violet = True
    (==) _ _ = False

colorList :: [Color]
colorList = [Red,Yellow,Green,Blue,Purple,Violet]



//6.
//Write a function that when given a color, returns its complement.
//That is:
//Red -> Blue, Yellow -> Purple, Green -> Violet, Blue -> Red, Purple -> Yellow, Violet -> Green

//colorComp :: Color -> Color

//Start = colorComp Red //Blue
//Start = colorComp Blue //Red
//Start = colorComp Green //Violet
//Start = colorComp Purple //Yellow



//7.
//Write a function that when given a Color, creates a list of possible color combos.
//Valid color combos can not have duplicate colors.

//colorCombo :: Color -> [ColorCombo]

//Start = colorCombo Red //[{color1=Red, color2=Yellow},{color1=Red, color2=Green},{color1=Red, color2=Blue},{color1=Red, color2=Purple},{color1=Red, color2=Violet}]
//Start = colorCombo Blue //[{color1=Blue, color2=Red},{color1=Blue, color2=Yellow},{color1=Blue, color2=Green},{color1=Blue, color2=Purple},{color1=Blue, color2=Violet}]



//8.
//Amicable numbers are two different numbers so related that the sum of the proper divisors of each 
//is equal to the other number. (A proper divisor of a number is a positive factor of that number other than the number itself. 
//For example, the proper divisors of 6 are 1, 2, and 3.) 
//Check if two integers are amicable pairs and put them together with the answer in a bag.
//amicable pair: 1184 and 1210 
//proper divisor of 1184 :  1, 2, 4, 8, 16, 32, 37, 74, 148, 296, 592 -> their sum == 1210
//proper divisor of 1210 : 1, 2, 5, 10, 11, 22, 55, 110, 121, 242, 605 -> their sum == 1184

//amiBag :: [(Int,Int)] -> Bag a

//Start = amiBag [(1184,1210), (13,245)]
//Start = amiBag [] // []
//all true
//Start = amiBag [(220, 284), (1184, 1210), (2620, 2924), (5020, 5564), (6232, 6368), (10744, 10856), (12285, 14595), (17296, 18416), (63020, 76084), (66928, 66992)]



//9.
//Given the Object type, compute for the state component the given method 
//and print the result as a String.
//ex: for state 3 compute 3 + 1 using the given method, 
//and print the result "4" as string.

//:: Object = { state::Int, method::Int->Int, tostring::Int -> String }

//MyObject :: Object
//MyObject = { state = 3, method = (+) 1, tostring = toString }

//f = ? 

//Start = f // "4". 



//10.
//Given an operator and two lists, apply the operator to the elements of 
//the same positions of lists, like in the examples.

:: Operator a :== a a -> a

//op2 :: (Operator a) [a] [a] -> [a]

//Start = op2 (*) [2,3,4,5] [7,8,9,10] // [14,24,36,50]
//Start = op2 (*) [2,3,4,5] [7,8] // [14,24]
//Start = op2 (*) [2,3] [7,8,9,10] // [14,24]
//Start :: [Int]
//Start = op2 (*) [] [] // []
//Start = op2 (+) [3,2] [7,8] // [10,10]