module ex_arr_rec_tree

import StdEnv


// 1. Define algebraic type : Day (Mon,Tue,Wed,Thu,Fri,Sat,Sun).
// And define function IsWeekend :: Day -> Bool to check if it is Sat or Sun.
// if it is weekend, then output "Happy day!", otherwise, "Oh noo".

:: Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

happy :: Day -> String
happy Sat = "Happy day!"
happy Sun = "Happy day!"
happy _ = "Oh noo"

//Start = happy Sun  // "Happy day!"
//Start = happy Tue  // "Oh noo"



// 2. Given a predefined Shape type, argument of the Circle constructor 
// is the radius, side length for Square, and equilateral Triangle, 
// width and height for Rectangle, write a function that calculates 
// the circumference area and circumference of each shape in the array, 
// store the results of each shape as a tuple like (area,circumference) 
// in an array.
//    			Circumference		Area
//    Circle			2*r*pi			r^2*pi		p=3.14
//    Square			4*a				a^2
//    Tiangle			3*a				sqrt(3)*a^2/4
//    Rectangle		2*a+2*b			a*b

:: Shape = Circle Real
        | Square Real
        | Triangle Real
        | Rectangle Real Real

cir :: Shape -> (Real, Real)
cir (Circle r) = (2.0*r*3.14,(r*r)*3.14)
cir (Square a) = (4.0*a,a*a)
cir (Triangle a) = (3.0*a,sqrt(3.0)*a^2.0/4.0)
cir (Rectangle a b) = (2.0*a+2.0*b,a*b)

calc :: {Shape} -> {(Real,Real)}
calc ar = {cir t \\ t<-: ar}

//Start = calc {(Circle 3.0), (Square 2.5)} 
// {(18.84,28.26),(10,6.25)}
//Start = calc {(Triangle 4.3), (Rectangle 5.4 7.2), (Circle 2.45)} 
// {(12.9,8.00640485798713),(25.2,38.88),(15.386,18.84785)}
//Start = calc {(Triangle 7.6), (Circle 1.75), (Square 0.95)} 
// {(22.8,25.0108136612946),(10.99,9.61625),(3.8,0.9025)}



// 3. Given an array of lists of integers and an integer, 
// keep the lists whose difference between max and min 
// element squared is greater than the given number
// There are no [] in the array.

cond1 :: [Int] Int -> Bool
cond1 ls n = (a-b)*(a-b) > n
where a = last (sort ls)
	  b = hd (sort ls)
	
minMaxDiff::{[Int]} Int->{[Int]}
minMaxDiff ar n = {t\\ t<-: ar | cond1 t n}

//Start = minMaxDiff {[1,21,2],[1,1,1,1,1],[1]} 5//{[1,21,2]}
//Start = minMaxDiff {[1,21],[1..10],[4,3]} 5//{[1,21],[1,2,3,4,5,6,7,8,9,10]}
//Start = minMaxDiff {[1..10],[5..6]} -3//{[1,2,3,4,5,6,7,8,9,10],[5,6]}



// 4. Given two Strings as parameters, remove all characters 
// of first string from the second one. Exampe: "z" "Pizza" -> "Pia"

remove_from_first_string :: String String -> String
remove_from_first_string sta stb = toString ls
where
	ls = [t \\ t<-: stb | not (isMember t la)]
	where la = [u\\ u<-: sta]
	
//Start = remove_from_first_string "z" "Zozo" // "Zoo"
//Start = remove_from_first_string "Xbc" "XccEcxacXmXs aXcrccXe hXaXccXbrXd"// "Exams are hard"
//Start = remove_from_first_string " " "Clean is the best"// "Cleanisthebest"
//Start = remove_from_first_string "" "It's a nice weather outside"// "It's a nice weather outside"
//Start = remove_from_first_string "" ""// ""



// 5. Given array find max of it and return new array which has 
//    all occurrences of maximum removed.
//	  E.g. {1,4,5,3,3,2,4,5,1,3,4} max is 5 -> {1,4,3,3,2,4,1,3,4}.

rem_max :: {Int} -> {Int}
rem_max ar = {t\\ t<-:ar | t <> n}
where n = last (sort [u \\ u<-: ar])

//Start = rem_max {1,4,5,3,3,2,4,5,1,3,4} //{1,4,3,3,2,4,1,3,4}
//Start = rem_max {1,42,42,52,452,4} // {1,42,42,52,4}
//Start = rem_max {5} // {}
//Start = rem_max {} // {}



// 6. Given two arrays, return new array such that i-th element of it is 
// maximum of i-th element of first and second arrays.
// E.g. when we calculate 5th element of result array, we look at 
// 5th element of first and 5th element of second arrays, and choose maximum of the two.
// You can assume that arrays have same length. 

maxOfTwo :: {Int} {Int} -> {Int}
maxOfTwo ara arb = {max a b \\ a<-: ara & b<-: arb }
//Start = maxOfTwo {} {} // {}
//Start = maxOfTwo {1} {5} // {5}
//Start = maxOfTwo {1,5,4} {2,3,6} // {2,5,6}
//Start = maxOfTwo {1,2,3,4,5} {1,2,3,4,5} // {1,2,3,4,5}



// 7. You are given array of integers.
// Your function should return true if each value appears at least twice 
// in the array, and it should return false
// if any element is distinct.

find :: Int {Int} -> [Int]
find i arr = [b\\ b <-: arr | b == i]

is2 :: Int {Int} -> Bool
is2 i arr = length (find i arr) >= 2

f7 :: {Int} -> Bool
f7 ar = and [is2 i ar \\ i <-: ar]

//Start = f7 {1,2,3,1,3,2,2,2} // True
//Start = f7 {1,2,3,4,3,2,1} // False
//Start = f7 {1,1,1,3,3,4,3,2,4,2} // True




// 8. An array is monotonic if it is either monotone increasing or 
// monotone decreasing
// A is monotone increasing if for all i<=j, A[i]<=A[j]
// A is monotone decreasing if for all i<=j, A[i]>=A[j]
// Given array, your task is to decide if it is monotonic.

isMonotonic :: {Int} -> Bool
isMonotonic arr = (l == x) || (l == reverse x)
where 
	l = [a \\ a <-: arr]
	x = sort l

//Start = isMonotonic {6,5,4,4} // True
//Start = isMonotonic {1,3,2} // False
//Start = isMonotonic {1,2,4,5} // True
//Start = isMonotonic {1,1,1} // True



:: Point = {  x       ::  Real
            , y       ::  Real
            , visible ::  Bool
            }

Origo :: Point
Origo = { x = 0.0
        , y = 0.0
        , visible = True
        }

// 9. Test about 3 points if they can form a right-angled triangle.

IsTriangle :: Point Point Point -> Bool
IsTriangle p1 p2 p3 = a == (b + c) || b == (a + c) || c == (a + b)
where
  a = (p2.x-p1.x)*(p2.x-p1.x) + (p2.y-p1.y)*(p2.y-p1.y)
  b = (p3.x-p2.x)*(p3.x-p2.x) + (p3.y-p2.y)*(p3.y-p2.y)
  c = (p3.x-p1.x)*(p3.x-p1.x) + (p3.y-p1.y)*(p3.y-p1.y)

//Start = IsTriangle Origo {x = 0.0, y = 3.0, visible = True} {x = 2.0, y = 0.0, visible = True}



// 10. Given a tree and an integer. Find all the nodes that are equal to the 
// integer and give the sum of their direct children. (Leaf count as 0).
exNode :: (Tree Int) -> Int
exNode Leaf = 0
exNode (Node x le ri) = x

f10 :: (Tree Int) Int -> Int
f10 Leaf n = 0
f10 (Node x le ri) n 
| x == n = exNode le + exNode ri + (f10 le n) + (f10 ri n)
= (f10 le n) + (f10 ri n)

//Start = f10 (Node 2 Leaf Leaf) 3 // 0
//Start = f10 (Node 3 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf)) 3 // 2
//Start = f10 (Node 1 (Node 0 Leaf Leaf)(Node 2 Leaf Leaf)) 1 // 2
//Start = f10 (Node 2 (Node 1 Leaf Leaf)(Node 2 (Node 3 Leaf Leaf) (Node 1 Leaf Leaf))) 2 // 7
//Start = f10 (Node 2 (Node 1 Leaf Leaf)(Node 2 Leaf (Node 1 Leaf Leaf))) 2 // 4



// 11. Given a tree and an integer n, find the nodes equal to n and 
// replace by 0.

:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf

replace :: Int (Tree Int) -> (Tree Int) 
replace n Leaf = Leaf
replace n (Node x le ri)
| n == x = (Node 0 (replace n le) (replace n ri))
= Node x (replace n le) (replace n ri) 

atree = Node 4 (Node 3 (Node 1 Leaf Leaf)(Node 3 Leaf Leaf)) (Node 6 (Node 3 Leaf Leaf)(Node 7 Leaf Leaf))

//Start = replace 3 atree  
//(Node 4 (Node 0 (Node 1 Leaf Leaf) (Node 0 Leaf Leaf)) (Node 6 (Node 0 Leaf Leaf) (Node 7 Leaf Leaf)))



// 12. Add "_over18" to the name of persons that are over age of 18 in a tree of persons. 

:: Person = { name::String
			, birthday::(Int,Int,Int)
	        }

t1::Tree Person
t1 = Node {name = "hh", birthday = (2001,11,22)} Leaf Leaf
t2::Tree Person
t2 = Node {name = "hh", birthday = (2005,11,22)} (Node {name = "hr", birthday = (2001,11,21)} Leaf Leaf)(Node {name = "ht", birthday = (2001,11,23)} Leaf Leaf)
t3::Tree Person
t3 = Node {name = "hh", birthday = (1999,11,22)} (Node {name = "hr", birthday = (2001,11,21)} (Node {name = "hh", birthday = (2003,11,22)} Leaf Leaf) (Node {name = "hh", birthday = (1998,11,22)} Leaf Leaf))(Node {name = "ht", birthday = (2005,11,23)} Leaf Leaf)

//Start = t1
//Start = t2
//Start = t3

extractNode :: (Tree a) -> a
extractNode (Node x l r) = x

over18 :: (Int,Int,Int) -> Bool
over18 (a,b,c) 
| a >= 2004 = False
= True

addString :: Person -> Person
addString a = {a & name = a.name +++ "_over18"}

//Start = ((extractNode t2).name) +++ "_over18"
//Start = addString {name = "hh", birthday = (2001,11,22)}

updateName :: (Tree Person) -> (Tree Person)
updateName Leaf = Leaf
updateName (Node x le ri)
| (over18 x.birthday) = Node (addString x) (updateName le) (updateName ri)
= Node x (updateName le) (updateName ri)

//Start = updateName t2 
//(Node (Person "hh" (2005,11,22)) 
//(Node (Person "hr_over18" (2001,11,21)) Leaf Leaf) 
//(Node (Person "ht_over18" (2001,11,23)) Leaf Leaf))

//Start = updateName t3 
//(Node (Person "hh_over18" (1999,11,22)) 
//(Node (Person "hr_over18" (2001,11,21)) 
//(Node (Person "hh_over18" (2003,11,22)) Leaf Leaf) 
//(Node (Person "hh_over18" (1998,11,22)) Leaf Leaf)) 
//(Node (Person "ht" (2005,11,23)) Leaf Leaf))



// 13. You are given a binary tree.
// Check if it is a binary search tree (BST).
// In BST values in left subtree should be 
// less then the current node's value and 
// values in right subtree should be greater.

:: BST a = BSTNode a (BST a) (BST a) | BSTLeaf

isBST :: (BST Int) -> Bool
isBST t = l == sort l
 where
     l = treeToList t

treeToList :: (BST a) -> [a]
treeToList BSTLeaf = []
treeToList (BSTNode x l r) = (treeToList l) ++ [x] ++ (treeToList r)

// For testing.
bst1 = (BSTNode 1 BSTLeaf (BSTNode 20 (BSTNode 3 (BSTNode 3 BSTLeaf BSTLeaf) (BSTNode 4 BSTLeaf (BSTNode 12 (BSTNode 5 BSTLeaf BSTLeaf) BSTLeaf))) (BSTNode 45 (BSTNode 34 (BSTNode 22 BSTLeaf BSTLeaf) BSTLeaf) (BSTNode 112 (BSTNode 53 BSTLeaf BSTLeaf) BSTLeaf))))
bst2 = (BSTNode 1 BSTLeaf (BSTNode 20 (BSTNode 7 BSTLeaf (BSTNode 12 (BSTNode 12 (BSTNode 9 BSTLeaf BSTLeaf) BSTLeaf) BSTLeaf)) BSTLeaf))
bst3 = (BSTNode 1 BSTLeaf (BSTNode 20 (BSTNode 3 (BSTNode 9 BSTLeaf BSTLeaf) (BSTNode 4 BSTLeaf (BSTNode 1 (BSTNode 8 BSTLeaf BSTLeaf) BSTLeaf))) (BSTNode 45 (BSTNode 34 (BSTNode 22 BSTLeaf BSTLeaf) BSTLeaf) (BSTNode 112 (BSTNode 53 BSTLeaf BSTLeaf) BSTLeaf))))
bst4 = (BSTNode 1 BSTLeaf (BSTNode 2 (BSTNode 7 BSTLeaf (BSTNode 12 (BSTNode 12 (BSTNode 8 BSTLeaf BSTLeaf) BSTLeaf) BSTLeaf)) BSTLeaf))

Start = map isBST [bst1,bst2,bst3,bst4,BSTLeaf] // [True,True,False,False,True]




