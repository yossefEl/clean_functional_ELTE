module endterm2
import StdEnv

/*---------------------------------------------------------------
-- Functional Programming & end-term 
-- This solution was submitted and prepared by
-- <NAME, NEPTUN> for the end-term programming retake assignment of FP.
-- I declare that this solution is my own work.

-- I have not copied or used third-party solutions.

-- The most serious consequence of a disciplinary fault can be dismissal
-- of the student from the University.
*/

//----------------------------

/*1. Arrays. (10 points)
*
* Implement the 'shiftCipher' function which takes a string consists of English capital letters
* (A -> Z) and a positive integer number represents the number of shifts and does the following:
* For each character in the input string, it shifts this character by the given number.
* The set of English capital letters:
* {A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z}
* While shifting, if you reach the end of the set of English capital letters then you should
* start from the beginning, for example if we want to shift Z by 2 then the result is B.
* The alphabet has 26 letters, the ascii code of 'A' is 65.
* Example:
* Input : "HELLOWORLD" 3
* Output: "KHOORZRUOG"
* Explanation:
H shifted by 3 -> K, because K comes 3 positions after H in the set of English capital letters.
E shifted by 3 -> H, because H comes 3 positions after E in the set of English capital letters.
L shifted by 3 -> O, because O comes 3 positions after L in the set of English capital letters.
...
*/

//shiftCipher:: String Int -> String

//Start = shiftCipher "HELLOWORLD" 3 // "KHOORZRUOG"
//Start = shiftCipher "CLEANISFUN" 1 // "DMFBOJTGVO"
//Start = shiftCipher "FUNCTIONALPROGRAMMINGISGREATSUBJECT" 4 // "JYRGXMSREPTVSKVEQQMRKMWKVIEXWYFNIGX"
//Start = shiftCipher "THISISMYTEXT" 10 // "DRSCSCWIDOHD"
//Start = shiftCipher "HELLOWORLD" 0 // "HELLOWORLD"
//Start = shiftCipher "XXYYZZZ" 3 // "AABBCCC"

//----------------------------

:: Point ={x :: Real, y :: Real}
:: Circle ={center :: Point, radius :: Real}

broadcaster :: Circle
broadcaster = {center = {x = 1.5 ,y = 3.5}, radius = 2.0}

car_location :: Point
car_location ={x = 1.5, y = 3.5}
bus_location :: Point
bus_location ={x = 4.5, y = 5.2}
tool1_location :: Point
tool1_location ={x = -4.5, y = -25.2}
tool2_location :: Point
tool2_location ={x = 0.5, y = 0.0}
tool3_location :: Point
tool3_location ={x =0.0, y = 0.0}
tool4_location :: Point
tool4_location ={x =100.0, y = 200.0}

/*2. Records. (10 points)
*
* There is a broadcaster, check if it can be received its signal by
* the car, bus and expensive tools, the locations of which are known.
* The broadcaster is represented by a circle data type: its radius and location.
* The receivers are represented by array of points; we want to check
* if we can get signals, i.e. all are within the radius,
* the distance between all location points and the centre of the
* broadcaster is less than the radius.
* Hint: distance = square_root((x_2-x_1)^2+(y_2-y_1)^2) between any two points.
*/

//check :: Circle {Point} -> Bool

//Start = check broadcaster {car_location,bus_location} // True
//Start = check broadcaster {tool1_location,tool2_location} // False
//Start = check broadcaster {car_location,tool1_location,tool2_location,tool4_location} // True
//Start = check broadcaster {tool1_location,tool2_location,tool3_location,tool4_location} // False

//----------------------------

:: Library = {lib_name :: String , books :: {Book}}
:: Book= {title::String,author::String, pyear :: Int,num_of_pages::Int, can_be_borrowed::Bool}

b1::Book
b1 = {title = "C Programming Language", author = "Abel" , pyear =2022 , num_of_pages = 1501 , can_be_borrowed = False }
b2::Book
b2 = {title = "Functional Programming", author = "Andrey" , pyear =1999 , num_of_pages = 1250 , can_be_borrowed = True }
b3::Book
b3 = {title = "Java Programming Language", author = "John" , pyear =1508 , num_of_pages = 2980 , can_be_borrowed =True}
b4::Book
b4 = {title = "OOP Programming", author = "Peter" , pyear =2020 , num_of_pages = 280 , can_be_borrowed = False }
b5::Book
b5 = {title = "Programming", author = "James" , pyear =2000 , num_of_pages =1645 , can_be_borrowed =True}

lib1::Library
lib1 = {lib_name = "lib1" , books ={b1,b2}}
lib2::Library
lib2 = {lib_name = "lib2" , books ={b1,b2,b3}}
lib3::Library
lib3 = {lib_name = "lib3" , books ={b1,b2,b3,b4}}
lib4::Library
lib4 = {lib_name = "lib4" , books ={b1,b4,b5}}
lib5::Library
lib5 = {lib_name = "lib5" , books ={b1,b2,b3}}
lib6::Library
lib6 = {lib_name = "lib6" , books ={b4,b4,b2,b1}}

//----------------------------

/*3. Instances - Records. (20 points) Note this task has 4 parts each of 5 points!
*
* 3.1 Create an instance of '==' for the type Book. Two books are equal
* if they have the same title, author, publishing year and pages.
* (Whether the books can be borrowed does not matter when comparing them.)
*/

//==

//Start = b1 == b1 // True
//Start = b1 == b2 // False

//----------------------------

/* 3.2 Create an instance of '<' for the type Book. A book is smaller then
* another if the publishing year is smaller then second book's publishing year.
*/

//<

//Start = b1 < b2 // False
//Start = b3 < b2 // True

//----------------------------

/* 3.3 Write an instance of '+' for libraries, which unifies libraries' books,
* eliminate redundancies and arrange them according to the publication year.
* The name of new library is the concatenation of the 2 libraries' name.
*/

//+

//Start = lib1 + lib1
//(Library "lib1lib1" {(Book "Functional Programming" "Andrey" 1999 1250 True),(Book "C Programming Language" "Abel" 2022 1501 False)})
//Start = lib1 + lib2
//(Library "lib1lib2" {(Book "Java Programming Language" "John" 1508 2980 True),(Book "Functional Programming" "Andrey" 1999 1250 True),(Book "C Programming Language" "Abel" 2022 1501 False)})

//----------------------------

/* 3.4 Write '==' operator for 'Library' data type.
* Two libraries are equal if they have 'exactly' the same books.
*/

//==

//Start = lib1 == lib1 // True
//Start = lib3 == lib6 // False
//Start = lib1 == lib5 // False
//Start = and [li == li \\ li <- [lib1,lib2,lib3,lib4,lib5]] // True
//Start = lib3 == lib5 // False
//Start = lib2 == lib5 // True

//----------------------------

/*4. Instances - Arrays. (10 points)
*
* Write '+' operator for Arrays of strings.
* For each position i in the interval [0..(|Array_1| - 1)] do the following:
* Merge the strings in Array_1[i] and Array_2[i].
* where Array_1 and Array_2 are the two given arrays.
* Assume that the two given arrays of strings are of the same size and not empty.
* Example: {"hello","ABC","CD"}+{"World","AB","Abod"}->{"hWeolrllod","AABBC","CADbod"}
*/


// +

//Start :: {String} // this is needed at each start
//Start = {"hello","ABC","CD"} + {"World","AB","Abod"} //{"hWeolrllod","AABBC","CADbod"}
//Start = {"CleanIsFun","FunctionalProgramming","ELTE"} + {"Budapest","Abd","Mohido"}
//{"CBluedaanpIessFtun","FAubndctionalProgramming","EMLoThEido"}
//Start = {"A","BC","DEF"} + {"AS","F","HELLO"} //{"AAS","BFC","DHEEFLLO"}
//Start = {"123","4567","897","8934","32","656444"} + {"3234","556","890","890","890","890"}
//{"1322334","4555667","889970","8899304","38290","685960444"}

//----------------------------

/*5. Trees. (10 points)
*
* Given a binary search tree, check if this tree satisfies the AVL-tree property:
* the difference between the heights of the two children subtrees of any node is at most 1.
* Example:
5
/ \
4 6
/ => False
3
/
1
It doesn't satisfy the Avl-tree property because the difference between
the heights of the two children subtrees of 4 is > 1.
5
/ \
4 6
\
7 => True

It satisfies the Avl-property because for each node in the tree, the difference
between the left subtree's height and the right subtree's height is at most 1.
*/

:: Tree a = Node a (Tree a) (Tree a) | Leaf

tree1 = Node 5 (Node 4 Leaf Leaf) (Node 6 Leaf (Node 7 Leaf Leaf))
tree2 = Node 5 (Node 4 (Node 3 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 6 Leaf Leaf)
tree3 = Node 3 (Node 0 (Node -1 Leaf Leaf) (Node 1 Leaf Leaf)) tree1
tree4 = Node 15 (tree3) (Node 20 Leaf (Node 23 Leaf (Node 25 Leaf Leaf)))


//AVL_prop_check :: (Tree Int) -> Bool

//Start = AVL_prop_check tree1 // True
//Start = AVL_prop_check tree2 // False
//Start = AVL_prop_check tree3 // True
//Start = AVL_prop_check tree4 // False

//----------------------------

/*6. Trees. (10 points)
*
* Given a binary search tree, change the BST to a binary Tree such that a key
* of a node becomes original key plus sum of all greater keys in the given BST.
* Example: Input:
5
/ \
/ \
/ \
3 8
/ \ / \
/ \ / \
2 4 6 10

Output:
29
/ \
/ \
/ \
36 18
/ \ / \
/ \ / \
38 33 24 10
For example let's take 2 which got replaced with 38 in the output:
2 got replaced by 38 because the sum of all greater keys present in the BST
and the original number is 38: 2 + 3 + 4 + 5 + 6 + 8 + 10 = 38
3 got replaced with 36: 3 + 4 + 5 + 6 + 8 + 10 = 36
5 got replaced with 29: 5 + 6 + 8 + 10 = 29
...
*/

BsTree1 = (Node 5 (Node 3 (Node 2 Leaf Leaf) (Node 4 Leaf Leaf)) (Node 8 (Node 6 Leaf Leaf) (Node 10 Leaf Leaf) ))
BsTree2 = (Node 4 (Node 3 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 4 Leaf Leaf)) (Node 5 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf)))

//transform :: (Tree Int) -> (Tree Int)

//Start = transform BsTree1
//(Node 29 (Node 36 (Node 38 Leaf Leaf) (Node 33 Leaf Leaf)) (Node 18 (Node 24 Leaf Leaf) (Node 10 Leaf Leaf)))
//Start = transform BsTree2
//(Node 24 (Node 30 (Node 30 (Node 32 (Node 33 Leaf Leaf) Leaf) Leaf) (Node 24 Leaf Leaf)) (Node 16 (Node 16 Leaf Leaf) (Node 6 Leaf Leaf)))

//----------------------------

/*7. Arrays. (10 points)
*
* Given an Array of tuples, MULTIPLY the array tuples elementwise by
* the SMALLEST unique tuple from that array. There will be always a unique tuple!
a) Unique tuple is a tuple with unique elements.
(a,b,c) => All unique => Tuple is a unique tuple
(b,b,c) => Redundant element => tuple is not unique.
b) Smallest tuple is where the sum of its elements are the closest to 0.
Therefore, absolute value of the sum must be considered.
(-1,3,2) => Sum = 4
(-3,-1,-2) => sum = -6
(-1,3,2) => smallest unique tuple between the given 2 unique tuples.
c) Multiply the smalles unique tuple with the array elements.
(-1,3,2) {(-3,-1,-2), (-1,3,2)} => {(3,-3,-4), (1,9,4)}
*/

//alterArray :: {(Int, Int, Int)} -> {(Int, Int, Int)}

//Start = alterArray {(-1,3,2), (-3,-1,-2)} // {(1,9,4),(3,-3,-4)}
//Start = alterArray {(1,-1,0), (-1,3,2), (-3,-1,-2)} // {(1,1,0),(-1,-3,0),(-3,1,0)}
//Start = alterArray {(0,0,0),(1,-1,2), (-1,3,2), (-3,-1,-2)} // {(0,0,0),(1,1,4),(-1,-3,4),(-3,1,-4)}
//Start = alterArray {(0,0,0),(1,-1,4), (1,0,2)} // {(0,0,0),(1,0,8),(1,0,4)}

//----------------------------

/*8. Records - Arrays. (10 points)
*
* Given an array of tuples of a EngagedPerson type, return the engaged couples
* of which they have been engaged the longest (if sorted by year, the smallest year).
* Only the family names of the couples must be returned.
* Homer Simpson, Marge Simpson => "Simpson"
*/

:: EngagedPerson = {fullName :: String, yearOfMarriage :: Int}

Homer :: EngagedPerson
Homer = {fullName = "Homer Simpson", yearOfMarriage = 1999}
Marge :: EngagedPerson
Marge = {fullName = "Marge Simpson", yearOfMarriage = 1999}

Mike :: EngagedPerson
Mike = {fullName = "Mike Krabappel", yearOfMarriage = 2031}
Edna :: EngagedPerson
Edna = {fullName = "Edna Krabappel", yearOfMarriage = 2031}

Leon :: EngagedPerson
Leon = {fullName = "Leon Kennedy", yearOfMarriage = 2000}
Ada :: EngagedPerson
Ada = {fullName = "Ada Kennedy", yearOfMarriage = 2000}

//getOldestMarriage :: {(EngagedPerson, EngagedPerson)} -> String

//Start = getOldestMarriage { (Leon, Ada), (Mike, Edna)} // "Kennedy"
//Start = getOldestMarriage { (Leon, Ada), (Homer, Marge)} // "Simpson"
//Start = getOldestMarriage { (Leon, Ada), (Mike, Edna), (Homer, Marge)} // "Simpson"

//----------------------------

/*9. Rainbow Colors. (10 points)
*
* Write a function that when given a list of Color, returns their second
* right neighbour color of the rainbow, in circular order. 2nd neighbours are:
* Red->Yellow, Orange->Green, Yellow->Blue ... Indigo->Red, Violet->Orange
* (simply matching arguments is not accepted solution).
*/

:: Color = Red | Orange | Yellow | Green | Blue | Indigo | Violet

//neighbours :: [Color] -> [Color]

//Start = neighbours [Red,Orange,Yellow,Green,Blue,Indigo,Violet] //[Yellow,Green,Blue,Indigo,Violet,Red,Orange]
//Start = neighbours [Blue] //[Violet]
//Start = neighbours [] //[]

//----------------------------

/*10. Trees. (10 points)
*
* Given a BinaryTree structure, write a function `getBTDiameter`
* that takes a BinaryTree and calculates its diameter.
* The diameter of a binary tree is the length of the longest path
* between any two leaf nodes in a tree. This path may or may not pass
* through the root. The length of a path between two nodes is
* represented by the number of nodes between them.
*
* Ex.: this is illustration the task has no values in the nodes.
* 1
* / \
* 2 3
* / \
* 4 5
* / \ / \
* 6 7 8 9
* / \ / \
* 10 11 12 13
* In this tree leaf nodes are: 2, 7, 8, 10, 11, 12, 13
* Pairs and distances between them are:
* From 2 to 7 or 8 distance is 5
* From 2 to 10, 11, 12 or 13 distance is 6
* From 10 to 11 and from 12 to 13 distance is 3
* From 10 to 12, 10 to 13, 11 to 12, 11 to 13 distance is 7
* Hence the diameter of this tree is 7 (Maximum among above distances).
*/

:: BinaryTree2 = BTNode2 BinaryTree2 BinaryTree2 | BTLeaf2

bt21 = (BTNode2 BTLeaf2 (BTNode2 BTLeaf2 BTLeaf2))
bt22 = (BTNode2 (BTNode2 BTLeaf2 BTLeaf2) (BTNode2 BTLeaf2 BTLeaf2))
bt23 = (BTNode2 (BTNode2 bt22 bt21) (BTNode2 BTLeaf2 bt22))
bt24 = (BTNode2 (BTNode2 bt23 bt21) (BTNode2 BTLeaf2 bt22))

//getBTDiameter :: BinaryTree2 -> Int

//Start = getBTDiameter BTLeaf2 // 1
//Start = getBTDiameter bt21 // 4
//Start = getBTDiameter bt22 // 5
//Start = getBTDiameter bt23 // 9
//Start = getBTDiameter bt24 // 11

//----------------------------