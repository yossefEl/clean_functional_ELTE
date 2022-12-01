module hw7
import StdEnv

/*
Name: Youssef ELMOUMEN
Neptun Code: XXXXXX
*/

:: Tree a = Node a (Tree a) (Tree a) | Leaf

/* Update the tree such that all the node values are the remainder of
the node value divided by the given integer. 

E.G: n = 3
	
	    07
	   /   \
	 02	    20
	 /\	    / \
	01 04  10 30 		

	so result:
	   1
	  / \
	 2	 2
 	/\	 /\
   1  1 1  0 */
   
tree1 = Node 7 ( Node 2 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf)) ( Node 20 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf))
						
tree2 = Node 5 ( Node 3 (Node 13 Leaf Leaf) (Node 11 Leaf Leaf)) ( Node 1 (Node 7 Leaf Leaf) (Node 9 Leaf Leaf))



updateTree :: (Tree Int) Int -> (Tree Int)
updateTree Leaf _ = Leaf
updateTree (Node v l r) n
|n==0 = (Node v l r)
= (Node (v rem n) (updateTree l n) (updateTree r n))


// Start = updateTree Leaf 2 // Leaf
// Start = updateTree tree1 4 // Node 3 ( Node 2 (Node 1 Leaf Leaf) (Node 0 Leaf Leaf)) ( Node 0 (Node 2 Leaf Leaf) (Node 2 Leaf Leaf))
// Start = updateTree tree2 2 // Node 1 ( Node 1 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf)) ( Node 1 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf))

/*
Define an instance of the built-in class ==
for Course. Courses are equal if they have same id
 and the difference between credits is less than 2.
*/

:: Uni = BME | ELTE | Deb | Pecs
:: Course = {id::String, uni :: Uni, credits:: Int}

Programming::Course
Programming = {id="CS1",uni=BME, credits =5}
Analysis::Course
Analysis = {id="CS2",uni=Pecs, credits =4}
Functional::Course
Functional = {id="CS0",uni=Pecs,credits=5}
Math::Course
Math = {id="CS2",uni=ELTE,credits=4}
Astronomy::Course
Astronomy = {id="PHSC3",uni=ELTE,credits=6}
Compilers::Course
Compilers = {id="CS3",uni=Deb,credits=7}
Hungarian::Course
Hungarian = {id="Basic1",uni=Pecs,credits=1}
HungarianBasic::Course
HungarianBasic = {id="Basic1",uni=Deb,credits=2}

instance == Course
where 
	(==) c1 c2 = c1.id == c2.id && abs(c1.credits - c2.credits) < 2


// Start = Analysis == Math // True
// Start = Hungarian == HungarianBasic // True
// Start = Programming == Functional // False
// Start = Hungarian == Compilers // False