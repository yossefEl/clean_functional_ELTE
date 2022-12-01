module hw8
import StdEnv




/*
Neptun Code : XXXXXX
Name		: Youssef ELMOUMEN

Given a binary search tree, change it into a sum tree.
Sum tree is a tree where each node is equal to its value + sum of all the nodes
that are greater in value. () -> represents the new value of the nodes

		4(30)				
	   / \				
 (36) 1   6	(21)				
	 / \ / \ 				
(36)0  2 5  7 (15)
        \ (26)\
   (33) 3     8 (8)
 */


// [4,1,0,0,0,2,0,3,0,0,6,5,0,0,7,0,8,0,0]

:: Tree a = Node a (Tree a) (Tree a) | Leaf
tree1 = (Node 4 (Node 1 (Node 0 Leaf Leaf) (Node 2 Leaf (Node 3 Leaf Leaf)))(Node 6 (Node 5 Leaf Leaf) (Node 7 Leaf (Node 8 Leaf Leaf))))
tree2 = (Node 0 Leaf (Node 1 Leaf Leaf))
tree3 = (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf (Node 4 Leaf Leaf)))

extractNodeValues :: (Tree Int) -> [Int]
extractNodeValues Leaf =[0]
extractNodeValues (Node v l r) = [v] ++  (extractNodeValues l )++ (extractNodeValues r)

getSumTree :: (Tree Int) (Tree Int) -> (Tree Int)
getSumTree (Node valOr leftOr rightOr) Leaf = Leaf
getSumTree (Node valOr leftOr rightOr) (Node v l r) = (Node sm left right) // node node
where 
	sm 		= sum [value \\ value<- extractNodeValues (Node valOr leftOr rightOr) | value >= v]
	left	= getSumTree (Node valOr leftOr rightOr) l 
	right	= getSumTree (Node valOr leftOr rightOr) r


BSTtoSumTree :: (Tree Int) -> (Tree Int)
BSTtoSumTree Leaf = Leaf
BSTtoSumTree (Node v l r) =  getSumTree (Node v l r) (Node v l r)

Start = BSTtoSumTree tree1 // (Node 30 (Node 36 (Node 36 Leaf Leaf) (Node 35 Leaf (Node 33 Leaf Leaf)))(Node 21 (Node 26 Leaf Leaf) (Node 15 Leaf (Node 8 Leaf Leaf))))
// Start = BSTtoSumTree tree2 // (Node 1 Leaf (Node 1 Leaf Leaf))
// Start = BSTtoSumTree tree3 // (Node 9 (Node 10 Leaf Leaf) (Node 7 Leaf (Node 4 Leaf Leaf)))

/* Word is type synonym of String.Define an operator <==>, and create an instance for Words which returns True 
if all the following conditions hold: 
	the number of consonants in both words are the same.
	the vowels in the words are the same.
	the number of upper and lower letter are equal.
*/

stringToArray :: Word -> [Char]
stringToArray string = [toChar x \\ x <-: string]

isVowel :: Char -> Bool
isVowel c = isMember c ['a','e','i','o','u','A','E','I','O','U','y','Y']

isConsonant :: Char -> Bool
isConsonant c = not (isVowel c)

::Word :== String

class <==> a 
	where
		(<==>) :: a a -> Bool

instance <==> Word 
	where (<==>) w1 w2 = (length (filter isVowel (stringToArray w1)) == length (filter isVowel (stringToArray w2))) &&
				(length (filter isConsonant (stringToArray w1)) == length (filter isConsonant (stringToArray w2))) &&
				(length (filter isUpper (stringToArray w1)) == length (filter isUpper (stringToArray w2))) &&
				(length (filter isLower (stringToArray w1)) == length (filter isLower (stringToArray w2)))


// Start = ["saah" <==> "sarah", "bOris" <==> "Boris", "functional" <==> "Functional", "abcde" <==> "abco", "haPPy" <==> "pLaYz"] // [False, True, False, False, True]
