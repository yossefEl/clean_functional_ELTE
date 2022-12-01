module ex_tree
import StdEnv

// Binary tree: https://www.geeksforgeeks.org/binary-tree-data-structure/
// Binary search tree: https://www.geeksforgeeks.org/tag/binary-tree/

// Examples
:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf

// Tree1 see link: http://graphonline.ru/en/?graph=RDODcKkbEjpzIbIh
Tree1 :: Tree Int
Tree1 = Node 7 Leaf Leaf

// Tree2 see link: http://graphonline.ru/en/?graph=apYgfCbqYeaQRHNL
Tree2 :: Tree Int
Tree2 = Node 0 (Node 1 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf))  (Node 2 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf)) 

// Tree3 see link: http://graphonline.ru/en/?graph=YMMkGtZycajcoXEU
Tree3 :: Tree Int
Tree3 = Node 0 (Node 1 (Node 3 Leaf (Node 8 Leaf Leaf)) Leaf)  (Node 2 Leaf Leaf)



// 1. Given a tree, find the number of its nodes (non leaves).
sizeT :: (Tree a) -> Int
sizeT Leaf = 0
sizeT (Node x le ri) = 1 + sizeT le + sizeT ri
//sizeT (Node _ le ri) = 1 + sizeT le + sizeT ri

//Node a l r ->a(value/key), l(left subtree->Tree a), r(right subtree->Tree a)
/*
=1 + sizeT (Node 1 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf)) 
   + sizeT (Node 2 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf))
=1 + 1 + sizeT (Node 3 Leaf Leaf) + sizeT (Node 4 Leaf Leaf)
   + 1 + sizeT (Node 5 Leaf Leaf) + sizeT (Node 6 Leaf Leaf)
=1 + 1 + (1 + sizeT Leaf + sizeT Leaf) + (1 + sizeT Leaf + sizeT Leaf)
   + 1 + (1 + sizeT Leaf + sizeT Leaf) + (1 + sizeT Leaf + sizeT Leaf)
=1 + 1 + (1+0+0) + (1+0+0)
   + 1 + (1+0+0) + (1+0+0)
=7
*/
//Start = sizeT Tree1
//Start = sizeT Tree2
//Start = sizeT Tree3



// 2. Given a tree, find its depth.
depth :: (Tree a) -> Int
depth Leaf = 0
depth (Node _ le ri) = (max (depth le) (depth ri)) + 1

//Start = depth Tree1 // 1
//Start = depth Tree2 // 3
//Start = depth Tree3 // 4

//Given a tree with key of type Int, find the sum of its nodes (leaf is 0)
sumT :: (Tree Int)->Int
sumT Leaf = 0
sumT (Node x le ri) = x + sumT le + sumT ri

//Start = sumT Tree1
//Start = sumT Tree2
//Start = sumT Tree3



// 3. Tree traversal (different ways of converting a given tree into a list):
// Create a tree and write the elements of it in 3 ways: 
// inorder, preorder, postorder

atree1 :: Tree Int
atree1 = Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)
//Start = atree1

atree2 :: Tree Int
atree2 = Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) Leaf
//Start = atree2


// a. Inorder: Left, Root, Right
inorder :: (Tree a) -> [a] 
inorder Leaf = []
inorder (Node x le ri) = inorder le ++ [x] ++ inorder ri

//Start = inorder atree1
// inorder: []++[1]++[]++[2]++[]++[3]++[] = [1,2,3]
//Start = inorder atree2
// inorder: []++[1]++[]++[2]++[]++[3]++[]++[4]++[] = [1,2,3,4]
//Start = inorder Tree3


// b. Preorder: Root, Left, Right
preorder :: (Tree a) -> [a] 
preorder Leaf = []
preorder (Node x le ri) = [x] ++ preorder le ++  preorder ri

//Start = preorder atree1
// preorder: [2]++[1]++[]++[]++[3]++[]++[] = [2,1,3]
//Start = preorder Tree2


// c. Postorder: Left, Right, Root
postorder :: (Tree a) -> [a] 
postorder Leaf = []
postorder (Node x le ri) = postorder le ++  postorder ri ++ [x]

//Start = postorder atree1
// postorder: []++[]++[1]++[]++[]++[3]++[2] = [1,3,2]
//Start = postorder Tree2



// 4. Given a (Tree Int), and a list of integers.
// Check if every element from the list is in the tree.

task1 :: (Tree Int) [Int] -> Bool
task1 x y = and [ isMember a z \\ a <- y] 
    where z = (inorder x) // z = sort (inorder x)

//Start = task1 Tree2 [1..4] // True
//Start = task1 Tree2 [1..10]// False
//Start = task1 Tree3 [1..10] // False



// 5. Given a (Tree Int) and an integer, 
// write a function which counts how many times
// the given number occurs in the tree.

Tree3fiveTimes :: Tree Int
Tree3fiveTimes = Node 3 (Node 3 Leaf (Node 3 Leaf (Node 2 Leaf Leaf))) (Node 3 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf))
//Start = Tree3fiveTimes 

task2 :: (Tree Int) Int-> Int
task2 x y = sum [1 \\ a <- (inorder x) | a==y]

task21 :: (Tree Int) Int-> Int
task21 Leaf y = 0
task21 (Node x le ri) y
| x==y = 1 + task21 le y + task21 ri y
= task21 le y + task21 ri y

//Start = task21 Tree3fiveTimes 3 // 5
//Start = task21 Tree2 (-10) // 0



// 6. Given a (Tree Int), write a function which gives back a list of triple tuples,
// where each tuple contains the value of the node, the left and the rigth child 
// of only the odd numbers from the tree in preorder traversal
// Leaf is considered to have value of -1.

task3 :: (Tree Int) -> [(Int,Int,Int)]
task3 Leaf = []
task3 (Node x le ri)
| isOdd x =  [(x, extractN le, extractN ri)] ++ task3 le ++ task3 ri
= task3 le ++ task3 ri

extractN :: (Tree Int) -> Int
extractN Leaf = -1
extractN (Node x le ri) = x

Start = task3 Tree2 // [(1,3,4),(3,-1,-1),(5,-1,-1)]
//Start = task3 Tree3 // [(1,3,-1),(3,-1,8)]



// 7. Given a (Tree Int) and an integer, write a function which
// searches the value in the tree. 

searchT :: (Tree Int) Int -> Bool
searchT Leaf n = False
searchT (Node x le ri) n
| x == n = True
=  searchT le n || searchT ri n

//Start = searchT Tree2 10 // False
//Start = searchT Tree2 1 // True

