module pt7
import StdEnv

/* Overload the built-in operators so that sort is usable on Tree type.
tree1 < tree2 if sum of all the node values of tree1 is less than tree2 */

:: Tree a = Node a (Tree a) (Tree a) | Leaf

btree = Node 4 (Node 2 (Node 1 Leaf Leaf)(Node 3 Leaf Leaf)) (Node 6 (Node 5 Leaf Leaf)(Node 7 Leaf Leaf))

ctree = Node 1 (Node 2 (Node 8 Leaf Leaf)(Node 9 (Node 4 (Node 16 Leaf Leaf) Leaf) Leaf)) (Node 7 (Node 3 Leaf Leaf)(Node 2 Leaf Leaf))

atree = Node 4 (Node 2 (Node 1 Leaf Leaf)(Node 3 Leaf Leaf)) (Node 6 (Node 3 Leaf Leaf)(Node 7 Leaf Leaf))


instance == Tree a
	 where
		(==) (Tree t1) (Tree t2) = t1 == t2
		(==)  (Node a t1 t2) (Node b u1 u2) = (a == b) && (t1 == u1) && (t2 == u2)
		(==)  Leaf Leaf = True
		(==)  _ _ = False



sumTree :: (Tree Int) -> Int
sumTree Leaf = 0
sumTree (Node v l r) = v + (sumTree l) + (sumTree r)

instance < (Tree Int) 
	 where
	 (<)  t1 t2 = (sumTree t1) < (sumTree t2)




sortTree :: [(Tree Int)] -> [(Tree Int)]
sortTree list = sort list



Start = sortTree [btree, ctree, atree] // [(Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 6 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf))),
											//(Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 6 (Node 5 Leaf Leaf) (Node 7 Leaf Leaf))),
											//(Node 1 (Node 2 (Node 8 Leaf Leaf) (Node 9 (Node 4 (Node 16 Leaf Leaf) Leaf) Leaf)) (Node 7 (Node 3 Leaf Leaf) (Node 2 Leaf Leaf)))]

//Start = sortTree [Leaf, btree] 