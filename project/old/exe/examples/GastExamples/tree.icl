module tree

import StdEnv
import Data.List
import Gast

:: Color = Red | Yellow | Blue
:: Tree a = Leaf | Node (Tree a) a (Tree a)

mirror :: (Tree a) -> Tree a
mirror Leaf = Leaf
mirror (Node l a r) = Node (mirror r) a (mirror l)

propMirror1 :: (Tree Color) -> Bool
propMirror1 t = mirror (mirror t) === t

propMirror2 :: (Tree Color) -> Property
propMirror2 t = not (symmetric t) ==> mirror t =!= t

symmetric Leaf = True
symmetric (Node l _ r) = l === r && symmetric l && symmetric r

Start =
	[ test propMirror1
	, test propMirror2
	]

derive gEq Color, Tree
derive genShow Color, Tree
derive gPrint Color, Tree
derive ggen Color, Tree
