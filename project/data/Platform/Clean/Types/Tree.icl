implementation module Clean.Types.Tree

import StdBool
import StdInt
import StdFunctions
import StdOrdList
import StdOverloadedList

import Clean.Types
import Clean.Types.Unify
import Clean.Types.Util
from Data.Func import $
import Data.GenLexOrd
import Data.Graphviz
import Data.List
import Data.Tuple
from Text import class Text(concat), instance Text String, <+
import Text.GenJSON

/* Each node in the tree (i.e. each type) contains a list of values (type
 * variable `v`) associated with that type. In Cloogle these are indexes for an
 * array of functions in the database. So functions that have the same type (up
 * to variable renaming) use the same node in the tree, and the list of values
 * contains multiple indexes to list all the isomorphic types. */

:: TypeTree v = Node !Type ![v] ![!TypeTree v!]

instance zero (TypeTree v) where zero = Node (Var "ra") [] [|]

instance < (TypeTree v) where (<) (Node a _ _) (Node b _ _) = a < b
derive gLexOrd Type, ?, TypeRestriction, TypeContext
instance < Type where (<) t u = (t =?= u) =: LT

derive JSONEncode TypeTree, Type, TypeRestriction, TypeContext
derive JSONDecode TypeTree, Type, TypeRestriction, TypeContext

typeTreeNodes :: !(TypeTree v) -> Int
typeTreeNodes (Node _ _ cs) = 1 + sum (MapM typeTreeNodes cs)

typeTreeSize :: !(TypeTree v) -> Int
typeTreeSize (Node _ vs cs) = length vs + sum (MapM typeTreeSize cs)

typeTreeDepth :: !(TypeTree v) -> Int
typeTreeDepth (Node _ _ cs) = maxList [0:MapM ((+) 1 o typeTreeDepth) cs]

/* When building the type tree, we start with a single node with type `a` (the
 * most general type). Then for each type *t* we walk through the tree to find
 * a place to add it, starting at the root as node *n*:
 *
 * 1. If *t* is more general than *n*, we replace *n* with *t* and add *n* as a
 *    child of *t*.
 * 2. If *t* is isomorphic to *n*, the nodes are merged (i.e. the new value is
 *    added to the list of values of *n*).
 * 3. Otherwise, *n* is more general than *t*. Then there are two cases:
 *    a. If there is a child *c* of *n* such that *c* generalises *t*, recurse
 *       with *n* = *c*.
 *    b. If there is no such child, *t* is added as a new child of *n*.
 *
 * In case 3a there may be multiple candidates for *c* (for instance,
 * `a -> Int` and `Int -> a` may already be in the tree, and `Int -> Int` is
 * added). In this case we always take the leftmost candidate. This makes sure
 * that the tree becomes slightly left-biased. By left-biasing the tree you are
 * more likely to find an isomorphic node (case 2), and this keeps the tree
 * small. */
addType :: !Type !Type !v !(TypeTree v) -> TypeTree v
addType tl tr v tree=:(Node n vs children)
| tl generalises` n
	/* The new type is more general than or isomorphic to the root of the
	 * tree... */
	| tl specialises` n =
		/* The new type is isomorphic to the root of the tree. Add the value
		 * to the list of values of this node (don't create a new node). */
		Node n [v:vs] children
	| otherwise =
		/* The new type is strictly more general than the root; create a new
		 * root with the old root as a child. */
		Node tr [v] [|tree]
| otherwise =
	/* The new type is more specific than the root. Try to find a child that
	 * generalises the new type and recurse, or create a new child node. */
	addToFirstGeneralisingNode [|] children
where
	addToFirstGeneralisingNode seen [|c=:(Node t` _ _):cs]
		| tl specialises` t` =
			/* We found a type that generalizes the new type. Recurse `addType`
			 * to add somewhere in its children structure. */
			Node n vs $ seen ++| [|addType tl tr v c:cs]
		| otherwise =
			/* Recurse to find a generalizing node. */
			addToFirstGeneralisingNode [|c:seen] cs
	addToFirstGeneralisingNode seen [|] =
		/* We could not find any generalising node. We create a new node for
		 * this type, and move children of the parent a level down if they are
		 * generalized by the new type. */
		Node n vs [|Node tr [v] yes : no]
	where
		(yes,no) = partition (\(Node t` _ _) -> tl generalises` t`) seen

findUnifying :: !Type !(TypeTree v) -> [(Type,Unifier,[v])]
findUnifying t tree=:(Node n ls cs) = case unify t n of
	?None      -> []
	?Just tvas -> [(n,finish_unification [] tvas,ls):[r \\ c <|- cs, r <- findUnifying t c]]

allTypes :: (TypeTree v) -> [(Type,[v],[!TypeTree v!])]
allTypes (Node t vs cs) = [(t,vs,cs):[t \\ c <|- cs, t <- allTypes c]]

allValues :: (TypeTree v) -> [v]
allValues (Node _ ls cs) = ls ++ [v \\ c <|- cs, v <- allValues c]

typeTreeToGraphviz :: !(TypeTree v) -> Digraph
typeTreeToGraphviz tree = Digraph
	"Type tree"
	[GAttRankDir RDLR]
	[NodeDef i []
		[NAttLabel $ concat $ print False n]
		[(c, []) \\ Node t` _ _ <|- cs, c <- [i \\ (t,_) <- nodes & i <- [0..] | t == t`]]
		\\ (n,cs) <- nodes & i <- [0..]]
	?None
where
	nodes = [(t,cs) \\ (t,vs,cs) <- allTypes tree | not (isEmpty vs) || not (IsEmpty cs)]
