implementation module Data.SetBy

import StdClass, StdMisc, StdBool, StdFunc, StdInt
from Data.GenLexOrd import :: LexOrd (..)
import Data.Monoid
from Data.Foldable import class Foldable (..)
import qualified StdList
from StdList import instance == [a]

/*
 * This function should only be used if the argument function preserves the ordering property of
 * the new set. 
 */
mapSetByMonotonic :: !(a -> b) !(SetBy a) -> SetBy b
mapSetByMonotonic _ TipBy = TipBy
mapSetByMonotonic f (BinBy n x l r) = BinBy n (f x) (mapSetByMonotonic f l) (mapSetByMonotonic f r)

/*
 * Sets are size balanced trees.
 * A set of values @a@.
 */
:: SetBy a = TipBy
           | BinBy !Int !a !(SetBy a) !(SetBy a)

isEqualBy :: !(a a -> Bool) !(SetBy a) !(SetBy a) -> Bool
isEqualBy comp s1 s2 = size s1 == size s2 && equalEltsBy comp (toAscList s1) (toAscList s2)
where
	equalEltsBy :: !(a a -> Bool) ![a] ![a] -> Bool
	equalEltsBy _ [] [] = True
	equalEltsBy _ [] _  = False
	equalEltsBy _ [_:_] [] = False
	equalEltsBy comp [a:as] [b:bs]
		| comp a b || comp b a = False
		| otherwise            = equalEltsBy comp as bs

isOrderedBy :: !(a a -> Bool) !(SetBy a) !(SetBy a) -> Bool
isOrderedBy comp s1 s2 = compare comp (toAscList s1) (toAscList s2)
where
	compare :: !(a a -> Bool) ![a] ![a] -> Bool
	compare _ []     [] = False
	compare _ []     _  = True
	compare _ [_:_]  [] = False
	compare comp [a:as] [b:bs]
		| comp a b     = True
		| comp b a     = False
		| otherwise    = compare comp as bs

lexOrdBy :: !(a a -> Bool) !(SetBy a) !(SetBy a) -> LexOrd
lexOrdBy comp s1 s2 = ordby comp (toAscList s1) (toAscList s2)
where
	ordby :: !(a a -> Bool) ![a] ![a] -> LexOrd
	ordby _ [] [] = EQ
	ordby _ [] _  = LT
	ordby _ [_:_] [] = GT
	ordby comp [a:as] [b:bs]
	| comp a b  = LT
	| comp b a  = GT
	| otherwise = ordby comp as bs

instance Foldable SetBy where
	foldr f z (BinBy _ x l r) = foldr f (f x (foldr f z r)) l
	foldr _ z _               = z

	foldr` f z (BinBy _ x l r) = foldr` f (f x (foldr` f z r)) l
	foldr` _ z _               = z

	foldl f z (BinBy _ x l r) = foldl f (f (foldl f z l) x) r
	foldl _ z _               = z

	foldl` f z (BinBy _ x l r) = foldl` f (f (foldl` f z l) x) r
	foldl` _ z _               = z

/*--------------------------------------------------------------------
 * Query
 *--------------------------------------------------------------------*/

memberBy :: !(a a -> Bool) !a !(SetBy a) -> Bool
memberBy comp x (BinBy _ y l r)
	| comp x y  = memberBy comp x l
	| comp y x  = memberBy comp x r
	| otherwise = True
memberBy _ _ _  = False

/*--------------------------------------------------------------------
 * Construction
 *--------------------------------------------------------------------*/
 
newSet :: SetBy a
newSet = TipBy

singleton :: !u:a -> w:(SetBy u:a), [w <= u]
singleton x = BinBy 1 x TipBy TipBy

/*--------------------------------------------------------------------
 * Insertion, Deletion
 *--------------------------------------------------------------------*/

insertBy :: !(a a -> Bool) !a !.(SetBy a) -> SetBy a
insertBy comp x (BinBy c y l r)
	| comp x y  = balanceL y (insertBy comp x l) r
	| comp y x  = balanceR y l (insertBy comp x r)
	| otherwise = BinBy c x l r
insertBy _ x _ = singleton x

deleteBy :: !(a a -> Bool) !a !.(SetBy a) -> SetBy a
deleteBy comp x (BinBy _ y l r)
	| comp x y  = balanceR y (deleteBy comp x l) r
	| comp y x  = balanceL y l (deleteBy comp x r)
	| otherwise = glue l r
deleteBy _ _ tip = tip

/*--------------------------------------------------------------------
 * Subset
 *--------------------------------------------------------------------*/

isSubsetOfXBy :: !(a a -> Bool) !(SetBy a) !(SetBy a) -> Bool
isSubsetOfXBy comp (BinBy _ x l r) t
	| t =: TipBy = False
	#! (lt, found, gt) = splitMemberBy comp x t
	= found && isSubsetOfXBy comp l lt && isSubsetOfXBy comp r gt
isSubsetOfXBy _ _ _ = True

/*--------------------------------------------------------------------
 * Minimal, Maximal
 *--------------------------------------------------------------------*/
 
findMin :: !(SetBy a) -> a
findMin (BinBy _ x TipBy _) = x
findMin (BinBy _ _ l _)     = findMin l
findMin TipBy               = abort "SetBy.findMin: empty set has no minimal element"

findMax :: !(SetBy a) -> a
findMax (BinBy _ x _ TipBy)  = x
findMax (BinBy _ _ _ r)      = findMax r
findMax TipBy                = abort "SetBy.findMax: empty set has no maximal element"

deleteMin :: !.(SetBy a) -> SetBy a
deleteMin (BinBy _ _ TipBy r) = r
deleteMin (BinBy _ x l r)     = balanceR x (deleteMin l) r
deleteMin TipBy               = TipBy

deleteMax :: !.(SetBy a) -> SetBy a
deleteMax (BinBy _ _ l TipBy) = l
deleteMax (BinBy _ x l r)     = balanceL x l (deleteMax r)
deleteMax TipBy               = TipBy

/*--------------------------------------------------------------------
 * Union. 
 *--------------------------------------------------------------------*/

unionBy :: !(a a -> Bool) !u:(SetBy a) !u:(SetBy a) -> SetBy a
unionBy _    t1 TipBy = t1
unionBy comp t1 (BinBy _ x TipBy TipBy) = insertBy comp x t1
unionBy comp (BinBy _ x TipBy TipBy) t2 = insertBy comp x t2
unionBy _    TipBy t2 = t2
unionBy comp t1=:(BinBy _ x l1 r1) t2 = link x l1l2 r1r2
where
	(l2,r2) = splitS comp x t2
	l1l2 = unionBy comp l1 l2
	r1r2 = unionBy comp r1 r2

splitS :: !(a a -> Bool) !a !(SetBy a) -> (!SetBy a, !SetBy a)
splitS _ _ TipBy = (TipBy,TipBy)
splitS comp x (BinBy _ y l r)
| comp x y  = let (lt,gt) = splitS comp x l in (lt, link y gt r)
| comp y x  = let (lt,gt) = splitS comp x r in (link y l lt, gt)
| otherwise = (l,r)

/*--------------------------------------------------------------------
 * Difference
 *--------------------------------------------------------------------*/
 
differenceBy :: !(a a -> Bool) !(SetBy a) !(SetBy a) -> SetBy a
differenceBy _ TipBy _   = TipBy
differenceBy comp t1 t2 =
	case t2 of
		BinBy _ x l2 r2 -> case splitBy comp x t1 of
			(l1, r1)
				| size l1l2 + size r1r2 == size t1 -> t1
				| otherwise -> merge l1l2 r1r2
			where
				l1l2 = differenceBy comp l1 l2
				r1r2 = differenceBy comp r1 r2
		_ -> t1

/*--------------------------------------------------------------------
 * Intersection
 *--------------------------------------------------------------------*/

intersectionsBy :: !(a a -> Bool) ![SetBy a] -> SetBy a
intersectionsBy _ [t] = t
intersectionsBy comp [t:ts] = 'StdList'.foldl (intersectionBy comp) t ts
intersectionsBy _ [] = abort "SetBy.intersectionsBy called with []\n"

intersectionBy :: !(a a -> Bool) !(SetBy a) !(SetBy a) -> SetBy a
intersectionBy _ TipBy _ = TipBy
intersectionBy _ _ TipBy = TipBy
intersectionBy comp t1 t2 = hedgeInt comp ?None ?None t1 t2

hedgeInt :: !(a a -> Bool) !(?a) !(?a) !(SetBy a) !(SetBy a) -> SetBy a
hedgeInt _ _ _ _   TipBy = TipBy
hedgeInt _ _ _ TipBy _   = TipBy
hedgeInt comp blo bhi (BinBy _ x l r) t2
	#! bmi = ?Just x
	#! l` = hedgeInt comp blo bmi l (trimBy comp blo bmi t2)
	#! r` = hedgeInt comp bmi bhi r (trimBy comp bmi bhi t2)
	= if (memberBy comp x t2) (link x l` r`) (merge l` r`)

/*--------------------------------------------------------------------
 * Filter and partition
 *--------------------------------------------------------------------*/

filter :: !(a -> Bool) !(SetBy a) -> SetBy a
filter p (BinBy _ x l r)
	| p x       = link x (filter p l) (filter p r)
	| otherwise = merge (filter p l) (filter p r)
filter _ tip = tip

partition :: !(a -> Bool) !(SetBy a) -> (!SetBy a, !SetBy a)
partition p (BinBy _ x l r)
  #! (l1,l2) = partition p l
  #! (r1,r2) = partition p r
  | p x       = (link x l1 r1,merge l2 r2)
  | otherwise = (merge l1 r1,link x l2 r2)
partition _ t = (t, t)

/*--------------------------------------------------------------------
 * Lists 
 *--------------------------------------------------------------------*/

fromListBy :: !(a a -> Bool) ![a] -> SetBy a
fromListBy comp xs = 'StdList'.foldl (ins comp) newSet xs
  where
  ins :: !(a a -> Bool) !(SetBy a) !a -> SetBy a
  ins comp t x = insertBy comp x t

/*--------------------------------------------------------------------
  Utility functions that return sub-ranges of the original
  tree. Some functions take a comparison function as argument to
  allow comparisons against infinite values. A function [cmplo x]
  should be read as [compare lo x].

  [trimBy comp cmplo cmphi t] A tree that is either empty or where [cmplo x == LT]
                              and [cmphi x == GT] for the value [x] of the root.
  [splitBy comp k t]          Returns two trees [l] and [r] where all values
                              in [l] are <[k] and all keys in [r] are >[k].
  [splitMemberBy comp k t]    Just like [splitBy] but also returns whether [k]
                              was found in the tree.
--------------------------------------------------------------------*/

/*--------------------------------------------------------------------
  [trimBy comp lo hi t] trims away all subtrees that surely contain no
  values between the range [lo] to [hi]. The returned tree is either
  empty or the key of the root is between @lo@ and @hi@.
--------------------------------------------------------------------*/

trimBy :: !(a a -> Bool) !(?a) !(?a) !(SetBy a) -> SetBy a
trimBy _ ?None ?None t = t
trimBy comp (?Just lx) ?None t = greater comp lx t
where
	greater comp lo (BinBy _ x _ r) | not (comp lo x) = greater comp lo r
	greater _ _  t` = t`
trimBy comp ?None (?Just hx) t = lesser comp hx t
where
	lesser comp hi (BinBy _ x l _) | not (comp x hi) = lesser comp hi l
	lesser _ _  t` = t`
trimBy comp (?Just lx) (?Just hx) t = middle comp lx hx t
where
	middle comp lo hi (BinBy _ x _ r) | not (comp lo x) = middle comp lo hi r
	middle comp lo hi (BinBy _ x l _) | not (comp x hi) = middle comp lo hi l
	middle _ _  _  t` = t`

/*--------------------------------------------------------------------
 * Split
 *--------------------------------------------------------------------*/

splitBy :: !(a a -> Bool) !a !(SetBy a) -> (!SetBy a, !SetBy a)
splitBy comp x (BinBy _ y l r)
	| comp x y
		#! (lt, gt) = splitBy comp x l
		= (lt, link y gt r)
	| comp y x
		#! (lt,gt) = splitBy comp x r
		= (link y l lt,gt)
	| otherwise = (l, r)
splitBy _ _ t = (t, t)

splitMemberBy :: !(a a -> Bool) !a !(SetBy a) -> (!SetBy a, !Bool, !SetBy a)
splitMemberBy comp x (BinBy _ y l r)
	| comp x y
		#! (lt, found, gt) = splitMemberBy comp x l
		= (lt, found, link y gt r)
	| comp y x
		#! (lt, found, gt) = splitMemberBy comp x r
		= (link y l lt, found, gt)
	| otherwise = (l, True, r)
splitMemberBy _ _ t = (t, False, t)

/*--------------------------------------------------------------------
  Utility functions that maintain the balance properties of the tree.
  All constructors assume that all values in [l] < [x] and all values
  in [r] > [x], and that [l] and [r] are valid trees.
  
  In order of sophistication:
    [BinBy sz x l r]  The type constructor.
    [bin x l r]       Maintains the correct size, assumes that both [l]
                      and [r] are balanced with respect to each other.
    [balance x l r]   Restores the balance and size.
                      Assumes that the original tree was balanced and
                      that [l] or [r] has changed by at most one element.
    [join x l r]      Restores balance and size. 

  Furthermore, we can construct a new tree from two trees. Both operations
  assume that all values in [l] < all values in [r] and that [l] and [r]
  are valid:
    [glue l r]        Glues [l] and [r] together. Assumes that [l] and
                      [r] are already balanced with respect to each other.
    [merge l r]       Merges two trees and restores balance.

  Note: in contrast to Adam's paper, we use (<=) comparisons instead
  of (<) comparisons in [join], [merge] and [balance]. 
  Quickcheck (on [difference]) showed that this was necessary in order 
  to maintain the invariants. It is quite unsatisfactory that I haven't 
  been able to find out why this is actually the case! Fortunately, it 
  doesn't hurt to be a bit more conservative.
--------------------------------------------------------------------*/

/*--------------------------------------------------------------------
 * Join 
 *--------------------------------------------------------------------*/
link :: !a !(SetBy a) !(SetBy a) -> SetBy a
link x l=:(BinBy sizeL y ly ry) r=:(BinBy sizeR z lz rz)
	| delta*sizeL < sizeR  = balanceL z (link x l lz) rz
	| delta*sizeR < sizeL  = balanceR y ly (link x ry r)
	| otherwise            = bin x l r
link x TipBy r  = insertMin x r
link x l _  = insertMax x l

// insertMin and insertMax don't perform potentially expensive comparisons.
insertMax :: !a !(SetBy a) -> SetBy a
insertMax x (BinBy _ y l r) = balanceR y l (insertMax x r)
insertMax x _ = singleton x

insertMin :: !a !(SetBy a) -> SetBy a
insertMin x (BinBy _ y l r) = balanceL y (insertMin x l) r
insertMin x _ = singleton x
         
/*--------------------------------------------------------------------
 * [merge l r]: merges two trees.
 *--------------------------------------------------------------------*/
merge :: !(SetBy a) !(SetBy a) -> SetBy a
merge l=:(BinBy sizeL x lx rx) r=:(BinBy sizeR y ly ry)
  | delta*sizeL < sizeR = balanceL y (merge l ly) ry
  | delta*sizeR < sizeL = balanceR x lx (merge rx r)
  | otherwise           = glue l r
merge TipBy r = r
merge l _ = l

/*--------------------------------------------------------------------
 * [glue l r]: glues two trees together.
 * Assumes that [l] and [r] are already balanced with respect to each other.
 *--------------------------------------------------------------------*/
glue :: !.(SetBy a) !.(SetBy a) -> SetBy a
glue TipBy r = r
glue l TipBy = l
glue l r
  | size l > size r
      #! (m, l`) = deleteFindMax l
      = balanceR m l` r
  | otherwise
      #! (m, r`) = deleteFindMin r
      = balanceL m l r`

deleteFindMin :: !.(SetBy a) -> (!a, !SetBy a)
deleteFindMin (BinBy _ x TipBy r) = (x, r)
deleteFindMin (BinBy _ x l r)
  #! (xm, l`) = deleteFindMin l
  = (xm, balanceR x l` r)
deleteFindMin TipBy = (abort "SetBy.deleteFindMin: can not return the minimal element of an empty set", TipBy)

deleteFindMax :: !.(SetBy a) -> (!a, !SetBy a)
deleteFindMax (BinBy _ x l TipBy ) = (x, l)
deleteFindMax (BinBy _ x l r)
  #! (xm, r`) = deleteFindMax r
  = (xm, balanceL x l r`)
deleteFindMax TipBy = (abort "SetBy.deleteFindMax: can not return the maximal element of an empty set", TipBy)

minView :: !.(SetBy a) -> . ?(!a, !SetBy a)
minView TipBy = ?None
minView x = ?Just (deleteFindMin x)

maxView :: !.(SetBy a) -> . ?(!a, !SetBy a)
maxView TipBy = ?None
maxView x = ?Just (deleteFindMax x)

/*--------------------------------------------------------------------
  [balance x l r] balances two trees with value x.
  The sizes of the trees should balance after decreasing the
  size of one of them. (a rotation).

  [delta] is the maximal relative difference between the sizes of
          two trees, it corresponds with the [w] in Adams' paper,
          or equivalently, [1/delta] corresponds with the $\alpha$
          in Nievergelt's paper. Adams shows that [delta] should
          be larger than 3.745 in order to garantee that the
          rotations can always restore balance.         

  [ratio] is the ratio between an outer and inner sibling of the
          heavier subtree in an unbalanced setting. It determines
          whether a double or single rotation should be performed
          to restore balance. It is correspondes with the inverse
          of $\alpha$ in Adam's article.

  Note that:
  - [delta] should be larger than 4.646 with a [ratio] of 2.
  - [delta] should be larger than 3.745 with a [ratio] of 1.534.
  
  - A lower [delta] leads to a more 'perfectly' balanced tree.
  - A higher [delta] performs less rebalancing.

  - Balancing is automatic for random data and a balancing
    scheme is only necessary to avoid pathological worst cases.
    Almost any choice will do in practice
    
  - Allthough it seems that a rather large [delta] may perform better 
    than smaller one, measurements have shown that the smallest [delta]
    of 4 is actually the fastest on a wide range of operations. It
    especially improves performance on worst-case scenarios like
    a sequence of ordered insertions.

  Note: in contrast to Adams' paper, we use a ratio of (at least) 2
  to decide whether a single or double rotation is needed. Allthough
  he actually proves that this ratio is needed to maintain the
  invariants, his implementation uses a (invalid) ratio of 1. 
  He is aware of the problem though since he has put a comment in his 
  original source code that he doesn't care about generating a 
  slightly inbalanced tree since it doesn't seem to matter in practice. 
  However (since we use quickcheck :-) we will stick to strictly balanced 
  trees.
--------------------------------------------------------------------*/
delta :== 4
ratio :== 2

// Functions balanceL and balanceR are specialised versions of balance.
// balanceL only checks whether the left subtree is too big,
// balanceR only checks whether the right subtree is too big.

// balanceL is called when left subtree might have been inserted to or when
// right subtree might have been deleted from.
balanceL :: !a !(SetBy a) !(SetBy a) -> SetBy a
balanceL x l r = case r of
	BinBy rs _ _ _ -> case l of
		BinBy ls lx ll lr
			| ls > delta*rs
				# (BinBy lls _   _   _  ) = ll
				# (BinBy lrs lrx lrl lrr) = lr
				| lrs < ratio*lls -> BinBy (1+ls+rs) lx ll (BinBy (1+rs+lrs) x lr r)
				| otherwise -> BinBy (1+ls+rs) lrx (BinBy (1+lls+size lrl) lx ll lrl) (BinBy (1+rs+size lrr) x lrr r)
			| otherwise -> BinBy (1+ls+rs) x l r
		_ -> BinBy (1+rs) x TipBy r

	_ -> case l of
			BinBy ls lx ll=:(BinBy lls _ _ _) lr=:(BinBy lrs lrx lrl lrr)
				| lrs < ratio*lls -> BinBy (1+ls) lx ll (BinBy (1+lrs) x lr TipBy)
				| otherwise -> BinBy (1+ls) lrx (BinBy (1+lls+size lrl) lx ll lrl) (BinBy (1+size lrr) x lrr TipBy)
			BinBy _ lx TipBy (BinBy _ lrx _ _)   -> BinBy 3 lrx (BinBy 1 lx TipBy TipBy) (BinBy 1 x TipBy TipBy)
			BinBy _ lx ll=:(BinBy _ _ _ _) TipBy -> BinBy 3 lx ll (BinBy 1 x TipBy TipBy)
			BinBy _ _ _ _ -> BinBy 2 x l TipBy
			_ -> BinBy 1 x TipBy TipBy

// balanceR is called when right subtree might have been inserted to or when
// left subtree might have been deleted from.
balanceR :: !a !(SetBy a) !(SetBy a) -> SetBy a
balanceR x l r = case l of
	BinBy ls _ _ _ -> case r of
		BinBy rs rx rl rr
			| rs > delta*ls
				# (BinBy rls rlx rll rlr) = rl
				# (BinBy rrs _   _   _  ) = rr
				| rls < ratio*rrs -> BinBy (1+ls+rs) rx (BinBy (1+ls+rls) x l rl) rr
				| otherwise -> BinBy (1+ls+rs) rlx (BinBy (1+ls+size rll) x l rll) (BinBy (1+rrs+size rlr) rx rlr rr)
			| otherwise -> BinBy (1+ls+rs) x l r
		_ -> BinBy (1+ls) x l TipBy

	_ -> case r of
		BinBy rs rx rl=:(BinBy rls rlx rll rlr) rr=:(BinBy rrs _ _ _)
			| rls < ratio*rrs -> BinBy (1+rs) rx (BinBy (1+rls) x TipBy rl) rr
			| otherwise -> BinBy (1+rs) rlx (BinBy (1+size rll) x TipBy rll) (BinBy (1+rrs+size rlr) rx rlr rr)
		BinBy _ rx TipBy rr=:(BinBy _ _ _ _) -> BinBy 3 rx (BinBy 1 x TipBy TipBy) rr
		BinBy _ rx (BinBy _ rlx _ _) TipBy -> BinBy 3 rlx (BinBy 1 x TipBy TipBy) (BinBy 1 rx TipBy TipBy)
		BinBy _ _ _ _ -> BinBy 2 x TipBy r
		_ -> BinBy 1 x TipBy TipBy

// rotate
rotateL :: !a !(SetBy a) !(SetBy a) -> SetBy a
rotateL x l r=:(BinBy _ _ ly ry)
  | size ly < ratio*size ry = singleL x l r
  | otherwise               = doubleL x l r
rotateL _ _ TipBy = abort "rotateL TipBy"

rotateR :: !a !(SetBy a) !(SetBy a) -> SetBy a
rotateR x l=:(BinBy _ _ ly ry) r
  | size ry < ratio*size ly = singleR x l r
  | otherwise               = doubleR x l r
rotateR _ TipBy _ = abort "rotateL TipBy"

// basic rotations
singleL :: !a !(SetBy a) !(SetBy a) -> SetBy a
singleL x1 t1 (BinBy _ x2 t2 t3)  = bin x2 (bin x1 t1 t2) t3
singleL _  _  TipBy               = abort "singleL"

singleR :: !a !(SetBy a) !(SetBy a) -> SetBy a
singleR x1 (BinBy _ x2 t1 t2) t3  = bin x2 t1 (bin x1 t2 t3)
singleR _  TipBy              _   = abort "singleR"

doubleL :: !a !(SetBy a) !(SetBy a) -> SetBy a
doubleL x1 t1 (BinBy _ x2 (BinBy _ x3 t2 t3) t4) = bin x3 (bin x1 t1 t2) (bin x2 t3 t4)
doubleL _ _ _ = abort "doubleL"

doubleR :: !a !(SetBy a) !(SetBy a) -> SetBy a
doubleR x1 (BinBy _ x2 t1 (BinBy _ x3 t2 t3)) t4 = bin x3 (bin x2 t1 t2) (bin x1 t3 t4)
doubleR _ _ _ = abort "doubleR"

/*--------------------------------------------------------------------
 * The bin constructor maintains the size of the tree
 *--------------------------------------------------------------------*/
//bin :: !a !(SetBy a) !(SetBy a) -> SetBy a
bin x l r :== BinBy (size l + size r + 1) x l r
