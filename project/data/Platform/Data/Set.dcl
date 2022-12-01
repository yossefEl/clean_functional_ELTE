definition module Data.Set

/**
 * An efficient implementation of sets.
 *
 * The implementation of `Set` is based on size balanced binary trees (or trees
 * of bounded balance) as described by:
 *
 * - Stephen Adams, "Efficient sets: a balancing act",
 *	 Journal of Functional Programming 3(4):553-562, October 1993,
 *	 <http://www.swiss.ai.mit.edu/~adams/BB/>.
 *
 * - J. Nievergelt and E.M. Reingold, "Binary search trees of bounded balance",
 *	 SIAM journal of computing 2(1), March 1973.
 *
 * Note that the implementation is left-biased -- the elements of a first
 * argument are always preferred to the second, for example in {{`union`}} or
 * {{`insert`}}. Of course, left-biasing can only be observed when equality is
 * an equivalence relation instead of structural equality.
 *
 * This module is ported from Haskell's Data.Set (copyright Daan Leijen 2002)
 * by L�szl� Domoszlai, 2013.
 *
 * @property-bootstrap
 *     import StdBool, StdChar, StdInt, StdOrdList, StdTuple
 *     from StdList import ++, all, isMember, removeDup, reverse,
 *         instance == [a], instance length []
 *     import qualified StdList
 *     from Data.Func import on, `on`, $
 *     import Data.GenLexOrd
 *     import Data.Maybe
 *
 *     derive genShow ?
 *     genShow{|Set|} show sep p xs rest = ["Set{":showList (toList xs) ["}":rest]]
 *     where
 *         showList [x]    rest = show sep False x rest
 *         showList [x:xs] rest = show sep False x [",":showList xs rest]
 *         showList []     rest = rest
 *
 *     :: Predicate a = ConstTrue | IsMember [a]
 *
 *     pred :: (Predicate a) a -> Bool | Eq a
 *     pred ConstTrue     _ = True
 *     pred (IsMember cs) c = isMember c cs
 *
 *     derive class Gast Predicate
 *     derive gPrint ?, Set
 *
 *     // Check that all elements from a list are in a set.
 *     contains :: (Set a) [a] -> Bool | < a
 *     contains d xs = all (\x -> member x d) xs
 *
 *     // Check that no elements from a list are in a set.
 *     does_not_contain :: (Set a) [a] -> Bool | < a
 *     does_not_contain d ys = all (\y -> notMember y d) ys
 *
 *     // Check that all elements from a set are in a list.
 *     all_in :: (Set a) [a] -> Bool | Eq a
 *     all_in s xs = all (\e -> isMember e xs) (toList s)
 *
 *     lazyList :: ![a] -> [a]
 *     lazyList x = x
 *
 * @property-test-with a = Char
 *
 * @property-test-generator [a] -> Set a | < a
 *     gen xs = fromList xs
 */
import StdStrictLists
from StdEnv import class List, instance < Char, instance < Real
from StdOverloaded import class ==, class < (..)
from StdClass import class Ord (..), <=, >
from StdList import map
from StdBool import not, &&
from StdOverloadedList import Foldl
from Data.GenEq import generic gEq
from Data.GenLexOrd import generic gLexOrd, :: LexOrd
import qualified Data.Foldable
from Data.Foldable import class Foldable

/**
 * A `Set a` is an unordered, uncounted collection of values of type `a`.
 *
 * Like with keys in `Map`, avoid tuple elements since this can be slow.
 * Instead, define a (local) record with strict fields where appropriate.
 *
 * @invariant integrity: A.s :: Set a | Eq, genShow{|*|}, gPrint{|*|} a:
 *     // Check that the data structure is still correct.
 *     name "no_duplicates" (no_duplicates s) /\
 *     name "balanced"      (balanced s) /\
 *     name "sizes_correct" (sizes_correct s)
 *
 * @invariant no_duplicates: A.s :: Set a | Eq, genShow{|*|}, gPrint{|*|} a:
 *     xs =.= removeDup xs where xs = toList s
 *
 * @invariant balanced: A.s :: Set a:
 *     case s of
 *         Tip -> prop True
 *         Bin _ _ l r -> abs (depth l - depth r) < 2 /\ balanced l /\ balanced r
 *     where
 *         depth :: (Set a) -> Int
 *         depth Tip = 0
 *         depth (Bin _ _ l r) = 1 + (max `on` depth) l r
 *
 * @invariant sizes_correct: A.s :: Set a:
 *     case s of
 *         Tip              -> prop True
 *         b=:(Bin _ _ l r) ->
 *             size b =.= 1 + size l + size r /\
 *             sizes_correct l /\
 *             sizes_correct r
 */
:: Set a = Tip
         | Bin !Int !a !(Set a) !(Set a)

instance == (Set a) | == a
instance < (Set a) | < a
derive gEq Set
derive gLexOrd Set
instance Foldable Set

/**
 * True iff this is the empty set.
 * @type (Set a) -> Bool
 * @property equivalence with size 0: A.s :: Set a:
 *     size s == 0 <==> null s
 * @property equivalence with newSet: A.s :: Set a:
 *     s == newSet <==> null s
 */
null s :== case s of
             Tip -> True
             (Bin sz _ _ _) -> False

/**
 * The number of elements in the set.
 * @type (Set a) -> Int
 * @property correctness: A.s :: Set a:
 *     size s =.= length (lazyList $ toList s)
 */
size s :== case s of
             Tip -> 0
             (Bin sz _ _ _) -> sz

/**
 * Is the element in the set?
 * @complexity O(log n)
 * @property correctness: A.x :: a; s :: Set a:
 *     member x s <==> isMember x (toList s)
 */
member    :: !a !(Set a) -> Bool | < a
	special a=Int; a=String

/**
 * Checks if an element is not in the set.
 * @complexity O(log n)
 * @type a (Set a) -> Bool | <, == a
 */
notMember x t :== not (member x t)

/**
 * Finds an element in a set.
 *
 * This function is for example useful to increase sharing of heap objects.
 * Suppose we have a group of values among which there are many duplicate
 * values but they are in different nodes on the heap. We can then build a
 * `Set` of these values, and replace every value in the group with the value
 * from the `Set`. After this procedure, all values in the group point to the
 * same node (the one used in the `Set`) and all other nodes can be garbage
 * collected.
 *
 * @complexity O(log n)
 * @property correctness: A.x :: a; s :: Set a:
 *     (member x s ==> getMember x s =.= ?Just x) /\
 *     (not (member x s) ==> isNone (getMember x s))
 */
getMember :: !a !(Set a) -> ?a | < a
	special a=Int; a=String

/**
 * Is t1 a subset of t2?
 * @type (Set a) (Set a) -> Bool | <, == a
 * @complexity O(n+m)
 * @property correctness: A.xs :: Set a; ys :: Set a:
 *     isSubsetOf xs ys <==> all (\x -> isMember x (toList ys)) (toList xs)
 */
isSubsetOf t1 t2 :== (size t1 <= size t2) && (isSubsetOfX t1 t2)

isSubsetOfX :: !(Set a) !(Set a) -> Bool | < a
	special a=Int; a=String

/**
 * Is t1 a proper subset of t2?
 * @type (Set a) (Set a) -> Bool | <, == a
 * @complexity O(n+m)
 * @property correctness: A.xs :: Set a; ys :: Set a:
 *     isProperSubsetOf xs ys
 *         <==> all (\x -> isMember x ys`) xs` && not (all (\y -> isMember y xs`) ys`)
 *     where
 *         (xs`,ys`) = (toList xs, toList ys)
 */
isProperSubsetOf s1 s2 :== (size s1 < size s2) && (isSubsetOf s1 s2)

/**
 * The empty set.
 * @complexity O(1)
 * @property is null:
 *     null newSet
 */
newSet :: Set a

/**
 * Create a singleton set.
 * @complexity O(1)
 */
singleton :: !u:a -> w:(Set u:a), [w <= u]

/**
 * Insert an element in a set. If the set already contains an element equal to
 * the given value, it is replaced with the new value.
 *
 * @complexity O(log n)
 * @property correctness: A.x :: a; xs :: Set a:
 *     let xs` = insert x xs in
 *         check member x xs` /\             // Membership
 *         check contains xs` (toList xs) /\ // Rest untouched
 *         integrity xs`                     // Data structure integrity
 */
insert :: !a !.(Set a) -> Set a | < a
	special a=Int; a=String

/**
 * Delete an element from a set.
 *
 * @complexity O(log n)
 * @property correctness: A.x :: a; xs :: Set a:
 *     let xs` = delete x xs in
 *         check notMember x xs` /\                                // Membership
 *         check contains xs` [x` \\ x` <- toList xs | x` <> x] /\ // Rest untouched
 *         integrity xs`                                           // Data structure integrity
 */
delete :: !a !.(Set a) -> Set a | < a
	special a=Int; a=String

/**
 * The minimal element of a set.
 * @complexity O(log n)
 * @property correctness: A.xs :: Set a:
 *     minList (toList xs) == findMin xs
 * @precondition not (null xs)
 */
findMin :: !(Set a) -> a

/**
 * The maximal element of a set.
 * @complexity O(log n)
 * @property correctness: A.xs :: Set a:
 *     maxList (toList xs) == findMax xs
 * @precondition not (null xs)
 */
findMax :: !(Set a) -> a

/**
 * Delete the minimal element.
 * @complexity O(log n)
 * @property correctness: A.xs :: Set a:
 *     case xs of
 *         Tip -> prop (null (deleteMin newSet))
 *         xs  -> let xs` = deleteMin xs in
 *             check notMember (minList (toList xs)) xs` /\
 *             size xs` =.= size xs - 1 /\
 *             integrity xs`
 */
deleteMin :: !.(Set a) -> Set a

/**
 * Delete the maximal element.
 * @complexity O(log n)
 * @property correctness: A.xs :: Set a:
 *     case xs of
 *         Tip -> prop (null (deleteMax newSet))
 *         xs  -> let xs` = deleteMax xs in
 *             check notMember (maxList (toList xs)) xs` /\
 *             size xs` =.= size xs - 1 /\
 *             integrity xs`
 */
deleteMax :: !.(Set a) -> Set a

/**
 * deleteFindMin set = (findMin set, deleteMin set)
 * @complexity O(log n)
 * @property correctness: A.xs :: Set a:
 *     let (m,xs`) = deleteFindMin xs in
 *         m =.= min /\ notMember min xs`
 *     where min = minList (toList xs)
 * @precondition not (null xs)
 */
deleteFindMin :: !.(Set a) -> (!a, !Set a)

/**
 * deleteFindMax set = (findMax set, deleteMax set)
 * @complexity O(log n)
 * @property correctness: A.xs :: Set a:
 *     let (m,xs`) = deleteFindMax xs in
 *         m =.= max /\ notMember max xs`
 *     where max = maxList (toList xs)
 * @precondition not (null xs)
 */
deleteFindMax :: !.(Set a) -> (!a, !Set a)

/**
 * Retrieves the minimal key of the set, and the set stripped of that element,
 * or `?None` if passed an empty set.
 * @complexity O(log n)
 * @property correctness: A.xs :: Set a:
 *     if (null xs)
 *         (?None =.= minView xs)
 *         (?Just (deleteFindMin xs) =.= minView xs)
 */
minView :: !.(Set a) -> . ?(!a, !Set a)

/**
 * Retrieves the maximal key of the set, and the set stripped of that element,
 * or `?None` if passed an empty set.
 * @complexity O(log n)
 * @property correctness: A.xs :: Set a:
 *     if (null xs)
 *         (?None =.= maxView xs)
 *         (?Just (deleteFindMax xs) =.= maxView xs)
 */
maxView :: !.(Set a) -> . ?(!a, !Set a)

/**
 * The union of two sets, preferring the first set when equal elements are
 * encountered.
 * @complexity O(n+m)
 * @property correctness: A.xs :: Set a; ys :: Set a:
 *     check contains u elems    // No missing elements
 *         /\ check all_in u elems // No junk
 *         /\ integrity u          // Data structure integrity
 *     where
 *         u = union xs ys
 *         elems = toList xs ++ toList ys
 */
union :: !u:(Set a) !u:(Set a) -> Set a | < a
	special a=Int; a=String

/**
 * The union of a list of sets.
 * @type !u:(l v:(Set a)) -> Set a | < a & List l (Set a), [u <= v]
 */
unions ts :== Foldl union newSet ts

/**
 * Difference of two sets.
 * @complexity O(n+m)
 * @property correctness: A.xs :: Set a; ys :: Set a:
 *     check does_not_contain d ys`                                 // No remaining elements
 *         /\ check contains d [x \\ x <- xs` | not (isMember x ys`)] // All good elements
 *         /\ integrity d                                             // Data structure integrity
 *     where
 *         d = difference xs ys
 *         (xs`,ys`) = (toList xs, toList ys)
 */
difference :: !(Set a) !(Set a) -> Set a | < a
	special a=Int; a=String

/**
 * The intersection of two sets.
 * Elements of the result come from the first set.
 * @complexity O(n+m)
 * @property correctness: A.xs :: Set a; ys :: Set a:
 *     check does_not_contain i [x \\ x <- xs` | not (isMember x ys`)]      // No junk
 *         /\ check does_not_contain i [y \\ y <- ys` | not (isMember y xs`)] // No junk
 *         /\ check contains i [x \\ x <- xs` | isMember x ys`]               // All good elements
 *         /\ integrity i                                                   // Data structure integrity
 *     where
 *         i = intersection xs ys
 *         (xs`,ys`) = (toList xs, toList ys)
 */
intersection :: !(Set a) !(Set a) -> Set a | < a
	special a=Int; a=String

/**
 * The intersection of a list of sets.
 * Elements of the result come from the first set
 */
intersections :: !(l (Set a)) -> Set a | < a & List l (Set a)
	special
		l = [], a = Int; l = [], a = String; l = [], a = Char; l = [], a = Real;
		l = [!], a = Int; l = [!], a = String; l = [!], a = Char; l = [!], a = Real;
		l = [!!], a = Int; l = [!!], a = String; l = [!!], a = Char; l = [!!], a = Real;
		l = [ !], a = Int; l = [ !], a = String; l = [ !], a = Char; l = [ !], a = Real

/**
 * Filter all elements that satisfy the predicate.
 * @complexity O(n)
 * @property correctness: A.p :: Predicate a; xs :: Set a:
 *     sort (toList (filter (pred p) xs))
 *         =.= sort (removeDup ('StdList'.filter (pred p) (toList xs)))
 */
filter :: !(a -> Bool) !(Set a) -> Set a

/**
 * Partition the set into two sets, one with all elements that satisfy the
 * predicate and one with all elements that don't satisfy the predicate.
 * See also {{`split`}}.
 *
 * @complexity O(n)
 * @property correctness: A.p :: Predicate a; xs :: Set a:
 *     all p` true`                                // Right split
 *         /\ all (\x -> not (p` x)) false`
 *         /\ all (\x -> isMember x (toList xs)) xs` // No junk
 *         /\ all (\x -> isMember x xs`) (toList xs) // All members used
 *         /\ integrity true                         // Data structure integrity
 *         /\ integrity false
 *     where
 *         p` = pred p
 *         (true,false) = partition p` xs
 *         (true`,false`) = (toList true, toList false)
 *         xs` = true` ++ false`
 */
partition :: !(a -> Bool) !(Set a) -> (!Set a, !Set a)

/**
 * Split a set in elements less and elements greater than a certain pivot.
 *
 * @param The pivot.
 * @param The set.
 * @result A tuple of two sets containing small and large values.
 * @complexity O(log n)
 *
 * @property correctness: A.p :: a; xs :: Set a:
 *     all ((>) p) lt`                        // Right split
 *         /\ all ((<) p) gt`
 *         /\ all (\x -> isMember x xsminp) xs` // No junk
 *         /\ all (\x -> isMember x xs`) xsminp // All members used
 *         /\ integrity lt                      // Data structure integrity
 *         /\ integrity gt
 *     where
 *         xsminp = 'StdList'.filter ((<>) p) (toList xs)
 *         (lt,gt) = split p xs
 *         (lt`,gt`) = (toList lt, toList gt)
 *         xs` = lt` ++ gt`
 */
split :: !a !(Set a) -> (!Set a, !Set a) | < a
	special a=Int; a=String

/**
 * Performs a 'split' but also returns whether the pivot element was found in
 * the original set.
 *
 * @complexity O(log n)
 * @property correctness: A.p :: a; xs :: Set a:
 *     all ((>) p) lt`                        // Right split
 *         /\ all ((<) p) gt`
 *         /\ all (\x -> isMember x xsminp) xs` // No junk
 *         /\ all (\x -> isMember x xs`) xsminp // All members used
 *         /\ bool =.= isMember p (toList xs)   // Boolean is correct
 *         /\ integrity lt                      // Data structure integrity
 *         /\ integrity gt
 *     where
 *         xsminp = 'StdList'.filter ((<>) p) (toList xs)
 *         (lt,bool,gt) = splitMember p xs
 *         (lt`,gt`) = (toList lt, toList gt)
 *         xs` = lt` ++ gt`
 */
splitMember :: !a !(Set a) -> (!Set a, !Bool, !Set a) | < a
	special a=Int; a=String

/**
 * Convert the set to an ascending list of elements.
 * @type (Set a) -> l a | List l a
 * @complexity O(n)
 * @property composition with fromList is identity: A.xs :: Set a:
 *     xs =.= fromList (lazyList $ toList xs)
 */
toList s :== toAscList s

/**
 * Same as toList.
 * @complexity O(n)
 * @type (Set a) -> l a | List l a
 */
toAscList t :== 'Data.Foldable'.foldr` (\a as -> [|a:as]) [|] t

/**
 * Create a set from a list of elements.
 * @complexity O(n*log n)
 */
fromList :: !(l a) -> Set a | < a & List l a
	special
		l = [], a = Int; l = [], a = String; l = [], a = Char; l = [], a = Real;
		l = [!], a = Int; l = [!], a = String; l = [!], a = Char; l = [!], a = Real;
		l = [!!], a = Int; l = [!!], a = String; l = [!!], a = Char; l = [!!], a = Real;
		l = [ !], a = Int; l = [ !], a = String; l = [ !], a = Char; l = [ !], a = Real;
		l = [#], a = Int; l = [#], a = String; l = [#], a = Char; l = [#], a = Real;
		l = [#!], a = Int; l = [#!], a = String; l = [#!], a = Char; l = [#!], a = Real

/**
 * Create a set from a list sorted in ascending order. The sorted precondition is not checked.
 * @complexity O(n)
 * @property composition with toAscList is identity: A.xs :: Set a:
 *     xs =.= fromAscList (lazyList $ toAscList xs)
 * @property identical to fromList as long as the list is sorted: A.a :: [a]:
 *     fromList a =.= fromAscList (sort a)
 * @property generates valid set: A.a :: [a]:
 *     integrity (fromAscList (sort a))
 */
fromAscList :: !(l a) -> Set a | == a & List l a
	special
		l = [], a = Int; l = [], a = String; l = [], a = Char; l = [], a = Real; l = [], a = Bool;
		l = [!], a = Int; l = [!], a = String; l = [!], a = Char; l = [!], a = Real; l = [!], a = Bool;
		l = [!!], a = Int; l = [!!], a = String; l = [!!], a = Char; l = [!!], a = Real; l = [!!], a = Bool;
		l = [ !], a = Int; l = [ !], a = String; l = [ !], a = Char; l = [ !], a = Real; l = [ !], a = Bool;
		l = [#], a = Int; l = [#], a = String; l = [#], a = Char; l = [#], a = Real; l = [#], a = Bool;
		l = [#!], a = Int; l = [#!], a = String; l = [#!], a = Char; l = [#!], a = Real; l = [#!], a = Bool

/**
 * Create a set from a list sorted in descending order. The sorted precondition is not checked.
 * @complexity O(n)
 * @property composition with (reverse o toAscList) is identity: A.xs :: Set a:
 *     xs =.= fromDescList (reverse (toAscList xs))
 * @property identical to fromList as long as the list is sorted: A.a :: [a]:
 *     fromList a =.= fromDescList (reverse (sort a))
 * @property generates valid set: A.a :: [a]:
 *     integrity (fromDescList (reverse (sort a)))
 */
fromDescList :: !(l a) -> Set a | == a & List l a
	special
		l = [], a = Int; l = [], a = String; l = [], a = Char; l = [], a = Real; l = [], a = Bool;
		l = [!], a = Int; l = [!], a = String; l = [!], a = Char; l = [!], a = Real; l = [!], a = Bool;
		l = [!!], a = Int; l = [!!], a = String; l = [!!], a = Char; l = [!!], a = Real; l = [!!], a = Bool;
		l = [ !], a = Int; l = [ !], a = String; l = [ !], a = Char; l = [ !], a = Real; l = [ !], a = Bool;
		l = [#], a = Int; l = [#], a = String; l = [#], a = Char; l = [#], a = Real; l = [#], a = Bool;
		l = [#!], a = Int; l = [#!], a = String; l = [#!], a = Char; l = [#!], a = Real; l = [#!], a = Bool

/**
 * Create a set from a list sorted in ascending order without any duplicate elements. These preconditions are not
 * checked.
 * @property composition with toAscList is identity: A.xs :: Set a:
 *     xs =.= fromAscList (lazyList $ toAscList xs)
 * @property identical to fromList as long as the list is sorted and does not contain duplicates: A.a :: [a]:
 *     fromList a =.= fromDistinctAscList (removeDup (sort a))
 * @property generates valid set: A.a :: [a]:
 *     integrity (fromDistinctAscList (removeDup (sort a)))
 */
fromDistinctAscList :: !(l a) -> Set a | List l a
	special
		l = []; l = [!]; l = [!!]; l = [ !];
		l = [#], a = Int; l = [#], a = String; l = [#], a = Char; l = [#], a = Real; l = [#], a = Bool;
		l = [#!], a = Int; l = [#!], a = String; l = [#!], a = Char; l = [#!], a = Real; l = [#!], a = Bool

/**
 * Create a set from a list sorted in descending order without any duplicate elements. These preconditions are not
 * checked.
 * @property composition with (reverse o toAscList) is identity: A.xs :: Set a:
 *     xs =.= fromDescList (reverse (toAscList xs))
 * @property identical to fromList as long as the list is sorted and does not contain duplicates: A.a :: [a]:
 *     fromList a =.= fromDistinctDescList (reverse (removeDup (sort a)))
 * @property generates valid set: A.a :: [a]:
 *     integrity (fromDistinctDescList (reverse (removeDup (sort a))))
 */
fromDistinctDescList :: !(l a) -> Set a | List l a
	special
		l = []; l = [!]; l = [!!]; l = [ !];
		l = [#], a = Int; l = [#], a = String; l = [#], a = Char; l = [#], a = Real; l = [#], a = Bool;
		l = [#!], a = Int; l = [#!], a = String; l = [#!], a = Char; l = [#!], a = Real; l = [#!], a = Bool

/**
 * Map a function to all elements in a set.
 * @type (a -> b) (Set a) -> Set b | <, == a & <, == b
 */
mapSet f s :== fromList (map f (toList s))

/**
 * Map a set without converting it to and from a list.
 */
mapSetMonotonic :: !(a -> b) !(Set a) -> Set b
