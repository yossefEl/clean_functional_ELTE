definition module Data.SetBy

/**
 * An efficient implementation of sets.
 *
 * This version is the same as Data.Set, except that the overloaded API is replaced
 * with a higher-order function API.
 *
 * The naming convention is to add 'By' to a function or macro name that is overloaded
 * in Data.Set but uses a higher-order function argument in Data.SetBy.
 *
 * For all documentation, please consult Data.Set.
 *
 * The `morally equivalent` function from Data.Set is added in the comment. This is not
 * a strictly equivalent function because of the different types.
 *
 * When using the functions in Data.SetBy, make sure to use the same higher-order
 * function parameter for the same data structure to ensure internal integrity. 
 * This higher-order function represents the < ordering on your set elements and
 * should have the usual ordering properties:
 *
 * - if a < b and b < c then a < c
 * - if a < b then not (b < a)
 * - if not (a < b) and not (b < a) then a and b are considered to 'equal'
 *
 * @property-bootstrap
 *     import StdChar, StdInt
 *     from StdList import instance length []
 *
 * @property-test-with a = Char
 *
 * @property-test-generator [a] -> SetBy a | < a
 *     gen xs = fromListBy (<) xs
 */

from StdOverloaded import class < (..)
from StdClass import class Ord (<=)
from StdList import foldl, map
from StdBool import not, &&
from Data.GenLexOrd import :: LexOrd
import qualified Data.Foldable
from Data.Foldable import class Foldable

:: SetBy a = TipBy
           | BinBy !Int !a !(SetBy a) !(SetBy a)

/**
 * True iff the two sets have the same number of elements, and these elements
 * are pairwise 'equal' as described above, so the higher-order function 
 * parameter represents < on a, *not* == on a(!)
 *
 * Morally equivalent function: instance == (Set a) | == a
 */
isEqualBy :: !(a a -> Bool) !(SetBy a) !(SetBy a) -> Bool

/**
 * True iff first set is `smaller` than second set, according to 
 * first argument (assuming the two sets are ordered with the
 * same first function argument).
 *
 * Morally equivalent function: instance < (Set a) | < a
 */
isOrderedBy :: !(a a -> Bool) !(SetBy a) !(SetBy a) -> Bool

/**
 * EQ iff the two sets have the same number of elements, occurring in the 
 *    same order. 
 * LT iff the first set is the common prefix of the second set or the common
 *    prefix is followed in the first set with an element that is considered 
 *    than the corresponding element in the second set.
 * GT iff the second set is the common prefix of the first set or the common
 *    prefix is followed in the second set with an element that is considered
 *    greater than the corresponding element in the first set.
 * The comparison of elements is done with the first function argument. 
 *
 * Morally equivalent function: derive gLexOrd Set
 */
lexOrdBy :: !(a a -> Bool) !(SetBy a) !(SetBy a) -> LexOrd

instance Foldable SetBy

/**
 * True iff this is the empty set.
 * @type (SetBy a) -> Bool
 * @property equivalence with size 0: A.s :: SetBy a:
 *     size s == 0 <==> null s
 * @property equivalence with newSet: A.s :: SetBy a:
 *     isEqualBy (<) s newSet <==> null s
 */
null s :== case s of
             TipBy -> True
             (BinBy sz _ _ _) -> False

/**
 * The number of elements in the set.
 * @type (SetBy a) -> Int
 * @property correctness: A.s :: SetBy a:
 *     size s =.= length (toList s)
 */
size s :== case s of
             TipBy -> 0
             (BinBy sz _ _ _) -> sz

/**
 * Is the element in the set?
 *
 * Morally equivalent function: Data.Set.member x s = Data.SetBy.memberBy (<) x s
 */
memberBy :: !(a a -> Bool) !a !(SetBy a) -> Bool

/**
 * Checks if an element is not in the set.
 */
notMemberBy comp x t :== not (memberBy comp x t)

/**
 * Is t1 a subset of t2?
 *
 * Morally equivalent function: Data.Set.isSubsetOf s1 s2 = Data.SetBy.isSubsetOfBy (<) s1 s2
 */
isSubsetOfBy comp t1 t2 :== (size t1 <= size t2) && (isSubsetOfXBy comp t1 t2)

isSubsetOfXBy :: !(a a -> Bool) !(SetBy a) !(SetBy a) -> Bool


/**
 * Is t1 a proper subset of t2?
 *
 * Morally equivalent function: Data.Set.isProperSubsetOf s1 s2 = Data.SetBy.isProperSubsetOfBy (<) s1 s2
 */
isProperSubsetOfBy comp s1 s2 :== (size s1 < size s2) && (isSubsetOfBy comp s1 s2)

/**
 * The empty set.
 * @complexity O(1)
 * @property is null:
 *     null newSet
 */
newSet :: SetBy a

/**
 * Create a singleton set.
 * @complexity O(1)
 */
singleton :: !u:a -> w:(SetBy u:a), [w <= u]

/**
 * Insert an element in a set. If the set already contains an element equal to
 * the given value, it is replaced with the new value.
 *
 * Morally equivalent function: Data.Set.insert x s = Data.SetBy.insertBy (<) x s
 */
insertBy :: !(a a -> Bool) !a !.(SetBy a) -> SetBy a

/**
 * Delete an element from a set.
 *
 * Morally equivalent function: Data.Set.delete x s = Data.SetBy (<) x s
 */
deleteBy :: !(a a -> Bool) !a !.(SetBy a) -> SetBy a


/**
 * The minimal element of a set.
 *
 * Morally equivalent function: Data.Set.findMin
 */
findMin :: !(SetBy a) -> a

/**
 * The maximal element of a set.
 *
 * Morally equivalent function: Data.Set.findMax
 */
findMax :: !(SetBy a) -> a

/**
 * Delete the minimal element.
 *
 * Morally equivalent function: Data.Set.deleteMin
 */
deleteMin :: !.(SetBy a) -> SetBy a

/**
 * Delete the maximal element.
 *
 * Morally equivalent function: Data.Set.deleteMax
 */
deleteMax :: !.(SetBy a) -> SetBy a

/**
 * deleteFindMin set = (findMin set, deleteMin set)
 */
deleteFindMin :: !.(SetBy a) -> (!a, !SetBy a)

/**
 * deleteFindMax set = (findMax set, deleteMax set)
 */
deleteFindMax :: !.(SetBy a) -> (!a, !SetBy a)

/**
 * Retrieves the minimal key of the set, and the set stripped of that element,
 * or `?None` if passed an empty set.
 */
minView :: !.(SetBy a) -> . ?(!a, !SetBy a)

/**
 * Retrieves the maximal key of the set, and the set stripped of that element,
 * or `?None` if passed an empty set.
 */
maxView :: !.(SetBy a) -> . ?(!a, !SetBy a)

/**
 * The union of two sets, preferring the first set when equal elements are
 * encountered.
 *
 * Morally equivalent function: Data.Set.union s1 s2 = Data.SetBy.unionBy (<) s1 s2
 */
unionBy :: !(a a -> Bool) !u:(SetBy a) !u:(SetBy a) -> SetBy a

/**
 * The union of a list of sets.
 *
 * Morally equivalent function: Data.Set.unions ts = Data.SetBy.unionsBy (<) ts
 */
unionsBy ts :== foldl unionBy newSet ts

/**
 * Difference of two sets.
 *
 * Morally equivalent function: Data.Set.difference s1 s2 = Data.SetBy.differenceBy (<) s1 s2
 */
differenceBy :: !(a a -> Bool) !(SetBy a) !(SetBy a) -> SetBy a

/**
 * The intersection of two sets.
 * Elements of the result come from the first set.
 *
 * Morally equivalent function: Data.Set.intersection s1 s2 = Data.SetBy.intersectionBy (<) s1 s2
 */
intersectionBy :: !(a a -> Bool) !(SetBy a) !(SetBy a) -> SetBy a

/**
 * The intersection of a list of sets.
 * Elements of the result come from the first set
 *
 * Morally equivalent function: Data.Set.intersections ts = Data.SetBy.intersectionsBy (<) ts
 */
intersectionsBy :: !(a a -> Bool) ![SetBy a] -> SetBy a

/**
 * Filter all elements that satisfy the predicate.
 *
 * Morally equivalent function: Data.Set.filter
 */
filter :: !(a -> Bool) !(SetBy a) -> SetBy a

/**
 * Partition the set into two sets, one with all elements that satisfy the
 * predicate and one with all elements that don't satisfy the predicate.
 *
 * Morally equivalent function: Data.Set.partition
 */
partition :: !(a -> Bool) !(SetBy a) -> (!SetBy a, !SetBy a)

/**
 * Split a set in elements less and elements greater than a certain pivot.
 *
 * Morally equivalent function: Data.Set.split x s = Data.SetBy.splitBy (<) x s
 */
splitBy :: !(a a -> Bool) !a !(SetBy a) -> (!SetBy a, !SetBy a)

/**
 * Performs a 'split' but also returns whether the pivot element was found in
 * the original set.
 *
 * Morally equivalent function: Data.Set.splitMember x s = Data.SetBy.splitMemberBy (<) x s
 */
splitMemberBy :: !(a a -> Bool) !a !(SetBy a) -> (!SetBy a, !Bool, !SetBy a)

/**
 * Convert the set to an ascending list of elements.
 */
toList s :== toAscList s

/**
 * Same as toList.
 */
toAscList t :== 'Data.Foldable'.foldr` (\a as -> [a:as]) [] t

/**
 * Create a set from a list of elements.
 *
 * Morally equivalent function: Data.Set.fromList xs = Data.SetBy.fromListBy (<) xs
 */
fromListBy :: !(a a -> Bool) ![a] -> SetBy a

/**
 * Map a function to all elements in a set.
 *
 * Morally equivalent function: Data.Set.mapSet f s = Data.SetBy.mapSetBy (<) f s
 */
mapSetBy comp_b f s :== fromListBy comp_b (map f (toList s))

/**
 * Map a set without converting it to and from a list.
 *
 * Morally equivalent function: Data.Set.mapSetMonotonic
 */
mapSetByMonotonic :: !(a -> b) !(SetBy a) -> SetBy b
