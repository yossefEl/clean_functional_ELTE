definition module Data.List.NonEmpty

/**
 * Implements a List type that is guaranteed to be non-empty. This is achieved
 * by ensuring that a head element always exists. Implements a series of
 * classes. For all classes and functions not directly implemented by this
 * type, one can use the `(fromListUnsafe o f o toList)` construct.
 *
 * Because of the many clashes with Data.List and StdList, you will almost
 * certainly want to import this module qualified. To import a module qualified
 * use the following:
 *
 * import qualified Data.List.NonEmpty as NonEmpty
 *
 * Using the functions can then be done like 'NonEmpty'.map

 * @property-bootstrap
 *     from StdList import instance == [a], ++
 *     from Data.List import instance Functor [!], instance length []
 *     import qualified Data.List
 *     import Data.Maybe
 *     import Data.Maybe.Gast
 *     import Data.Maybe.GenPrint
 *     import StdBool
 *     import StdFunctions
 *     import StdInt
 *
 *     derive class Gast NonEmpty
 *
 * @property-test-with a = Int
 */

from Control.Applicative import class <*>, class pure, class Applicative
from Control.Monad import class Monad
from Data.Foldable import class Foldable
from Data.Functor import class Functor
from StdOverloaded import class length, class +++, class ==

:: NonEmpty a = (:|) infixr 5 a [a]

/**
 * @property correctness: A.as :: NonEmpty a; bs :: NonEmpty a:
 *     as == bs <==> toList as == toList bs
 */
instance == (NonEmpty a) | == a

/**
 * @property correctness: A.a :: a; as :: [a]:
 *     length (a :| as) =.= 'Data.List'.length [a : as]
 */
instance length NonEmpty

/**
 * @property correctness: A.as :: NonEmpty a; bs :: NonEmpty a:
 *     as +++ bs =.= fromListUnsafe ((toList as) ++ (toList bs))
 */
instance +++ (NonEmpty a)
instance Foldable NonEmpty
instance Functor NonEmpty
instance pure NonEmpty
instance <*> NonEmpty
instance Monad NonEmpty

/**
 * Maps a function on all elements of the NonEmpty list.
 *
 * @property no-reorder: A.as :: NonEmpty a:
 *    map id as == as
 */
map :: !(a -> b) !(NonEmpty a) -> NonEmpty b

/**
 * Results in the first element of the list. Note that this head function
 * cannot bottom because the head is guaranteed to be non-empty.
 *
 * @property correctness: A.a :: a; as :: [a]:
 *     head (a :| as) =.= a
 */
head :: !(NonEmpty a) -> a

/**
 * Results in a list with the first element removed. Such that the length after
 * the function is always one less than before.
 *
 * @property correctness: A.a :: a; as :: [a]:
 *     tail (a :| as) =.= as
 */
tail :: !(NonEmpty a) -> [a]

/**
 * Results in the last element of the list. Like that head function, this
 * function cannot bottom since the list is guaranteed to have at least one
 * element.
 *
 * @property correctness: A.a ::a; as :: [a]:
 *     last (fromListUnsafe (as ++ [a])) =.= a
 */
last :: !(NonEmpty a) -> a

/**
 * Results in a list with the last element removed. Such that the length after
 * the function is always one less than before.
 *
 * @property correctness: A.a ::a; as :: [a]:
 *     init (fromListUnsafe (as ++ [a])) =.= as
 */
init :: !(NonEmpty a) -> [a]

/**
 * Flatten a NonEmpty list of NonEmpty lists.
 *
 * @property correctness: A.as :: NonEmpty (NonEmpty a):
 *     'Data.List'.flatten (toList (map toList as)) =.= toList (flatten as)
 */
flatten :: !(NonEmpty (NonEmpty a)) -> NonEmpty a

/**
 * Convert a NonEmpty list into an ordinary list
 * @property correctness: A.a ::a; as :: [a]:
 *     toList (a :| as) =.= [a : as]
 */
toList :: !(NonEmpty a) -> [a]

/**
 * Convert a list into a NonEmpty list, results in ?None when the provided list is empty.
 * @property correctness: A.a ::a; as :: [a]:
 *     ?Just (a :| as) =.= fromList [a : as]
 */
fromList :: ![a] -> ?(NonEmpty a)

/**
 * Convert a lists into a NonEmpty list. Aborts when the provided list is empty with a fitting error message.
 *
 * NOTE: Use only when absolutely sure the provided list is non-empty.
 *
 * @property correctness: A.a ::a; as :: [a]:
 *     (a :| as) =.= fromListUnsafe [a : as]
 */
fromListUnsafe :: ![a] -> NonEmpty a
