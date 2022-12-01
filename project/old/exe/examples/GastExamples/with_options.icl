module with_options

/**
 * An example program using the Gast.CommandLine module to wrap a test
 * collection in a CLI application.
 * Compile this with -nr -nt. For usage details, see --help on the executable.
 *
 * The tests are taken from Peter Achten's StdSetTest in
 * https://gitlab.science.ru.nl/peter88/FP_Example_Solutions/.
 */

import StdBool
from StdFunc import flip
import StdList
import StdString

import Control.GenBimap
from Data.Func import $
import Data.GenLexOrd
from Data.Set import :: Set, instance == (Set a), class Foldable, instance Foldable Set
import qualified Data.Set

import Gast
import Gast.CommandLine

Start w = exposeProperties [OutputTestEvents] []
	[ EP membership
	, EP conversion_invariant
	, EP length_correct
	, EP subset_correct
	, EP proper_subset_correct
	, EP newSet_is_empty
	, EP emptyset
	]
	w

:: Enum = A | B | C
derive class Gast Enum
derive gEq Enum
derive gLexOrd Enum
instance == Enum where (==) x y = x === y
instance <  Enum where (<)  x y = (x =?= y) === LT

membership :: Enum [Enum] -> Property
membership x xs = 'Data.Set'.member x ('Data.Set'.fromList xs) <==> isMember x xs

conversion_invariant :: [Enum] -> Bool
conversion_invariant xs = xs` == 'Data.Set'.fromList (lazyList $ 'Data.Set'.toList xs`)
where xs` = 'Data.Set'.fromList xs

length_correct :: [Enum] -> Bool
length_correct xs = 'Data.Set'.size ('Data.Set'.fromList xs) == length (removeDup xs)

subset_correct :: [Enum] [Enum] -> Property
subset_correct xs ys = 'Data.Set'.isSubsetOf ('Data.Set'.fromList xs) ('Data.Set'.fromList ys)
	<==> all (flip isMember ys) xs

proper_subset_correct :: [Enum] [Enum] -> Property
proper_subset_correct xs ys = 'Data.Set'.isProperSubsetOf ('Data.Set'.fromList xs) ('Data.Set'.fromList ys)
	<==> all (flip isMember ys) xs && not (all (flip isMember xs) ys)

newSet_is_empty :: Property
newSet_is_empty = name "newSet_is_empty" $ 'Data.Set'.null 'Data.Set'.newSet

emptyset :: [Enum] -> Property
emptyset xs =
	('Data.Set'.size xs` == 0 <==> 'Data.Set'.null xs`) /\
	(xs` == 'Data.Set'.newSet <==> 'Data.Set'.null xs`)
where xs` = 'Data.Set'.fromList xs

lazyList :: ![a] -> [a]
lazyList x = x
