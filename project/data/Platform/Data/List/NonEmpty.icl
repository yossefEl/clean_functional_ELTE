implementation module Data.List.NonEmpty


from Control.Applicative import class <*>(..), class pure(..)
from Control.Monad import class Monad
from Data.Foldable import class Foldable
from Data.Functor import class Functor(..)
from Data.List import instance == [a], instance <*> []
from StdList import instance length []
from StdOverloaded import class length, class +++, class ==
import Data.Foldable
import Data.Func
import Data.Monoid
import StdBool
import StdFunctions
import StdInt
import StdMisc
import qualified Data.List

instance == (NonEmpty a) | == a
where
	(==) (a :| as) (b :| bs) = a == b && as == bs

instance length NonEmpty
where
	length (_ :| xs) = 1 + length xs

instance +++ (NonEmpty a)
where
	(+++) (a :| as) (b :| bs) = a :| (as 'Data.List'. ++ [b : bs])

instance Foldable NonEmpty
where
	foldr f z (x :| xs) = foldr f z [x : xs]

instance Functor NonEmpty
where
	fmap f xs = map f xs

instance pure NonEmpty
where
	pure x = x :| []

instance <*> NonEmpty
where
	(<*>) (f :| fs) (x :| xs) =
		case [f : fs] <*> [x : xs] of
			[x : xs] -> x :| xs
			_ -> abort "Unreachable: The app of two non-empty lists will never produce the empty list"

instance Monad NonEmpty
where
	bind xs f = flatten $ map f xs

map :: !(a -> b) !(NonEmpty a) -> NonEmpty b
map f (x :| xs) = f x :| 'Data.List'.map f xs

head :: !(NonEmpty a) -> a
head (x :| _) = x

tail :: !(NonEmpty a) -> [a]
tail (_ :| xs) = xs

last :: !(NonEmpty a) -> a
last (x :| []) = x
last (_ :| xs) = 'Data.List'.last xs

init :: !(NonEmpty a) -> [a]
init (_ :| []) = []
init (x :| xs) = [x : 'Data.List'.init xs]

flatten :: !(NonEmpty (NonEmpty a)) -> NonEmpty a
flatten xs = foldl1 (+++) xs

toList :: !(NonEmpty a) -> [a]
toList (x :| xs) = [x : xs]

fromList :: ![a] -> ?(NonEmpty a)
fromList [] = ?None
fromList [x : xs] = ?Just (x :| xs)

fromListUnsafe :: ![a] -> NonEmpty a
fromListUnsafe [] = abort "Data.List.NonEmpty: Tried to contruct NonEmpty list from empty list."
fromListUnsafe [x : xs] = x :| xs
