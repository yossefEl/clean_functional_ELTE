implementation module Control.Monad.Fix

from StdFunc import o
from StdMisc import abort
import Control.Applicative
from Control.Monad import class Monad
from Data.Func import fix
import Data.List

instance MonadFix ?
where
	mfix :: !(a -> ?a) -> ?a
	mfix f = let a = f (unJust a) in a
	where
		unJust (?Just x) = x
		unJust ?None  = abort "mfix Maybe: ?None"

instance MonadFix []
where
	mfix f = case fix (f o head) of
		[]    -> []
		[x:_] -> [x : mfix (tail o f)]

