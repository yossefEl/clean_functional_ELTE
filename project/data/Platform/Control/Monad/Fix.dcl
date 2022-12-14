definition module Control.Monad.Fix

from Control.Monad import class Monad
from Control.Applicative import class pure, class <*>, class Applicative
from Data.Functor import class Functor

class MonadFix m | Monad m where
	mfix :: (a -> m a) -> m a

instance MonadFix ? where
	mfix :: !(a -> ?a) -> ?a

instance MonadFix []
