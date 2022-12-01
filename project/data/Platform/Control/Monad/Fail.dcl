definition module Control.Monad.Fail

from Control.Monad import class Monad
from Control.Applicative import class Applicative, class <*>, class pure
from Data.Functor import class Functor

/**
 * The class with a monadic fail operation for descriptive errors
 *
 * Instances should satisfy the following law:
 *     fail s >>= f = fail s
 *
 * If your {{`Monad`}} has a {{`MonadPlus`}} instance, a popular definition is:
 *     fail _ = mzero
 */
class MonadFail m | Monad m
where
	fail :: String -> m a
