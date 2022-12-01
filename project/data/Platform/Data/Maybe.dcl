definition module Data.Maybe

/**
 * This module provides functions to deal with the builtin `?` type.
 */

import _SystemStrictMaybes
from StdMaybe import
	isJust, isNothing, isNone, isJustU, isNothingU, isNoneU,
	mapMaybe,
	instance == (?x),
	instance == (?^x),
	instance == (?#x),
	maybeToList, listToMaybe, catMaybes
from StdMisc import abort
from StdOverloaded import class ==

from Control.Applicative import class pure, class <*>, class Applicative, class *>, class <*, class Alternative
from Control.Monad import class Monad, class MonadPlus
from Control.Monad.Fail import class MonadFail
from Control.Monad.Trans import class MonadTrans
from Data.Foldable import class Foldable
from Data.Functor import class Functor
from Data.Monoid import class Semigroup, class Monoid
from Data.Traversable import class Traversable
from Data.GenEq import generic gEq

fromJust m :== case m of ?|Just x -> x; _ -> abort "fromJust: Nothing\n"

instance Functor ?, ?^
instance pure ?, ?^
instance <*> ?, ?^
instance *> ?, ?^
instance <* ?, ?^
instance Alternative ?, ?^
instance Monad ?, ?^
instance MonadPlus ?, ?^
instance MonadFail ?, ?^

instance Semigroup (? a) | Semigroup a
where
	mappend :: !(? a) !(? a) -> ? a | Semigroup a
instance Semigroup (?^ a) | Semigroup a
where
	mappend :: !(?^ a) !(?^ a) -> ?^ a | Semigroup a
instance Monoid (? a), (?^ a)
instance Foldable ?, ?^
instance Traversable ?, ?^

derive gEq ?, ?^

/**
 * Apply a function to the the contents of a Just value and directly return
 * the result, or return a default value if the argument is a Nothing value.
 */
maybe :: .b .(u:a -> .b) v:(m u:a) -> .b | Maybe m a, [v <= u]
	special m= ?; m= ?^

/**
 * Apply a function to the the contents of a Just value and the state, and
 * directly return the result and a new state. Return the state immediately
 * if the argument is a Nothing value.
 */
maybeSt :: *st (u:a *st -> *st) !v:(m u:a) -> *st | Maybe m a, [v<=u]
	special m= ?; m= ?^

/**
 * Directly return a Just value or return a default value if the argument is a Nothing value.
 */
fromMaybe :: u:a !(v:m u:a) -> u:a | Maybe m a, [v<=u]
	special m= ?; m= ?^

/**
 * The Maybe monad transformer.
 */
:: MaybeT m a = MaybeT !(m (? a))

/**
 * Runs a MaybeT as the monad wrapped inside the transformer.
 */
runMaybeT :: !(MaybeT m a) -> m (? a)

/**
 * Transforms the computation inside a transformer.
 *
 * @param The computation transformation.
 * @param The transformer to be transformed.
 * @result The transformed transformer.
 */
mapMaybeT :: !((m (? a)) -> n (? b)) !(MaybeT m a) -> MaybeT n b

instance Functor (MaybeT m) | Functor m
instance pure (MaybeT m) | pure m
instance <*> (MaybeT m) | Monad m
instance Alternative (MaybeT m) | Monad m
instance Monad (MaybeT m) | Monad m
instance MonadFail (MaybeT m) | MonadFail m
instance MonadTrans MaybeT
where
	liftT :: !(a b) -> MaybeT a b | Monad a
