implementation module Data.Maybe

import StdBool
import StdFunctions
import StdMaybe
import StdMisc
import Data.Functor
import Data.Monoid
import Data.Func
from Data.Foldable import class Foldable(..)
from Data.Traversable import class Traversable(..)
import qualified Data.Traversable
import Control.Applicative
from Control.Monad import class Monad(..)
import Control.Monad.Fail
import Control.Monad.Trans
import Data.GenEq

// We cannot take the fromJust from StdMaybe because that version is not total.
fromJust m :== case m of ?|Just x -> x; _ -> abort "fromJust: Nothing\n"

instance Functor ? where fmap f m = mapMaybe f m
instance Functor ?^ where fmap f m = mapMaybe f m

instance pure ? where pure x = ?Just x
instance pure ?^ where pure x = ?^Just x

instance <*> ?
where
	(<*>) ?None     _  = ?None
	(<*>) (?Just f) ma = fmap f ma
instance <*> ?^
where
	(<*>) ?^None     _  = ?^None
	(<*>) (?^Just f) ma = fmap f ma

instance *> ?
where
	(*>) (?Just _) m = m
	(*>) _         _ = ?None

instance *> ?^
where
	(*>) (?^Just _) m = m
	(*>) _          _ = ?^None

instance <* ?
where
	(<*) ?None _     = ?None
	(<*) m (?Just _) = m
	(<*) _ _         = ?None
instance <* ?^
where
	(<*) ?^None _     = ?^None
	(<*) m (?^Just _) = m
	(<*) _ _          = ?^None

instance Alternative ?
where
	empty         = ?None
	(<|>) ?None r = r
	(<|>) l     _ = l

instance Alternative ?^
where
	empty          = ?^None
	(<|>) ?^None r = r
	(<|>) l      _ = l

instance Monad ?
where
	bind (?Just x) k = k x
	bind ?None     _ = ?None

instance Monad ?^
where
	bind (?^Just x) k = k x
	bind ?^None     _ = ?^None

instance MonadPlus ?
where
	mzero = ?None
	mplus ?None ys = ys
	mplus xs    _  = xs

instance MonadFail ?
where
	fail _ = ?None

instance MonadPlus ?^
where
	mzero = ?^None
	mplus ?^None ys = ys
	mplus xs     _  = xs

instance MonadFail ?^
where
	fail _ = ?^None

instance Semigroup (? a) | Semigroup a
where
	mappend :: !(? a) !(? a) -> ? a | Semigroup a
	mappend ?None      m          = m
	mappend m          ?None      = m
	mappend (?Just m1) (?Just m2) = ?Just (mappend m1 m2)
	mappend _          _          = abort "impossible case in mappend_Maybe\n"

instance Semigroup (?^ a) | Semigroup a
where
	mappend :: !(?^ a) !(?^ a) -> ?^ a | Semigroup a
	mappend ?^None      m           = m
	mappend m           ?^None      = m
	mappend (?^Just m1) (?^Just m2) = ?^Just (mappend m1 m2)
	mappend _           _           = abort "impossible case in mappend_Maybe\n"

instance Monoid (? a)
where
	mempty = ?None

instance Monoid (?^ a)
where
	mempty = ?^None

instance Foldable ? where
	fold x = foldMap id x
	foldMap f x = foldr (mappend o f) mempty x
	foldr _ z ?None = z
	foldr f z (?Just x) = f x z
	foldr` f z0 xs = foldl f` id xs z0
	where f` k x z = k (f x z)

	foldl _ z ?None = z
	foldl f z (?Just x) = f z x
	foldl` f z0 xs = foldr f` id xs z0
	where f` x k z = k (f z x)
	foldr1 f xs = fromMaybe (abort "foldr1: empty structure") (foldr mf ?None xs)
	where
		mf x ?None = ?Just x
		mf x (?Just y) = ?Just (f x y)
	foldl1 f xs = fromMaybe (abort "foldl1: empty structure") (foldl mf ?None xs)
	where
		mf ?None y = ?Just y
		mf (?Just x) y = ?Just (f x y)

instance Foldable ?^ where
	fold x = foldMap id x
	foldMap f x = foldr (mappend o f) mempty x
	foldr _ z ?^None = z
	foldr f z (?^Just x) = f x z
	foldr` f z0 xs = foldl f` id xs z0
	where f` k x z = k (f x z)

	foldl _ z ?^None = z
	foldl f z (?^Just x) = f z x
	foldl` f z0 xs = foldr f` id xs z0
	where f` x k z = k (f z x)
	foldr1 f xs = fromMaybe (abort "foldr1: empty structure") (foldr mf ?^None xs)
	where
		mf x ?^None = ?^Just x
		mf x (?^Just y) = ?^Just (f x y)
	foldl1 f xs = fromMaybe (abort "foldl1: empty structure") (foldl mf ?^None xs)
	where
		mf ?^None y = ?^Just y
		mf (?^Just x) y = ?^Just (f x y)

instance Traversable ?
where
	traverse _ ?None = pure ?None
	traverse f (?Just x) = ?Just <$> f x

instance Traversable ?^
where
	traverse _ ?^None = pure ?^None
	traverse f (?^Just x) = ?^Just <$> f x

derive gEq ?, ?^

maybe :: .b .(u:a -> .b) v:(m u:a) -> .b | Maybe m a, [v <= u]
maybe x _ ?|None     = x
maybe _ f (?|Just x) = f x

maybeSt :: *st (u:a *st -> *st) !v:(m u:a) -> *st | Maybe m a, [v<=u]
maybeSt st _ ?|None     = st
maybeSt st f (?|Just x) = f x st

fromMaybe :: u:a !(v:m u:a) -> u:a | Maybe m a, [v<=u]
fromMaybe x mb = maybe x id mb

runMaybeT :: !(MaybeT m a) -> m (? a)
runMaybeT (MaybeT f) = f

mapMaybeT :: !((m (? a)) -> n (? b)) !(MaybeT m a) -> MaybeT n b
mapMaybeT f m = MaybeT $ f $ runMaybeT m

instance Functor (MaybeT m) | Functor m where
	fmap f m = mapMaybeT (fmap $ fmap f) m

instance pure (MaybeT m) | pure m where pure x = MaybeT (pure (?Just x))

instance <*> (MaybeT m) | Monad m where
	(<*>) mf mx = MaybeT $
		runMaybeT mf >>= \mb_f ->
		case mb_f of
			?None = pure ?None
			?Just f  =
				runMaybeT mx >>= \mb_x ->
				case mb_x of
					?None   = pure ?None
					?Just x = pure $ ?Just $ f x

instance Alternative (MaybeT m) | Monad m where
	empty = MaybeT $ return ?None

	(<|>) x y = MaybeT $
		runMaybeT x >>= \v ->
		case v of
			?None    -> runMaybeT y
			?Just _  -> return v

instance Monad (MaybeT m) | Monad m where
	bind x f = MaybeT $
		runMaybeT x >>= \v ->
		case v of
			?None   -> return ?None
			?Just y -> runMaybeT $ f y

instance MonadFail (MaybeT m) | MonadFail m where
	fail s = liftT (fail s)

instance MonadTrans MaybeT
where
	liftT :: !(a b) -> MaybeT a b | Monad a
	liftT m = MaybeT $ ?Just <$> m
