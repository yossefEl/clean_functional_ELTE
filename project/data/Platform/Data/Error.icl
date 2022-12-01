implementation module Data.Error

import StdEnv
import Data.Functor, Data.Bifunctor, Data.Maybe, Data.GenEq, Data.Func
import Control.Monad
import Control.Monad.Fail
import Control.Applicative

instance Functor (MaybeError a)
where
	fmap :: (a -> b) !(MaybeError e a) -> MaybeError e b
	fmap f mbErr = mapMaybeError f mbErr

instance Bifunctor MaybeError
where
	bifmap f _ (Error a) = Error (f a)
	bifmap _ g (Ok a) = Ok (g a)

instance pure (MaybeError a)
where
	pure x = Ok x

instance <*> (MaybeError a)
where
	(<*>) (Error e) _ = Error e
	(<*>) (Ok f)    r = fmap f r

instance Monad (MaybeError e) where
	bind (Error l) _ = Error l
	bind (Ok r) k = k r

instance MonadFail (MaybeError String) where
	fail s = Error s

instance == (MaybeError a b) | == a & == b where
	(==) (Ok x) y = case y of
		Ok y = x == y
		_    = False
	(==) (Error x) y = case y of
		Error y = x == y
		_       = False

derive gEq MaybeError

isOk :: !(MaybeError a b) -> Bool
isOk (Ok _)    = True
isOk (Error _) = False

isError :: !(MaybeError a b) -> Bool
isError (Ok _)    = False
isError (Error _) = True

fromOk :: !(MaybeError .a .b) -> .b
fromOk (Ok b)    = b
fromOk (Error _) = abort "Data.Error.fromOk: argument is Error"

fromOkWithError :: !(MaybeError e .a) -> .a | toString e
fromOkWithError (Ok a) = a
fromOkWithError (Error e) = abort $ "Data.Error.fromOkWithError: " +++ toString e

fromError :: !(MaybeError .a .b) -> .a
fromError (Error a) = a
fromError (Ok _)    = abort "Data.Error.fromError: argument is Ok"

liftError :: !(MaybeError .a .b) -> (MaybeError .a .c)
liftError (Error a) = Error a
liftError (Ok _)    = abort "Data.Error.liftError: argument is Ok"

mapMaybeError :: (.a -> .b) !(.MaybeError e .a) -> .MaybeError e .b
mapMaybeError f (Ok a) = Ok (f a)
mapMaybeError _ e      = liftError e

mapError :: !(ex -> ey) !(MaybeError ex a) -> MaybeError ey a
mapError f me = first f me

mb2error :: !e !(?a) -> MaybeError e a
mb2error error mbV = maybe (Error error) Ok mbV

okSt :: *st (.a *st -> *st) !(MaybeError .e .a) -> *st
okSt st f (Error _) = st
okSt st f (Ok x)    = f x st

error2mb :: !(MaybeError e a) -> ?a
error2mb (Error _) = ?None
error2mb (Ok a)  = ?Just a

seqErrors :: !(MaybeError e a) (a -> MaybeError e b) -> MaybeError e b
seqErrors a bfunc = case a of
	Ok a    = bfunc a
	Error e = Error e

combineErrors :: !(MaybeError e a) (MaybeError e b) (a b -> MaybeError e c) -> MaybeError e c
combineErrors a b combf = case a of
	Error e = Error e
	Ok a = case b of
		Error e = Error e
		Ok b    = combf a b

seqErrorsSt :: !(.st -> (MaybeError e a,.st)) (a .st -> u:(MaybeError e b, .st)) !.st -> v:(MaybeError e b, !.st), [u <= v]
seqErrorsSt aop bop st
	# (a,st) = aop st
	= case a of
		Error e = (Error e, st)
		Ok a    = bop a st

combineErrorsSt :: !(.st -> (MaybeError e a, .st)) (.st -> (MaybeError e b, .st)) (a b -> MaybeError e c) !.st -> (!MaybeError e c, !.st)
combineErrorsSt aop bop combf st
	# (a,st) = aop st
	= case a of
		Error e = (Error e,st)
		Ok a
			# (b,st) = bop st
			= case b of
				Error e = (Error e, st)
				Ok b    = (combf a b, st)
