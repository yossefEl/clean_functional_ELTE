definition module Control.GenMonad

import StdGeneric
import StdList

class Monad m
where
	ret :: a:a -> m:(m a:a), [m <= a]
 	(>>=) infixl 5 :: u:(m .a) v:(.a -> u:(m .b))  -> u:(m .b), [u <= v]
	
:: StMonad s a = { st_monad :: .(s -> *(a, s)) }
derive bimap StMonad
instance Monad ?, [], (StMonad .s)

generic gMapLM a b :: a:a -> m:(m b:b) | Monad m, [m <= b]
derive gMapLM c, UNIT, PAIR, EITHER, CONS, FIELD, OBJECT
derive gMapLM [], ?, (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

generic gMapRM a b :: a:a -> m:(m b:b) | Monad m, [m <= b]
derive gMapRM c, UNIT, PAIR, EITHER, CONS, FIELD, OBJECT
derive gMapRM [], ?, (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
