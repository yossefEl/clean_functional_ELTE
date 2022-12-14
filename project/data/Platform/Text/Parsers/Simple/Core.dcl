definition module Text.Parsers.Simple.Core

from Control.Applicative import class pure, class <*>, class Applicative, class *>, class <*, class Alternative
from Control.Monad import class Monad, class MonadPlus
from Data.Either import :: Either
from Data.Functor import class Functor
from StdOverloaded import class ==

:: Parser t a =: Parser ([t] -> ([(a, [t])], [String]))

// AMF instances
instance Functor (Parser t)
instance pure (Parser t)
instance <*> (Parser t)
instance *> (Parser t)
instance <* (Parser t)
instance Alternative (Parser t)
instance Monad (Parser t)
instance MonadPlus (Parser t)

// Functions to run the parser
parse     :: !(Parser t a) [t] -> Either [String] a
runParser :: !(Parser t a) [t] -> ([(a, [t])], [String])

// Core combinators
pFail    :: Parser t a
pYield   :: a -> Parser t a
pSatisfy :: (t -> Bool) -> Parser t t
pError   :: String -> Parser t a
pPeek :: Parser t [t]

// Convenience parsers
(@!) infixr 4 :: (Parser t a) String -> Parser t a

(<<|>) infixr 4 :: (Parser t a) (Parser t a) -> Parser t a
(<|>>) infixr 4 :: (Parser t a) (Parser t a) -> Parser t a

(<:>) infixr 6 :: !(Parser s r) (Parser s [r]) -> Parser s [r]

pMany     :: (Parser s r) -> Parser s [r]
pSome     :: !(Parser s r) -> Parser s [r]
pOptional :: !(Parser s r) (r -> Parser s r) -> Parser s r
pOneOf    :: [t] -> Parser t t | == t
pChainl   :: !(Parser t a) (Parser t (a a -> a)) a -> Parser t a
pChainl1  :: !(Parser t a) (Parser t (a a -> a)) -> Parser t a
pToken    :: t -> Parser t t | == t
pSepBy    :: !(Parser t a) (Parser t s) -> Parser t [a]
pSepBy1   :: !(Parser t a) (Parser t s) -> Parser t [a]
pList     :: ![Parser t a] -> Parser t [a]
