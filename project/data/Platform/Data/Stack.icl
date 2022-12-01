implementation module Data.Stack
import StdList, StdOverloaded

newStack :: Stack a
newStack = Stack []

instance length Stack
where
	length (Stack a) = length a

push :: a !(Stack a) -> Stack a
push a (Stack as) = Stack [a : as]

pop :: !(Stack a) -> (?a, Stack a)
pop (Stack []) = (?None, newStack)
pop (Stack [a : as]) = (?Just a, Stack as)

peek :: !(Stack a) -> ?a
peek (Stack []) = ?None
peek (Stack [a : _]) = ?Just a
