definition module Data.GenHash

/**
 * This module provides a generic hashing function, with the basic building
 * blocks taken from MurmurHash2 (https://github.com/aappleby/smhasher).
 *
 * This hash is not cryptographically secure, but generates few collisions and
 * is relatively fast. It can therefore be used to speed up comparisons
 * (https://softwareengineering.stackexchange.com/a/145633), in particular to
 * create keys for efficient `Map`s.
 */

import StdGeneric
from StdInt import instance * Int, IF_INT_64_OR_32, bitxor
from StdList import qualified foldl
from StdOverloaded import class toInt(..), class toString(..), class *(*), class +(+)
from Data.Either import :: Either
from Data.Error import :: MaybeError

//* Used to quickly compare keys of `SDSNotifyRequest`s and `SDSCacheKey`s.
generic gHash a :: !a -> Int

gHash{|Int|} i = murmurHash_prep i
gHash{|Char|} c = murmurHash_prep (toInt c + 1)
gHash{|Bool|} b = murmurHash_prep (if b 1 2)
gHash{|Real|} r = murmurHash (toString r) // this gives an equal hash for different binary representations of the same value
gHash{|String|} s = murmurHash s

gHash{|()|} _ = 0
gHash{|(,)|} fx fy (x,y) = murmurHash_combine2 (fx x) (fy y)
gHash{|(,,)|} fa fb fc (a,b,c) = murmurHash_combine [fa a,fb b,fc c]
gHash{|(,,,)|} fa fb fc fd (a,b,c,d) = murmurHash_combine [fa a,fb b,fc c,fd d]

gHash{|[]|} fx xs = murmurHash_combine [fx x \\ x <- xs]

gHash{|UNIT|} _ = 0

gHash{|PAIR|} fx fy (PAIR x y) = murmurHash_combine2 (fx x) (fy y)

gHash{|EITHER|} fl _ (LEFT l)  = fl l
gHash{|EITHER|} _ fr (RIGHT r) = fr r

gHash{|CONS of {gcd_index}|} fx (CONS x) = murmurHash_combine2 (murmurHash_prep gcd_index) (fx x)
gHash{|OBJECT|} fx (OBJECT x) = fx x

gHash{|FIELD|} fx (FIELD x) = fx x
gHash{|RECORD|} fx (RECORD x) = fx x

derive gHash ?, Either, MaybeError

murmurHash :: !String -> Int

murmurHash_M :== IF_INT_64_OR_32 0xc6a4a7935bd1e995 0x5bd1e995
murmurHash_R :== IF_INT_64_OR_32 47 24

//* Combines a list of ints as is done in murmurHash
murmurHash_combine xs :== combine xs
where
	combine [] = murmurHash_prep 3
	combine [x:xs] = 'StdList'.foldl murmurHash_combine2 x xs

//* Combines two ints as is done in murmurHash
murmurHash_combine2 x y :== (x bitxor y) * murmurHash_M

//* Prepares an int for incorporation into a murmurHash
murmurHash_prep x
	# x = x * murmurHash_M
	# x = x bitxor (x shiftrU murmurHash_R)
	:== x

//* Unsigned >>
(shiftrU) infix 7 :: !Int !Int -> Int
