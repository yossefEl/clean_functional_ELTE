implementation module Data.GenHash

import StdEnv
import StdGeneric

import Data.Either
import Data.Error
import qualified Data.Map

generic gHash a :: !a -> Int

gHash{|Int|} i = murmurHash_prep i
gHash{|Char|} c = murmurHash_prep (toInt c + 1)
gHash{|Bool|} b = murmurHash_prep (if b 1 2)
gHash{|Real|} r = murmurHash (toString r)
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

M :== murmurHash_M
R :== murmurHash_R

murmurHash :: !String -> Int
murmurHash s = IF_INT_64_OR_32 (murmurHash_64 s) (murmurHash_32 s)

murmurHash_64 :: !String -> Int
murmurHash_64 s
	# h = seed bitxor (len*M)
	# mainlen = (len>>3)<<3
	# h = runblocks 0 mainlen h
	# restlen = len bitand 7
	# rest = get_int_from_string mainlen s
	# rest = rest bitand ((1<<(restlen<<3))-1)
	# h = (h bitxor rest) * M
	# h = h bitxor (h shiftrU R)
	# h = h * M
	# h = h bitxor (h shiftrU R)
	= h
where
	seed = bitnot M
	len = size s

	runblocks :: !Int !Int !Int -> Int
	runblocks i end h
		| i >= end = h
		# k = get_int_from_string i s
		# k = k * M
		# k = k bitxor (k shiftrU R)
		# h = (h * M) bitxor (k * M)
		= runblocks (i+8) end h

	get_int_from_string :: !Int !String -> Int
	get_int_from_string offset s = code inline {
		push_a_b 0
		pop_a 1
		addI
		load_i 16
	}

murmurHash_32 :: !String -> Int
murmurHash_32 s
	# h = seed bitxor len
	# mainlen = (len>>2)<<2
	# h = runblocks 0 mainlen h
	# restlen = len bitand 3
	# rest = get_int_from_string mainlen s
	# rest = rest bitand ((1<<(restlen<<3))-1)
	# h = (h bitxor rest) * M
	# h = h bitxor (h shiftrU 13)
	# h = h * M
	# h = h bitxor (h shiftrU 15)
	= h
where
	seed = bitnot M
	len = size s

	runblocks :: !Int !Int !Int -> Int
	runblocks i end h
		| i >= end = h
		# k = get_int_from_string i s
		# k = k * M
		# k = k bitxor (k shiftrU R)
		# h = (h * M) bitxor (k * M)
		= runblocks (i+4) end h

	get_int_from_string :: !Int !String -> Int
	get_int_from_string offset s = code inline {
		push_a_b 0
		pop_a 1
		addI
		load_i 8
	}

(shiftrU) infix 7 :: !Int !Int -> Int
(shiftrU) _ _ = code inline {
	shiftrU
}
