implementation module Math.Random

import StdArray, StdBool, StdEnum, StdInt, StdList, StdMisc, StdReal, StdString

n :== 624
m :== 397

/* Explanation on these bit patterns:
 *
 * This was originally written for 32-bit platforms. There are a couple of
 * special cases that we need to be aware of:
 *
 * - 64-bit platforms should return values in the same range. For positive
 *   values nothing needs to be done, but negative values require a different
 *   binary representation.
 * - When 32-bit code is interpreted on a 64-bit platform using the ABC
 *   interpreter (which can for example happen in WebAssembly), we must avoid
 *   'unsigned' integer constants with the highest bit set (e.g.
 *   `lower_32_bits` is used as an unsigned integer). Because the compiler
 *   outputs integer constants in decimal notation, these will be negative when
 *   compiling with the 32-bit compiler, and will be sign-extended in the
 *   64-bit bytecode. Hence `0xffffffff` becomes `-1` in ABC code and would
 *   become `0xffffffffffffffff` in the bytecode, while we need `0xffffffff`.
 *   In default of unsigned integer constants in Clean, we use ABC code for
 *   integer constants larger than 2^31-1 on 32-bit platforms.
 */

a :== IF_INT_64_OR_32 0x9908b0df a
where
	a :: Int
	a = code inline {
		pushI 2567483615
	}

uppermask :== IF_INT_64_OR_32 0x80000000 uppermask
where
	uppermask :: Int
	uppermask = code inline {
		pushI 2147483648
	}

lowermask :== 0x7fffffff

mask_b :== IF_INT_64_OR_32 0x9d2c5680 mask_b
where
	mask_b :: Int
	mask_b = code inline {
		pushI 2636928640
	}

mask_c :== IF_INT_64_OR_32 0xefc60000 mask_c
where
	mask_c :: Int
	mask_c = code inline {
		pushI 4022730752
	}

lower_32_bits :== IF_INT_64_OR_32 0xffffffff lower_32_bits
where
	lower_32_bits :: Int
	lower_32_bits = code inline {
		pushI 4294967295
	}

u :== 11
s :== 7
t :== 15
l :== 18

shiftRight m n :== (m >> n) bitand (0x7FFFFFFF >> (n-1))

initrand :: Int *{# Int} -> *{# Int}
initrand seed mt 
	| seed==0 = abort "initrand: seed must not be 0\n"
	= init_i 0 { mt & [0] = seed bitand lower_32_bits}
where
	init_i i mt =: { [i] = mti}
		| ii==n = mt
		= init_i ii { mt & [ii] = (69069 * mti) bitand lower_32_bits}
	where
		ii = inc i
		
genRandInt :: !Int -> [Int]
genRandInt seed 
	#! first_mt = initrand seed (createArray n 0)
	#! r = gr_mti 0 (new_mti 0 first_mt)
	= IF_INT_64_OR_32 (map fixsign r) r
where
	//* Interpret the 32bit signed int in the lower 32 bits as an 64 bit signed int
	fixsign :: !Int -> Int
	fixsign n
		| n >= 0x7fffffff = ~(bitnot n bitand 0x7fffffff + 1)
		= n

	gr_mti mti mt
		| mti==n = gr_mti 0 (new_mti 0 mt)
		#! (y, mt) = mt![mti]
		#! y = y bitxor (shiftRight y u)
		#! y = y bitxor ((y << s) bitand mask_b)
		#! y = y bitxor ((y << t) bitand mask_c)
		#! y = y bitxor (shiftRight y l)
		= [ y : gr_mti (inc mti) mt]
	
	new_mti :: Int *{# Int} -> *{# Int}
	new_mti k mt
		| k==n = mt 
		#! (mtk, mt) = mt![k]
		#! (mtkk, mt) = mt![(inc k) rem n] 
		#! y = (mtk bitand uppermask) bitor (mtkk bitand lowermask)
		#! (mtind, mt) = mt![ind k]
		#! mttmp = mtind bitxor (shiftRight y 1) bitxor ((y bitand 0x1) * a)
		= new_mti (inc k) { mt & [k] = mttmp}
	where
		ind :: Int -> Int
		ind k 
			| k<n-m = k+m
			= k+m-n

uint2Real i :== if (i >= 0) (toReal i) (4294967296.0 + toReal i)

genRandReal :: !Int -> [Real]
genRandReal seed = map (\y -> (uint2Real y) / 4294967295.0) (genRandInt seed)

