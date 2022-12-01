// ************************************************************
//  MersenneTwister - A pseudo-random generator
//  Version 1.0.1 - July 30, 1999 - Thorsten Zoerner
//  Catholic University of Nijmegen - zoerner@cs.kun.nl
// ************************************************************
//  This implementation is a rewrite of a C program by T. Nishimura
//  which can be found in the paper "Mersenne Twister: A 
//  623-dimensionally equidistributed uniform pseudorandom number 
//  generator" by M. Matsumoto and T. Nishimura in ACM Transactions 
//  on Modeling and Computer Simulation, vol. 8, no. 1, 
//  January 1998, pp. 3-30.
//  And for the 64 bit version in the paper "Tables of 64-bit
//  Mersenne Twisters" by Takuji Nishimura
// ************************************************************
//  The original C code contained the following notice:
//      Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.
//      When you use this, send an email to: matumoto@math.keio.ac.jp
//      with an appropriate reference to your work.
// ************************************************************

implementation module MersenneTwister

import StdArray,StdBool,StdEnum,StdDebug,StdInt,StdList,StdMisc,StdReal,StdString

N :== IF_INT_64_OR_32 312 624
M :== IF_INT_64_OR_32 156 397

Nm1 :== IF_INT_64_OR_32 311 623
NmM :== IF_INT_64_OR_32 156 227
MmN :== IF_INT_64_OR_32 -156 -227

A			:== IF_INT_64_OR_32 0xB5026F5AA96619E9 0x9908b0df
uppermask	:== IF_INT_64_OR_32 0xFFFFFFFF80000000 0x80000000
lowermask	:== 0x7FFFFFFF

Mask_b		:== IF_INT_64_OR_32 0x71D67FFFEDA60000 0x9d2c5680
Mask_c		:== IF_INT_64_OR_32 0xFFF7EEE000000000 0xefc60000

u :== IF_INT_64_OR_32 29 11
s :== IF_INT_64_OR_32 17 7
t :== IF_INT_64_OR_32 37 15
l :== IF_INT_64_OR_32 43 18

shiftRight :: !Int !Int -> Int;
shiftRight m n = code inline {
	shiftrU
}

initrand :: Int *{#Int} -> *{#Int}
initrand seed mt 
	| seed==0
		= abort "initrand: seed must not be 0\n"
		= init_i 1 {mt & [0] = seed} seed
where
	init_i :: !Int !*{#Int} !Int -> *{#Int}
	init_i i mt n
		| i<N
			# n = IF_INT_64_OR_32 ((n bitxor (shiftRight n 62)) * 6364136223846793005 + i) (n * 69069);
			= init_i (i+1) {mt & [i] = n} n
			= mt

genRandInt :: Int -> [Int]
genRandInt seed 
	#! first_mt = initrand seed (createArray N 0)
	= gr_mti 0 (new_mti 0 first_mt)
where
	gr_mti mti mt
		| mti==N
			= gr_mti 0 (new_mti 0 mt)
			#! y = mt.[mti]
			# y = y bitxor (IF_INT_64_OR_32 ((shiftRight y u) bitand 0x5555555555555555) (shiftRight y u))
			# y = y bitxor ((y << s) bitand Mask_b)
			# y = y bitxor ((y << t) bitand Mask_c)
			#! y = y bitxor (shiftRight y l)
			= [y : gr_mti (inc mti) mt]

	new_mti :: Int *{#Int} -> *{#Int}
	new_mti k mt
		| k<NmM
			#! y = (mt.[k] bitand uppermask) bitor (mt.[k+1] bitand lowermask)
			#! mttmp = mt.[k+M] bitxor (shiftRight y 1) bitxor ((~(y bitand 1)) bitand A)
			= new_mti (inc k) {mt & [k] = mttmp}
			= new_mti2 k mt

	new_mti2 :: Int *{#Int} -> *{#Int}
	new_mti2 k mt
		| k<Nm1
			#! y = (mt.[k] bitand uppermask) bitor (mt.[k+1] bitand lowermask)
			#! mttmp = mt.[k+MmN] bitxor (shiftRight y 1) bitxor ((~(y bitand 1)) bitand A)
			= new_mti2 (inc k) {mt & [k] = mttmp}
			#! y = (mt.[k] bitand uppermask) bitor (mt.[0] bitand lowermask)
			#! mttmp = mt.[k+MmN] bitxor (shiftRight y 1) bitxor ((~(y bitand 1)) bitand A)
			= {mt & [k] = mttmp}

uint2Real i :== if (i >= 0) (toReal i) (4294967296.0 + toReal i)

genRandReal :: Int -> [Real]
genRandReal seed
	= [IF_INT_64_OR_32
		((toReal (shiftRight y 11)) / 9007199254740991.0)
		((uint2Real y) / 4294967295.0)
	  \\ y<-genRandInt seed]
