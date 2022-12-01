// --------------------------------------------------------------------------
// |
// Module      :  Data.IntSet.Base
// Copyright   :  (c) Daan Leijen 2002
//                (c) Joachim Breitner 2011
// License     :  BSD-style (see the LICENSE.BSD3 file)
// Maintainer  :  libraries@haskell.org
// Stability   :  provisional
// Portability :  portable
//
// = WARNING
//
// This module is considered __internal__.
//
// The Package Versioning Policy __does not apply__.
//
// This contents of this module may change __in any way whatsoever__
// and __without any warning__ between minor versions of this package.
//
// Authors importing this module are expected to track development
// closely.
//
// = Description
//
// An efficient implementation of integer sets.
//
// These modules are intended to be imported qualified, to avoid name
// clashes with Prelude functions, e.g.
//
// >  import Data.IntSet (IntSet)
// >  import qualified Data.IntSet as IntSet
//
// The implementation is based on /big-endian patricia trees/.  This data
// structure performs especially well on binary operations like 'union'
// and 'intersection'.  However, my benchmarks show that it is also
// (much) faster on insertions and deletions when compared to a generic
// size-balanced set implementation (see "Data.Set").
//
//    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
//      Workshop on ML, September 1998, pages 77-86,
//      <http://citeseer.ist.psu.edu/okasaki98fast.html>
//
//    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve
//      Information Coded In Alphanumeric/\", Journal of the ACM, 15(4),
//      October 1968, pages 514-534.
//
// Additionally, this implementation places bitmaps in the leaves of the tree.
// Their size is the natural size of a machine word (32 or 64 bits) and greatly
// reduce memory footprint and execution times for dense sets, e.g. sets where
// it is likely that many values lie close to each other. The asymptotics are
// not affected by this optimization.
//
// Many operations have a worst-case complexity of /O(min(n,W))/.
// This means that the operation can become linear in the number of
// elements with a maximum of /W/ -- the number of bits in an 'Int`
// (32 or 64).
// --------------------------------------------------------------------------

// [Note: INLINE bit fiddling]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~
// It is essential that the bit fiddling functions like mask, zero, branchMask
// etc are inlined. If they do not, the memory allocation skyrockets. The GHC
// usually gets it right, but it is disastrous if it does not. Therefore we
// explicitly mark these functions INLINE.


// [Note: Local 'go' functions and capturing]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Care must be taken when using 'go' function which captures an argument.
// Sometimes (for example when the argument is passed to a data constructor,
// as in insert), GHC heap-allocates more than necessary. Therefore C-- code
// must be checked for increased allocation when creating and modifying such
// functions.


// [Note: Order of constructors]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// The order of constructors of IntSet matters when considering performance.
// Currently in GHC 7.0, when type has 3 constructors, they are matched from
// the first to the last -- the best performance is achieved when the
// constructors are ordered by frequency.
// On GHC 7.0, reordering constructors from Nil | Tip | Bin to Bin | Tip | Nil
// improves the benchmark by circa 10%.

implementation module Data.IntSet.Base

import StdInt, StdBool, StdFunc, StdMisc, StdOverloaded, StdClass, StdTuple
import qualified StdList

// A "Nat" is a natural machine word (an unsigned Int)
:: Nat :== Int

member :: !Key !IntSet -> Bool
member x is = go is
  where
    go (Bin p m l r)
      | nomatch x p m = False
      | zero x m      = go l
      | otherwise     = go r
    go (Tip y bm) = prefixOf x == y && bitmapOf x bitand bm <> 0
    go Nil = False

/* ------------------------------------------------------------------
  Construction
*/
// | /O(1)/. The empty set.
empty :: IntSet
empty = Nil

/* ------------------------------------------------------------------
  Insert
*/
// | /O(min(n,W))/. Add a value to the set. There is no left- or right bias for
// IntSets.
insert :: !Key IntSet -> IntSet
insert x s = insertBM (prefixOf x) (bitmapOf x) s

// Helper function for insert and union.
insertBM :: !Prefix !BitMap IntSet -> IntSet
insertBM kx bm t=:(Bin p m l r)
  | nomatch kx p m = link kx (Tip kx bm) p t
  | zero kx m      = Bin p m (insertBM kx bm l) r
  | otherwise      = Bin p m l (insertBM kx bm r)
insertBM kx bm t=:(Tip kx` bm`)
  | kx` == kx = Tip kx` (bm bitor bm`)
  | otherwise = link kx (Tip kx bm) kx` t
insertBM kx bm Nil = Tip kx bm

// | /O(n*min(n,W))/. Create a set from a list of integers.
fromList :: ![Key] -> IntSet
fromList xs
  = 'StdList'.foldl ins empty xs
  where
    ins t x  = insert x t

/* ------------------------------------------------------------------
  Helpers
*/
/* ------------------------------------------------------------------
  Link
*/
link :: Prefix IntSet Prefix IntSet -> IntSet
link p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m

/* --------------------------------------------------------------------
  Functions that generate Prefix and BitMap of a Key or a Suffix.
*/

//suffixBitMask :: Int
suffixBitMask :== IF_INT_64_OR_32 64 32

//prefixBitMask :: Int
prefixBitMask :== bitnot suffixBitMask

prefixOf :: Int -> Prefix
prefixOf x = x bitand prefixBitMask

suffixOf :: Int -> Int
suffixOf x = x bitand suffixBitMask

bitmapOfSuffix :: Int -> BitMap
bitmapOfSuffix s = 1 << s

bitmapOf :: Int -> BitMap
bitmapOf x = bitmapOfSuffix (suffixOf x)

/* ------------------------------------------------------------------
  Endian independent bit twiddling
*/
zero :: Int Mask -> Bool
zero i m = (i) bitand (m) == 0

nomatch :: Int Prefix Mask -> Bool
nomatch i p m = (mask i m) <> p

// Suppose a is largest such that 2^a divides 2*m.
// Then mask i m is i with the low a bits zeroed out.
mask :: Int Mask -> Prefix
mask i m = maskW (i) (m)

/* ------------------------------------------------------------------
  Big endian operations
*/
maskW :: Nat Nat -> Prefix
maskW i m =  (i bitand (bitnot (m-1) bitxor m))

branchMask :: Prefix Prefix -> Mask
branchMask p1 p2 =  (highestBitMask (p1 bitxor p2))

// TODO Test
testBit x i = (x bitand i) <> 0

// The highestBitMask implementation is based on
// http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
// which has been put in the public domain.

// | Return a word where only the highest bit is set.
highestBitMask :: Int -> Int
highestBitMask x1 = let x2 = x1 bitor x1 >> 1
                        x3 = x2 bitor x2 >> 2
                        x4 = x3 bitor x3 >> 4
                        x5 = x4 bitor x4 >> 8
                        x6 = x5 bitor x5 >> 16
                     in x6 bitxor  (x6 >> 1)
