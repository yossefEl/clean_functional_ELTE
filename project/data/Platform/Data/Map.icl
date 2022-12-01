implementation module Data.Map

import StdEnv
import Data.Either
import Data.GenLexOrd
import Data.Maybe
import Data.Monoid
import Data.Functor
import Data.List
import Control.Applicative
import Control.Monad

import qualified Data.Set
from Data.Set import :: Set

instance Semigroup (Map k v) | < k
where
	mappend :: !(Map k v) !(Map k v) -> Map k v | < k
	mappend x y = union x y

instance Monoid (Map k v) | < k
where
	mempty = newMap

mapSize :: !(Map k a) -> Int
mapSize Tip              = 0
mapSize (Bin sz _ _ _ _) = sz

mapSizeU :: !u:(Map k v:v) -> (!Int, !u:Map k v:v), [u <= v]
mapSizeU m=:Tip              = (0, m)
mapSizeU m=:(Bin sz _ _ _ _) = (sz, m)

lexOrd x y :== if (x < y) LT (if (x > y) GT EQ)

member :: !k !(Map k a) -> Bool | < k
member _ Tip              = False
member k (Bin _ kx _ l r) = if (k < kx)
                              (member k l)
                              (if (k > kx)
                                 (member k r)
                                 True)

find :: !k !(Map k a) -> a | < k
find _ Tip              = abort "Map.!: given key is not an element in the map"
find k (Bin _ kx x l r) = if (k < kx)
                              (find k l)
                              (if (k > kx)
                                 (find k r)
                                 x)

findWithDefault :: !a !k !(Map k a) -> a | < k
findWithDefault def _ Tip              = def
findWithDefault def k (Bin _ kx x l r) = if (k < kx)
                                           (findWithDefault def k l)
                                           (if (k > kx)
                                              (findWithDefault def k r)
                                              x)

findKey :: !a !(Map k a) -> ?k | == a
findKey a m = findKeyWith ((==) a) m

findKeyWith :: !(a -> Bool) !(Map k a) -> ?k
findKeyWith select m = listToMaybe [k` \\ (k`,v) <- toList m | select v]

findKeyWithDefault :: !k !a !(Map k a) -> k | == a
findKeyWithDefault k a m = findKeyWithDefaultWith ((==) a) k m

findKeyWithDefaultWith :: !(a -> Bool) !k !(Map k a) -> k
findKeyWithDefaultWith compare k m = fromMaybe k (findKeyWith compare m)

newMap :: w:(Map k u:v), [ w <= u]
newMap = Tip

singleton :: !k !v:v -> u:Map k v:v, [u <= v]
singleton k x = Bin 1 k x Tip Tip

put :: !k !v:v !u:(Map k v:v) -> u:Map k v:v | < k
put kx x Tip               = singleton kx x
put kx x (Bin sz ky y l r) =
  if (kx < ky)
    (balanceL ky y (put kx x l) r)
    (if (kx > ky)
       (balanceR ky y l (put kx x r))
       (Bin sz kx x l r))

// Insert a new key and value in the map if it is not already present.
// Used by `union`.
putR :: !k !a !(Map k a) -> Map k a | < k
putR kx x Tip = singleton kx x
putR kx x t=:(Bin _ ky y l r) =
  if (kx < ky)
    (balanceL ky y (putR kx x l) r)
    (if (kx > ky)
       (balanceR ky y l (putR kx x r))
       t)

del :: !k !(Map k a) -> Map k a | < k
del _ Tip = Tip
del k (Bin _ kx x l r) =
  if (k < kx)
    (balanceR kx x (del k l) r)
    (if (k > kx)
       (balanceL kx x l (del k r))
       (glue l r))

alter :: !((?a) -> ?a) !k !(Map k a) -> Map k a | < k
alter f k Tip = case f ?None of
	?None   -> Tip
	?Just x -> singleton k x

alter f k (Bin sx kx x l r) = case lexOrd k kx of
	LT -> balance kx x (alter f k l) r
	GT -> balance kx x l (alter f k r)
	EQ -> case f (?Just x) of
		?Just x` -> Bin sx kx x` l r
		?None    -> glue l r

//////////////////////////////////////////////////////////////////////
//  Indexing
//////////////////////////////////////////////////////////////////////
getIndex :: !k !(Map k a) -> ?Int | < k
getIndex k m = go 0 k m
where
	go :: !Int !k !(Map k a) -> ?Int | < k
	go _   _ Tip  = ?None
	go idx k (Bin _ kx _ l r) = case lexOrd k kx of
		LT -> go idx k l
		GT -> go (idx + mapSize l + 1) k r
		EQ -> ?Just (idx + mapSize l)

elemAt :: !Int !(Map k a) -> ?(!k, !a)
elemAt _ Tip = ?None
elemAt i (Bin _ kx x l r)
	#! mapSizeL = mapSize l
	= case lexOrd i mapSizeL of
		LT -> elemAt i l
		GT -> elemAt (i - mapSizeL - 1) r
		EQ -> ?Just (kx,x)

updateAt :: !(k a -> ?a) !Int !(Map k a) -> ?(Map k a)
updateAt f i Tip = ?None
updateAt f i (Bin sx kx x l r)
	#! mapSizeL = mapSize l
	= case lexOrd i mapSizeL of
		LT -> flip (balanceR kx x) r <$> updateAt f i l
		GT -> balanceL kx x l <$> updateAt f (i-mapSizeL-1) r
		EQ -> case f kx x of
			?Just x` -> ?Just (Bin sx kx x` l r)
			?None    -> ?Just (glue l r)

//////////////////////////////////////////////////////////////////////
//  Minimal, Maximal
//////////////////////////////////////////////////////////////////////
findMin :: !(Map k a) -> (!k, !a)
findMin (Bin _ kx x Tip _)  = (kx,x)
findMin (Bin _ _  _ l _)    = findMin l
findMin Tip                 = abort "Map.findMin: newMap map has no minimal element"

findMax :: !(Map k a) -> (!k, !a)
findMax (Bin _ kx x _ Tip)  = (kx,x)
findMax (Bin _ _  _ _ r)    = findMax r
findMax Tip                 = abort "Map.findMax: newMap map has no maximal element"

//////////////////////////////////////////////////////////////////////
//  Union.
//////////////////////////////////////////////////////////////////////
union :: !(Map k a) !(Map k a) -> Map k a | < k
union Tip t2  = t2
union t1 Tip  = t1
union t1 t2 = hedgeUnion ?None ?None t1 t2

unions :: ![Map k a] -> Map k a | < k
unions ts = foldl union newMap ts

unionsWith :: !(a a -> a) ![Map k a] -> Map k a | < k
unionsWith f ts = foldl (unionWith f) newMap ts

unionWith :: !(a a -> a) !(Map k a) !(Map k a) -> Map k a | < k
unionWith f m1 m2 = unionWithKey (appUnion f) m1 m2
  where
  appUnion :: !(a a -> a) k !a !a -> a
  appUnion f _ x y = f x y

unionWithKey :: !(k a a -> a) !(Map k a) !(Map k a) -> Map k a | < k
unionWithKey f t1 t2 = mergeWithKey (appUnion f) id id t1 t2
where
	appUnion :: !(k a a -> a) !k !a !a -> ?a
	appUnion f k x y = ?Just (f k x y)

// left-biased hedge union
hedgeUnion :: !(?a) !(?a) !(Map a b) !(Map a b) -> Map a b | < a
hedgeUnion _   _   t1  Tip = t1
hedgeUnion blo bhi Tip (Bin _ kx x l r) = link kx x (filterGt blo l) (filterLt bhi r)
hedgeUnion _   _   t1  (Bin _ kx x Tip Tip) = putR kx x t1 // According to benchmarks, this special case increases performance
                                                           // up to 30%. It does not help in difference or intersection.
hedgeUnion blo bhi (Bin _ kx x l r) t2
	#! bmi = ?Just kx
	= link kx x (hedgeUnion blo bmi l (trim blo bmi t2))
	            (hedgeUnion bmi bhi r (trim bmi bhi t2))
hedgeUnion _ _ _ _ = abort "error in hedgeUnion\n"

//////////////////////////////////////////////////////////////////////
//  Difference
//////////////////////////////////////////////////////////////////////
difference :: !(Map k a) !(Map k b) -> Map k a | < k
difference Tip _   = Tip
difference t1 Tip  = t1
difference t1 t2   = hedgeDiff ?None ?None t1 t2

hedgeDiff :: !(?a) !(?a) !(Map a b) !(Map a c) -> Map a b | < a
hedgeDiff _   _   Tip              _ = Tip
hedgeDiff blo bhi (Bin _ kx x l r) Tip = link kx x (filterGt blo l) (filterLt bhi r)
hedgeDiff blo bhi t (Bin _ kx _ l r)
	#! bmi = ?Just kx
	= merge (hedgeDiff blo bmi (trim blo bmi t) l) (hedgeDiff bmi bhi (trim bmi bhi t) r)
hedgeDiff _ _ _ _ = abort "error in hedgeDiff\n"

//////////////////////////////////////////////////////////////////////
//  Intersection
//////////////////////////////////////////////////////////////////////
intersection :: !(Map k a) !(Map k b) -> Map k a | < k
intersection Tip _ = Tip
intersection _ Tip = Tip
intersection t1 t2 = hedgeInt ?None ?None t1 t2

hedgeInt :: !(?k) !(?k) !(Map k a) !(Map k b) -> Map k a | < k
hedgeInt _ _ _   Tip = Tip
hedgeInt _ _ Tip _   = Tip
hedgeInt blo bhi (Bin _ kx x l r) t2
	#! bmi = ?Just kx
	#! l` = hedgeInt blo bmi l (trim blo bmi t2)
	#! r` = hedgeInt bmi bhi r (trim bmi bhi t2)
	| member kx t2 = link kx x l` r`
	| otherwise    = merge l` r`

intersectionWith :: !(a b -> c) !(Map k a) !(Map k b) -> Map k c | < k
intersectionWith f m1 m2
  = intersectionWithKey (\_ x y -> f x y) m1 m2

intersectionWithKey :: !(k a b -> c) !(Map k a) !(Map k b) -> Map k c | < k
intersectionWithKey f t1 t2 = mergeWithKey (\k x1 x2 -> ?Just (f k x1 x2)) (const Tip) (const Tip) t1 t2

//////////////////////////////////////////////////////////////////////
//  MergeWithKey
//////////////////////////////////////////////////////////////////////

/**
 * A high-performance universal combining function. This function is used to
 * define `unionWith`, `unionWithKey`, `differenceWith`, `differenceWithKey`,
 * `intersectionWith`, `intersectionWithKey` and can be used to define other
 * custom combine functions.
 *
 * When calling `mergeWithKey combine only1 only2`, a function combining two
 * maps is created, such that
 *
 * * if a key is present in both maps, it is passed with both corresponding
 *   values to the `combine` function. Depending on the result, the key is
 *   either present in the result with specified value, or is left out;
 *
 * * a nonnewMap subtree present only in the first map is passed to `only1` and
 *   the output is added to the result;
 *
 * * a nonnewMap subtree present only in the second map is passed to `only2`
 *   and the output is added to the result.
 *
 * The `only1` and `only2` methods *must return a map with a subset (possibly
 * `newMap`) of the keys of the given map*. The values can be modified
 * arbitrarily. Most common variants of `only1` and `only2` are `id` and
 * `const newMap`, but for example `map f` or `filterWithKey f` could be used
 * for any `f`.
 *
 * @complexity O(m+n)
 */
mergeWithKey :: !(k a b -> ?c) !((Map k a) -> Map k c) !((Map k b) -> Map k c)
             !(Map k a) !(Map k b) -> Map k c | < k
mergeWithKey f g1 g2 Tip t2 = g2 t2
mergeWithKey f g1 g2 t1 Tip = g1 t1
mergeWithKey f g1 g2 t1 t2 = hedgeMerge f g1 g2 ?None ?None t1 t2

hedgeMerge :: !(a b c -> ?d) !((Map a b) -> Map a d) !((Map a c) -> Map a d)
              !(?a) !(?a) !(Map a b) !(Map a c) -> Map a d | < a
hedgeMerge f g1 g2 _   _   t1  Tip = g1 t1
hedgeMerge f g1 g2 blo bhi Tip (Bin _ kx x l r) = g2 (link kx x (filterGt blo l) (filterLt bhi r))
hedgeMerge f g1 g2 blo bhi (Bin _ kx x l r) t2
	#! bmi              = ?Just kx
	#! l`               = hedgeMerge f g1 g2 blo bmi l (trim blo bmi t2)
	#! (found, trim_t2) = trimLookupLo kx bhi t2
	#! r`               = hedgeMerge f g1 g2 bmi bhi r trim_t2
	= case found of
		?None -> case g1 (singleton kx x) of
			Tip -> merge l` r`
			Bin _ _ x` Tip Tip -> link kx x` l` r`
			_ -> abort "mergeWithKey: Given function only1 does not fulfil required conditions (see documentation)\n"
		?Just x2 -> case f kx x x2 of
			?None    -> merge l` r`
			?Just x` -> link kx x` l` r`
hedgeMerge _ _ _ _ _ _ _ = abort "error in hedgeMerge\n"

//////////////////////////////////////////////////////////////////////
//  Submap
//////////////////////////////////////////////////////////////////////
isSubmapOf :: !(Map k a) !(Map k a) -> Bool | < k & == a
isSubmapOf m1 m2 = isSubmapOfBy (==) m1 m2

isSubmapOfBy :: !(a b -> Bool) !(Map k a) !(Map k b) -> Bool | < k
isSubmapOfBy f t1 t2 = mapSize t1 <= mapSize t2 && submap` f t1 t2

submap` :: !(b c -> Bool) !(Map a b) !(Map a c) -> Bool | < a
submap` _ Tip _ = True
submap` _ _ Tip = False
submap` f (Bin _ kx x l r) t = case found of
	?None   -> False
	?Just y -> f x y && submap` f l lt && submap` f r gt
where
	(lt,found,gt) = splitLookup kx t
submap` _ _ _ = abort "error in submap`\n"

//////////////////////////////////////////////////////////////////////
//  Filtering and mapping
//////////////////////////////////////////////////////////////////////
filterWithKey :: !(k a -> Bool) !(Map k a) -> Map k a
filterWithKey _ Tip = Tip
filterWithKey p (Bin _ kx x l r)
  | p kx x    = link kx x (filterWithKey p l) (filterWithKey p r)
  | otherwise = merge (filterWithKey p l) (filterWithKey p r)

map :: !(a -> b) !(Map k a) -> Map k b
map _ Tip = Tip
map f (Bin sx kx x l r) = Bin sx kx (f x) (map f l) (map f r)

mapWithKey :: !(k a -> b) !(Map k a) -> Map k b
mapWithKey _ Tip = Tip
mapWithKey f (Bin sx kx x l r) = Bin sx kx (f kx x) (mapWithKey f l) (mapWithKey f r)

//////////////////////////////////////////////////////////////////////
//  Folds
//////////////////////////////////////////////////////////////////////
foldrWithKey :: !(k v u:a -> u:a) !u:a !(Map k v) -> u:a
foldrWithKey f z` Tip              = z`
foldrWithKey f z` (Bin _ kx x l r) = foldrWithKey f (f kx x (foldrWithKey f z` r)) l

foldrWithKey` :: !(k v u:a -> u:a) !u:a !(Map k v) -> u:a
foldrWithKey` f e Tip = e
foldrWithKey` f e (Bin _ kx x l r)
	#! e = foldrWithKey` f e r
	#! e = f kx x e
	= foldrWithKey` f e l

foldlWithKey :: !(.a -> .(k -> .(v -> .a))) !.a !(Map k v) -> .a
foldlWithKey f z` Tip              = z`
foldlWithKey f z` (Bin _ kx x l r) = foldlWithKey f (f (foldlWithKey f z` l) kx x) r

foldlWithKey` :: !(.a -> .(k -> .(v -> .a))) !.a !(Map k v) -> .a
foldlWithKey` f e Tip = e
foldlWithKey` f e (Bin _ kx x l r)
	#! e = foldlWithKey` f e l
	#! e = f e kx x
	= foldlWithKey` f e r

//////////////////////////////////////////////////////////////////////
//  List variations
//////////////////////////////////////////////////////////////////////

keysSet :: !(Map k a) -> Set k
keysSet Tip = 'Data.Set'.Tip
keysSet (Bin sz kx _ l r) = 'Data.Set'.Bin sz kx (keysSet l) (keysSet r)

fromSet :: !(k -> a) !(Set k) -> Map k a
fromSet _ 'Data.Set'.Tip            = Tip
fromSet f ('Data.Set'.Bin sz x l r) = Bin sz x (f x) (fromSet f l) (fromSet f r)

fromList :: !u:[v:(a, b)] -> Map a b | == a & < a, [u <= v]
fromList [] = Tip
fromList [(kx, x)] = Bin 1 kx x Tip Tip
fromList [(kx0, x0) : xs0]
  | not_ordered kx0 xs0 = fromList` (Bin 1 kx0 x0 Tip Tip) xs0
  | otherwise = go 1 (Bin 1 kx0 x0 Tip Tip) xs0
  where
    not_ordered :: !a !u:[v:(a, b)] -> Bool | == a & < a, [u <= v]
    not_ordered _ [] = False
    not_ordered kx [(ky,_) : _] = kx >= ky

    fromList` :: !(Map a b) !u:[v:(a, b)] -> Map a b | == a & < a, [u <= v]
    fromList` t0 xs = foldl ins t0 xs
      where ins t (k,x) = put k x t

    go :: !Int !(Map a b) !u:[v:(a, b)] -> Map a b | == a & < a, [u <= v]
    go _ t [] = t
    go _ t [(kx, x)] = putMax kx x t
    go s l xs=:[(kx, x) : xss]
      | not_ordered kx xss = fromList` l xs
      | otherwise = case create s xss of
                      (r, ys, []) -> go (s << 1) (link kx x l r) ys
                      (r, _,  ys) -> fromList` (link kx x l r) ys

    // The create is returning a triple (tree, xs, ys). Both xs and ys
    // represent not yet processed elements and only one of them can be nonnewMap.
    // If ys is nonnewMap, the keys in ys are not ordered with respect to tree
    // and must be puted using fromList`. Otherwise the keys have been
    // ordered so far.
    create :: !Int !u:[v:(a, b)] -> (!Map a b, ![(a, b)], ![(a, b)]) | == a & < a, [u <= v]
    create _ [] = (Tip, [], [])
    create s xs=:[xp : xss]
      | s == 1 = case xp of (kx, x) | not_ordered kx xss -> (Bin 1 kx x Tip Tip, [], xss)
                                    | otherwise -> (Bin 1 kx x Tip Tip, xss, [])
      | otherwise = case create (s >> 1) xs of
                      res=:(_, [], _) -> res
                      (l, [(ky, y)], zs) -> (putMax ky y l, [], zs)
                      (l, ys=:[(ky, y):yss], _) | not_ordered ky yss -> (l, [], ys)
                                               | otherwise -> case create (s >> 1) yss of
                                                   (r, zs, ws) -> (link ky y l r, zs, ws)

//////////////////////////////////////////////////////////////////////
//  Utility functions that return sub-ranges of the original
//  tree. Some functions take a `Maybe value` as an argument to
//  allow comparisons against infinite values. These are called `blow`
//  (?None is -\infty) and `bhigh` (here ?None is +\infty).
//  We use ?value, which is strict in the Just case.
//
//  [trim blow bhigh t]   A tree that is either newMap or where [x > blow]
//                        and [x < bhigh] for the value [x] of the root.
//  [filterGt blow t]     A tree where for all values [k]. [k > blow]
//  [filterLt bhigh t]    A tree where for all values [k]. [k < bhigh]
//
//  [split k t]           Returns two trees [l] and [r] where all keys
//                        in [l] are <[k] and all keys in [r] are >[k].
//  [splitLookup k t]     Just like [split] but also returns whether [k]
//                        was found in the tree.
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
//  [trim blo bhi t] trims away all subtrees that surely contain no
//  values between the range [blo] to [bhi]. The returned tree is either
//  newMap or the key of the root is between @blo@ and @bhi@.
//////////////////////////////////////////////////////////////////////
trim :: !(?k) !(?k) !(Map k a) -> Map k a | < k
trim ?None      ?None t = t
trim (?Just lk) ?None t = greater lk t
where
	greater :: !k !(Map k a) -> Map k a | < k
	greater lo (Bin _ k _ _ r) | k <= lo = greater lo r
	greater _  t` = t`
trim ?None (?Just hk) t = lesser hk t
where
	lesser :: !k !(Map k a) -> Map k a | < k
	lesser hi (Bin _ k _ l _) | k >= hi = lesser hi l
	lesser _  t` = t`
trim (?Just lk) (?Just hk) t = middle lk hk t
where
	middle :: !k !k !(Map k a) -> Map k a | < k
	middle lo hi (Bin _ k _ _ r) | k <= lo = middle lo hi r
	middle lo hi (Bin _ k _ l _) | k >= hi = middle lo hi l
	middle _  _  t` = t`

// Helper function for 'mergeWithKey`. The @'trimLookupLo' lk hk t@ performs both
// @'trim' (Just lk) hk t@ and @'get' lk t@.
trimLookupLo :: !k !(?k) !(Map k a) -> (!?a, !Map k a) | < k
trimLookupLo lk ?None t = greater lk t
where
	greater :: !k !(Map k a) -> (!?a, !Map k a) | < k
	greater lo t`=:(Bin _ kx x l r)
		| lo < kx   = (get lo l, t`)
		| lo > kx   = greater lo r
		| otherwise = (?Just x, r)
	greater _ Tip = (?None, Tip)
trimLookupLo lk (?Just hk) t = middle lk hk t
where
	middle :: !k !k !(Map k a) -> (!?a, !Map k a) | < k
	middle lo hi t`=:(Bin _ kx x l r)
		| lo < kx   = if (kx < hi) (get lo l, t`) (middle lo hi l)
		| lo > kx   = middle lo hi r
		| otherwise = (?Just x, lesser hi r)
	middle _ _ Tip = (?None, Tip)

	lesser :: !k (Map k a) -> Map k a | < k
	lesser hi (Bin _ k _ l _) | k >= hi = lesser hi l
	lesser _ t` = t`

//////////////////////////////////////////////////////////////////////
//  [filterGt b t] filter all keys >[b] from tree [t]
//  [filterLt b t] filter all keys <[b] from tree [t]
//////////////////////////////////////////////////////////////////////
filterGt :: !(?k) !(Map k v) -> Map k v | < k
filterGt ?None t = t
filterGt (?Just b) t = filter` b t
  where
  filter` :: !k !(Map k a) -> Map k a | < k
  filter` _   Tip = Tip
  filter` b` (Bin _ kx x l r) = if (b` < kx)
                                  (link kx x (filter` b` l) r)
                                  (if (b` > kx)
                                     (filter` b` r)
                                     r)

filterLt :: !(?k) !(Map k v) -> Map k v | < k
filterLt ?None t = t
filterLt (?Just b) t = filter` b t
  where
  filter` :: !k !(Map k a) -> Map k a | < k
  filter` _   Tip = Tip
  filter` b` (Bin _ kx x l r) =
    if (kx < b`)
      (link kx x l (filter` b` r))
      (if (kx > b`)
         (filter` b` l)
         l)

//////////////////////////////////////////////////////////////////////
//  Split
//////////////////////////////////////////////////////////////////////
// | /O(log n)/. The expression (@'splitLookup' k map@) splits a map just
// like 'split` but also returns @'get' k map@.
//
// > splitLookup 2 (fromList [(5,"a"), (3,"b")]) == (newMap, ?None, fromList [(3,"b"), (5,"a")])
// > splitLookup 3 (fromList [(5,"a"), (3,"b")]) == (newMap, Just "b", singleton 5 "a")
// > splitLookup 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", ?None, singleton 5 "a")
// > splitLookup 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Just "a", newMap)
// > splitLookup 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], ?None, newMap)

splitLookup :: !k !(Map k a) -> (!Map k a, !?a, !Map k a) | < k
splitLookup k t = case t of
	Tip            -> (Tip,?None,Tip)
	Bin _ kx x l r -> case lexOrd k kx of
		LT
			#! (lt,z,gt) = splitLookup k l
			#! gt` = link kx x gt r
			-> (lt,z,gt`)
		GT
			#! (lt,z,gt) = splitLookup k r
			#! lt` = link kx x l lt
			-> (lt`,z,gt)
		EQ
			-> (l,?Just x,r)

//////////////////////////////////////////////////////////////////////
//  Utility functions that maintain the balance properties of the tree.
//  All constructors assume that all values in [l] < [k] and all values
//  in [r] > [k], and that [l] and [r] are valid trees.
//
//  In order of sophistication:
//    [Bin sz k x l r]  The type constructor.
//    [bin k x l r]     Maintains the correct mapSize, assumes that both [l]
//                      and [r] are balanced with respect to each other.
//    [balance k x l r] Restores the balance and mapSize.
//                      Assumes that the original tree was balanced and
//                      that [l] or [r] has changed by at most one element.
//    [link k x l r]    Restores balance and mapSize.
//
//  Furthermore, we can construct a new tree from two trees. Both operations
//  assume that all values in [l] < all values in [r] and that [l] and [r]
//  are valid:
//    [glue l r]        Glues [l] and [r] together. Assumes that [l] and
//                      [r] are already balanced with respect to each other.
//    [merge l r]       Merges two trees and restores balance.
//
//  Note: in contrast to Adam's paper, we use (<=) comparisons instead
//  of (<) comparisons in [link], [merge] and [balance].
//  Quickcheck (on [difference]) showed that this was necessary in order
//  to maintain the invariants. It is quite unsatisfactory that I haven't
//  been able to find out why this is actually the case! Fortunately, it
//  doesn't hurt to be a bit more conservative.
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
//  Link
//////////////////////////////////////////////////////////////////////
link :: !k !a !(Map k a) !(Map k a) -> Map k a
link kx x Tip r  = putMin kx x r
link kx x l Tip  = putMax kx x l
link kx x l=:(Bin mapSizeL ky y ly ry) r=:(Bin mapSizeR kz z lz rz)
  | delta*mapSizeL < mapSizeR  = balanceL kz z (link kx x l lz) rz
  | delta*mapSizeR < mapSizeL  = balanceR ky y ly (link kx x ry r)
  | otherwise                  = bin kx x l r
link _ _ _ _ = abort "error in link\n"

// putMin and putMax don't perform potentially expensive comparisons.
putMax :: !k !a !(Map k a) -> Map k a
putMax kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balanceR ky y l (putMax kx x r)

putMin :: !k !a !(Map k a) -> Map k a
putMin kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balanceL ky y (putMin kx x l) r

////////////////////////////////////////////////////////////////////
//  [merge l r]: merges two trees.
////////////////////////////////////////////////////////////////////
merge :: !(Map k a) !(Map k a) -> Map k a
merge Tip r   = r
merge l Tip   = l
merge l=:(Bin mapSizeL kx x lx rx) r=:(Bin mapSizeR ky y ly ry)
  | delta*mapSizeL < mapSizeR = balanceL ky y (merge l ly) ry
  | delta*mapSizeR < mapSizeL = balanceR kx x lx (merge rx r)
  | otherwise                 = glue l r
merge _ _ = abort "error in merge\n"

////////////////////////////////////////////////////////////////////
//  [glue l r]: glues two trees together.
//  Assumes that [l] and [r] are already balanced with respect to each other.
////////////////////////////////////////////////////////////////////
glue :: !(Map k a) !(Map k a) -> Map k a
glue Tip r = r
glue l Tip = l
glue l r
  | mapSize l > mapSize r
      #! ((km, m), l`) = deleteFindMax l
      = balanceR km m l` r
  | otherwise
      #! ((km, m), r`) = deleteFindMin r
      = balanceL km m l r`

// | /O(log n)/. Delete and find the minimal element.
//
// > deleteFindMin (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((3,"b"), fromList[(5,"a"), (10,"c")])
// > deleteFindMin                                            Error: can not return the minimal element of an newMap map

deleteFindMin :: !(Map k a) -> (!(!k, !a), !Map k a)
deleteFindMin t
  = case t of
      Bin _ k x Tip r
        = ((k, x), r)
      Bin _ k x l r
        #! (km,l`) = deleteFindMin l
        = (km, balanceR k x l` r)
      Tip
        = (abort "Map.deleteFindMin: can not return the minimal element of an newMap map", Tip)

// | /O(log n)/. Delete and find the maximal element.
//
// > deleteFindMax (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((10,"c"), fromList [(3,"b"), (5,"a")])
// > deleteFindMax newMap                                      Error: can not return the maximal element of an newMap map

deleteFindMax :: !(Map k a) -> (!(!k, !a), !Map k a)
deleteFindMax t
  = case t of
      Bin _ k x l Tip
        = ((k, x), l)
      Bin _ k x l r
        #! (km,r`) = deleteFindMax r
        = (km, balanceL k x l r`)
      Tip
        = (abort "Map.deleteFindMax: can not return the maximal element of an newMap map", Tip)

////////////////////////////////////////////////////////////////////
//  [balance l x r] balances two trees with value x.
//  The mapSizes of the trees should balance after decreasing the
//  mapSize of one of them. (a rotation).
//
//  [delta] is the maximal relative difference between the mapSizes of
//          two trees, it corresponds with the [w] in Adams' paper.
//  [ratio] is the ratio between an outer and inner sibling of the
//          heavier subtree in an unbalanced setting. It determines
//          whether a double or single rotation should be performed
//          to restore balance. It is corresponds with the inverse
//          of $\alpha$ in Adam's article.
//
//  Note that according to the Adam's paper:
//  - [delta] should be larger than 4.646 with a [ratio] of 2.
//  - [delta] should be larger than 3.745 with a [ratio] of 1.534.
//
//  But the Adam's paper is erroneous:
//  - It can be proved that for delta=2 and delta>=5 there does
//    not exist any ratio that would work.
//  - Delta=4.5 and ratio=2 does not work.
//
//  That leaves two reasonable variants, delta=3 and delta=4,
//  both with ratio=2.
//
//  - A lower [delta] leads to a more 'perfectly` balanced tree.
//  - A higher [delta] performs less rebalancing.
//
//  In the benchmarks, delta=3 is faster on put operations,
//  and delta=4 has slightly better deletes. As the put speedup
//  is larger, we currently use delta=3.
//
////////////////////////////////////////////////////////////////////
//delta :: Int
delta :== 3

//ratio :: Int
ratio :== 2

// The balance function is equivalent to the following:
//
//   balance :: k -> a -> Map k a -> Map k a -> Map k a
//   balance k x l r
//     | mapSizeL + mapSizeR <= 1    = Bin mapSizeX k x l r
//     | mapSizeR > delta*mapSizeL   = rotateL k x l r
//     | mapSizeL > delta*mapSizeR   = rotateR k x l r
//     | otherwise             = Bin mapSizeX k x l r
//     where
//       mapSizeL = mapSize l
//       mapSizeR = mapSize r
//       mapSizeX = mapSizeL + mapSizeR + 1
//
//   rotateL :: a -> b -> Map a b -> Map a b -> Map a b
//   rotateL k x l r=:(Bin _ _ _ ly ry) | mapSize ly < ratio*mapSize ry = singleL k x l r
//                                     | otherwise               = doubleL k x l r
//
//   rotateR :: a -> b -> Map a b -> Map a b -> Map a b
//   rotateR k x l@(Bin _ _ _ ly ry) r | mapSize ry < ratio*mapSize ly = singleR k x l r
//                                     | otherwise               = doubleR k x l r
//
//   singleL, singleR :: a -> b -> Map a b -> Map a b -> Map a b
//   singleL k1 x1 t1 (Bin _ k2 x2 t2 t3)  = bin k2 x2 (bin k1 x1 t1 t2) t3
//   singleR k1 x1 (Bin _ k2 x2 t1 t2) t3  = bin k2 x2 t1 (bin k1 x1 t2 t3)
//
//   doubleL, doubleR :: a -> b -> Map a b -> Map a b -> Map a b
//   doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
//   doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
//
// It is only written in such a way that every node is pattern-matched only once.

balance :: !k !u:v !v:(Map k u:v) !v:(Map k u:v) -> v:Map k u:v, [v <= u]
balance k x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x Tip r
           (Bin _ rk rx Tip rr=:(Bin _ _ _ _ _)) -> Bin 3 rk rx (Bin 1 k x Tip Tip) rr
           (Bin _ rk rx (Bin _ rlk rlx _ _) Tip) -> Bin 3 rlk rlx (Bin 1 k x Tip Tip) (Bin 1 rk rx Tip Tip)
           (Bin rs rk rx rl=:(Bin rls rlk rlx rll rlr) rr=:(Bin rrs _ _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rk rx (Bin (1+rls) k x Tip rl) rr
             # (rlls, rll) = mapSizeU rll
             # (rlrs, rlr) = mapSizeU rlr
             | otherwise -> Bin (1+rs) rlk rlx (Bin (1+rlls) k x Tip rll) (Bin (1+rrs+rlrs) rk rx rlr rr)

  (Bin ls lk lx ll lr) -> case r of
           Tip -> case (ll, lr) of
                    (ll=:Tip, lr=:Tip) -> Bin 2 k x (Bin ls lk lx ll lr) Tip
                    (Tip, (Bin _ lrk lrx _ _)) -> Bin 3 lrk lrx (Bin 1 lk lx Tip Tip) (Bin 1 k x Tip Tip)
                    (ll=:(Bin _ _ _ _ _), Tip) -> Bin 3 lk lx ll (Bin 1 k x Tip Tip)
                    (ll=:(Bin lls _ _ _ _), lr=:(Bin lrs lrk lrx lrl lrr))
                      | lrs < ratio*lls -> Bin (1+ls) lk lx ll (Bin (1+lrs) k x lr Tip)
                      # (lrls, lrl) = mapSizeU lrl
                      # (lrrs, lrr) = mapSizeU lrr
                      | otherwise -> Bin (1+ls) lrk lrx (Bin (1+lls+lrls) lk lx ll lrl) (Bin (1+lrrs) k x lrr Tip)
           (Bin rs rk rx rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (rl=:Bin rls rlk rlx rll rlr, rr=:Bin rrs _ _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rk rx (Bin (1+ls+rls) k x l rl) rr
                     # (rlls, rll) = mapSizeU rll
                     # (rlrs, rlr) = mapSizeU rlr
                     | otherwise -> Bin (1+ls+rs) rlk rlx (Bin (1+ls+rlls) k x l rll) (Bin (1+rrs+rlrs) rk rx rlr rr)
                   (_, _) -> abort "Failure in Data.Map.balance"
              | ls > delta*rs  -> case (ll, lr) of
                   (ll=:Bin lls _ _ _ _, lr=:Bin lrs lrk lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lk lx ll (Bin (1+rs+lrs) k x lr r)
                     # (lrls, lrl) = mapSizeU lrl
                     # (lrrs, lrr) = mapSizeU lrr
                     | otherwise -> Bin (1+ls+rs) lrk lrx (Bin (1+lls+lrls) lk lx ll lrl) (Bin (1+rs+lrrs) k x lrr r)
                   (_, _) -> abort "Failure in Data.Map.balance"
              | otherwise -> Bin (1+ls+rs) k x l r

// Functions balanceL and balanceR are specialised versions of balance.
// balanceL only checks whether the left subtree is too big,
// balanceR only checks whether the right subtree is too big.

// balanceL is called when left subtree might have been puted to or when
// right subtree might have been deleted from.
balanceL :: !k !v:v !u:(Map k v:v) !u:(Map k v:v) -> u:Map k v:v, [u <= v]
balanceL k x l r = case r of
  Tip -> case l of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x l Tip
           (Bin _ lk lx Tip (Bin _ lrk lrx _ _)) -> Bin 3 lrk lrx (Bin 1 lk lx Tip Tip) (Bin 1 k x Tip Tip)
           (Bin _ lk lx ll=:(Bin _ _ _ _ _) Tip) -> Bin 3 lk lx ll (Bin 1 k x Tip Tip)
           (Bin ls lk lx ll=:(Bin lls _ _ _ _) lr=:(Bin lrs lrk lrx lrl lrr))
             | lrs < ratio*lls -> Bin (1+ls) lk lx ll (Bin (1+lrs) k x lr Tip)
             # (lrls, lrl) = mapSizeU lrl
             # (lrrs, lrr) = mapSizeU lrr
             | otherwise -> Bin (1+ls) lrk lrx (Bin (1+lls+lrls) lk lx ll lrl) (Bin (1+lrrs) k x lrr Tip)

  (Bin rs _ _ _ _) -> case l of
           Tip -> Bin (1+rs) k x Tip r

           (Bin ls lk lx ll lr)
              | ls > delta*rs  -> case (ll, lr) of
                   (ll=:Bin lls _ _ _ _, lr=:Bin lrs lrk lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lk lx ll (Bin (1+rs+lrs) k x lr r)
                     # (lrls, lrl) = mapSizeU lrl
                     # (lrrs, lrr) = mapSizeU lrr
                     | otherwise -> Bin (1+ls+rs) lrk lrx (Bin (1+lls+lrls) lk lx ll lrl) (Bin (1+rs+lrrs) k x lrr r)
                   (_, _) -> abort "Failure in Data.Map.balanceL"
              | otherwise -> Bin (1+ls+rs) k x l r

// balanceR is called when right subtree might have been puted to or when
// left subtree might have been deleted from.
balanceR :: !k !v:v !u:(Map k v:v) !u:(Map k v:v) -> u:Map k v:v, [u <= v]
balanceR k x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x Tip r
           (Bin _ rk rx Tip rr=:(Bin _ _ _ _ _)) -> Bin 3 rk rx (Bin 1 k x Tip Tip) rr
           (Bin _ rk rx (Bin _ rlk rlx _ _) Tip) -> Bin 3 rlk rlx (Bin 1 k x Tip Tip) (Bin 1 rk rx Tip Tip)
           (Bin rs rk rx rl=:(Bin rls rlk rlx rll rlr) rr=:(Bin rrs _ _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rk rx (Bin (1+rls) k x Tip rl) rr
             # (rlls, rll) = mapSizeU rll
             # (rlrs, rlr) = mapSizeU rlr
             | otherwise -> Bin (1+rs) rlk rlx (Bin (1+rlls) k x Tip rll) (Bin (1+rrs+rlrs) rk rx rlr rr)

  (Bin ls _ _ _ _) -> case r of
           Tip -> Bin (1+ls) k x l Tip

           (Bin rs rk rx rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (rl=:Bin rls rlk rlx rll rlr, rr=:Bin rrs _ _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rk rx (Bin (1+ls+rls) k x l rl) rr
                     # (rlls, rll) = mapSizeU rll
                     # (rlrs, rlr) = mapSizeU rlr
                     | otherwise -> Bin (1+ls+rs) rlk rlx (Bin (1+ls+rlls) k x l rll) (Bin (1+rrs+rlrs) rk rx rlr rr)
                   (_, _) -> abort "Failure in Data.Map.balanceR"
              | otherwise -> Bin (1+ls+rs) k x l r

////////////////////////////////////////////////////////////////////
//  The bin constructor maintains the mapSize of the tree
////////////////////////////////////////////////////////////////////
bin :: !k !a !(Map k a) !(Map k a) -> Map k a
bin k x l r = Bin (mapSize l + mapSize r + 1) k x l r

////////////////////////////////////////////////////////////////////
//  Eq converts the tree to a list. In a lazy setting, this
//  actually seems one of the faster methods to gLexOrd{|*|} two trees
//  and it is certainly the simplest :-)
////////////////////////////////////////////////////////////////////
instance == (Map k a) | Eq k & Eq a where
  (==) t1 t2  = (mapSize t1 == mapSize t2) && (toAscList t1 == toAscList t2)

instance < (Map k v) | Ord k & Ord v where
    (<) t1 t2 = toAscList t1 < toAscList t2

instance Functor (Map k)
where
	fmap :: !(a -> b) !(Map k a) -> Map k b
	fmap f m  = map f m

getU :: !k !w:(Map k v) -> x:(!?v, !y:(Map k v)) | == k & < k, [ x <= y, w <= y]
getU k Tip = (?None, Tip)
getU k (Bin h nk nv left right)
	| k == nk
		= (?Just nv, Bin h nk nv left right)
	| k < nk
		#! (mbv, left) = getU k left
		= (mbv, Bin h nk nv left right)
	| otherwise
		#! (mbv, right) = getU k right
		= (mbv, Bin h nk nv left right)

delU :: !k !w:(Map k u:v) -> x:(?u:v, !y:(Map k u:v)) | == k & < k, [ w y <= u, x <= y, w <= y]
delU k Tip = (?None, Tip) //Do nothing
delU k (Bin h nk nv Tip Tip) //A node with just leaves as children can be safely removed
	| k == nk   = (?Just nv, Tip)
	| otherwise = (?None, Bin h nk nv Tip Tip)
delU k (Bin h nk nv Tip right) //A node without smaller items
	| k == nk = (?Just nv, right) //When found, just remove
	| k < nk  = (?None, Bin h nk nv Tip right) //Do nothing, k is not in the mapping
	| otherwise
		#! (mbv, right)    = delU k right
		#! (hright, right) = height right
		= (mbv, balance nk nv Tip right)
delU k (Bin h nk nv left Tip) //A node without larger items
	| k == nk = (?Just nv, left) //When found just remove
	| k < nk
		#! (mbv,left) = delU k left
		#! (hleft,left) = height left
		= (mbv, balance nk nv left Tip)
	| otherwise
		= (?None, Bin h nk nv left Tip) //Do nothing, k is not in hte mapping
delU k (Bin h nk nv left right) //A node with both larger and smaller items
	| k == nk
		#! (left,k,v) = takeMax left
		#! (h,left,right) = parentHeight left right
		= (?Just nv, balance k v left right) //Replace with the largest of the smaller items and rebalance
	| k < nk	
		#! (mbv, left) = delU k left
		#! (h,left,right) = parentHeight left right
		= (mbv, balance nk nv left right)
	| otherwise
		#! (mbv, right) = delU k right
		#! (h,left,right) = parentHeight left right
		= (mbv, balance nk nv left right)
where // TODO
	//Takes the k and v values from the maximum node in the tree and removes that node
	takeMax :: !u:(Map k v:v) -> (!u:Map k v:v, !k, !v:v), [u <= v]
	takeMax Tip = abort "takeMax of leaf evaluated\n"
	takeMax (Bin _ nk nv left Tip) = (left, nk, nv)
	takeMax (Bin _ nk nv left right)
		#! (right,k,v)    = takeMax right
		#! (hleft,left)   = height left
		#! (hright,right) = height right
		= (balance nk nv left right, k, v)

	//Determines the height of the parent node of two sub trees
	parentHeight :: !u:(Map a v:b) !u:(Map c v:d) -> (!Int, !u:Map a v:b, !u:Map c v:d)
	parentHeight left right
		#! (hleft,left)   = height left
		#! (hright,right) = height right
		#! h              = (max hleft hright) + 1
		= (h, left, right)

height :: !u:(Map k w:v) -> x:(!Int, !y:(Map k w:v)), [u y <= w, x <= y, u <= y]
height m=:Tip             = (0, m)
height m=:(Bin h _ _ _ _) = (h, m)

gEq{|Map|} fk fv mx my = mapSize mx == mapSize my && and [fk kx ky && fv vx vy \\ (kx,vx) <- toList mx & (ky,vy) <- toList my]

gLexOrd{|Map|} kLexOrd vLexOrd x y = gLexOrd{|* -> *|} (gLexOrd{|* -> * -> *|} kLexOrd vLexOrd) (toAscList x) (toAscList y)
