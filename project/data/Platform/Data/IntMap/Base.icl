implementation module Data.IntMap.Base

import StdEnv

import Data.Functor

// Types
instance Functor IntMap
where
	fmap :: (a -> b) !(IntMap a) -> IntMap b
    fmap f xs = map f xs

//  Query
// | /O(1)/. Is the map empty?
//
// > Data.IntMap.null (empty)           == True
// > Data.IntMap.null (singleton 1 'a') == False

null :: (IntMap a) -> Bool
null Nil = True
null _   = False

// | /O(n)/. Number of elements in the map.
//
// > size empty                                   == 0
// > size (singleton 1 'a')                       == 1
// > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3
size :: (IntMap a) -> Int
size t
  = case t of
      Bin _ _ l r -> size l + size r
      Tip _ _ -> 1
      Nil     -> 0

// | /O(min(n,W))/. Is the key a member of the map?
//
// > member 5 (fromList [(5,'a'), (3,'b')]) == True
// > member 1 (fromList [(5,'a'), (3,'b')]) == False

member :: !Int (IntMap a) -> Bool
member k (Bin p m l r)
  | nomatch k p m = False
  | zero k m  = member k l
  | otherwise = member k r
member k (Tip kx _) = k == kx
member k Nil = False

// | /O(min(n,W))/. Is the key not a member of the map?
//
// > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
// > notMember 1 (fromList [(5,'a'), (3,'b')]) == True

notMember :: Int (IntMap a) -> Bool
notMember k m = not (member k m)

// | /O(min(n,W))/. Lookup the value at a key in the map. See also 'Data.Map.lookup'.
lookup :: !Int !(IntMap a) -> ?a
lookup k (Bin p m l r)
	| nomatch k p m = ?None
	| zero k m  = lookup k l
	| otherwise = lookup k r
lookup k (Tip kx x)
	| k == kx   = ?Just x
	| otherwise = ?None
lookup k Nil = ?None


find :: !Int (IntMap a) -> a
find k (Bin p m l r)
  | nomatch k p m = not_found k
  | zero k m  = find k l
  | otherwise = find k r
find k (Tip kx x)
  | k == kx   = x
  | otherwise = not_found k
find k Nil = not_found k

not_found k = abort ("IntMap.!: key is not an element of the map")

// | /O(min(n,W))/. The expression @('findWithDefault' def k map)@
// returns the value at key @k@ or returns @def@ when the key is not an
// element of the map.
//
// > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
// > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'
findWithDefault :: a !Int (IntMap a) -> a
findWithDefault def k (Bin p m l r)
  | nomatch k p m = def
  | zero k m  = findWithDefault def k l
  | otherwise = findWithDefault def k r
findWithDefault def k (Tip kx x)
  | k == kx   = x
  | otherwise = def
findWithDefault def k Nil = def

// | /O(1)/. The empty map.
//
// > empty      == fromList []
// > size empty == 0
empty :: IntMap a
empty
  = Nil

// | The union of a list of maps.
//
// > unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
// >     == fromList [(3, "b"), (5, "a"), (7, "C")]
// > unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
// >     == fromList [(3, "B3"), (5, "A3"), (7, "C")]
unions :: ![IntMap a] -> IntMap a
unions xs = foldl union empty xs

// | /O(n+m)/. The (left-biased) union of two maps.
// It prefers the first map when duplicate keys are encountered,
// i.e. (@'union' == 'unionWith' 'const'@).
//
// > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]
union :: !(IntMap a) !(IntMap a) -> IntMap a
union m1 m2 = mergeWithKey` Bin const id id m1 m2

// | /O(n+m)/. A high-performance universal combining function. Using
// 'mergeWithKey', all combining functions can be defined without any loss of
// efficiency (with exception of 'union', 'difference' and 'intersection',
// where sharing of some nodes is lost with 'mergeWithKey').
//
// Please make sure you know what is going on when using 'mergeWithKey',
// otherwise you can be surprised by unexpected code growth or even
// corruption of the data structure.
//
// When 'mergeWithKey' is given three arguments, it is inlined to the call
// site. You should therefore use 'mergeWithKey' only to define your custom
// combining functions. For example, you could define 'unionWithKey',
// 'differenceWithKey' and 'intersectionWithKey' as
//
// > myUnionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> ?Just (f k x1 x2)) id id m1 m2
// > myDifferenceWithKey f m1 m2 = mergeWithKey f id (const empty) m1 m2
// > myIntersectionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> ?Just (f k x1 x2)) (const empty) (const empty) m1 m2
//
// When calling @'mergeWithKey' combine only1 only2@, a function combining two
// 'IntMap's is created, such that
//
// * if a key is present in both maps, it is passed with both corresponding
//   values to the @combine@ function. Depending on the result, the key is either
//   present in the result with specified value, or is left out;
//
// * a nonempty subtree present only in the first map is passed to @only1@ and
//   the output is added to the result;
//
// * a nonempty subtree present only in the second map is passed to @only2@ and
//   the output is added to the result.
//
// The @only1@ and @only2@ methods /must return a map with a subset (possibly empty) of the keys of the given map/.
// The values can be modified arbitrarily. Most common variants of @only1@ and
// @only2@ are 'id' and @'const' 'empty'@, but for example @'map' f@ or
// @'filterWithKey' f@ could be used for any @f@.
mergeWithKey :: (Int a b -> ?c) ((IntMap a) -> IntMap c) ((IntMap b) -> IntMap c)
                (IntMap a) (IntMap b) -> IntMap c
mergeWithKey f g1 g2 m1 m2 = mergeWithKey` bin combine g1 g2 m1 m2
where
	combine (Tip k1 x1) (Tip _ x2) = case f k1 x1 x2 of
		?None   -> Nil
		?Just x -> Tip k1 x

// Slightly more general version of mergeWithKey. It differs in the following:
//
// * the combining function operates on maps instead of keys and values. The
//   reason is to enable sharing in union, difference and intersection.
//
// * mergeWithKey' is given an equivalent of bin. The reason is that in union*,
//   Bin constructor can be used, because we know both subtrees are nonempty.
mergeWithKey` :: (Prefix Mask (IntMap c) (IntMap c) -> IntMap c)
                 ((IntMap a) (IntMap b) -> IntMap c) ((IntMap a) -> IntMap c)
                 ((IntMap b) -> IntMap c) (IntMap a) (IntMap b) -> IntMap c
mergeWithKey` bin` f g1 g2 m1 m2 = go m1 m2
  where
    go t1=:(Bin p1 m1 l1 r1) t2=:(Bin p2 m2 l2 r2)
      | shorter m1 m2  = merge1
      | shorter m2 m1  = merge2
      | p1 == p2       = bin` p1 m1 (go l1 l2) (go r1 r2)
      | otherwise      = maybe_link p1 (g1 t1) p2 (g2 t2)
      where
        merge1 | nomatch p2 p1 m1  = maybe_link p1 (g1 t1) p2 (g2 t2)
               | zero p2 m1        = bin` p1 m1 (go l1 t2) (g1 r1)
               | otherwise         = bin` p1 m1 (g1 l1) (go r1 t2)
        merge2 | nomatch p1 p2 m2  = maybe_link p1 (g1 t1) p2 (g2 t2)
               | zero p1 m2        = bin` p2 m2 (go t1 l2) (g2 r2)
               | otherwise         = bin` p2 m2 (g2 l2) (go t1 r2)

    go t1`=:(Bin _ _ _ _) t2`=:(Tip k2` _) = merge t2` k2` t1`
      where
      merge t2 k2 t1=:(Bin p1 m1 l1 r1)
        | nomatch k2 p1 m1 = maybe_link p1 (g1 t1) k2 (g2 t2)
        | zero k2 m1 = bin` p1 m1 (merge t2 k2 l1) (g1 r1)
        | otherwise  = bin` p1 m1 (g1 l1) (merge t2 k2 r1)
      merge t2 k2 t1=:(Tip k1 _)
        | k1 == k2 = f t1 t2
        | otherwise = maybe_link k1 (g1 t1) k2 (g2 t2)
      merge t2 _  Nil = g2 t2

    go t1=:(Bin _ _ _ _) Nil = g1 t1

    go t1`=:(Tip k1` _) t2` = merge t1` k1` t2`
      where
      merge t1 k1 t2=:(Bin p2 m2 l2 r2)
        | nomatch k1 p2 m2 = maybe_link k1 (g1 t1) p2 (g2 t2)
        | zero k1 m2 = bin` p2 m2 (merge t1 k1 l2) (g2 r2)
        | otherwise  = bin` p2 m2 (g2 l2) (merge t1 k1 r2)
      merge t1 k1 t2=:(Tip k2 _)
        | k1 == k2 = f t1 t2
        | otherwise = maybe_link k1 (g1 t1) k2 (g2 t2)
      merge t1 _  Nil = g1 t1

    go Nil t2 = g2 t2

    maybe_link _ Nil _ t2 = t2
    maybe_link _ t1 _ Nil = t1
    maybe_link p1 t1 p2 t2 = link p1 t1 p2 t2

// | /O(min(n,W))/. Retrieves the maximal (key,value) pair of the map, and
// the map stripped of that element, or '?None' if passed an empty map.
//
// > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == ?Just ((5,"a"), singleton 3 "b")
// > maxViewWithKey empty == ?None
maxViewWithKey :: !(IntMap a) -> ?((Int, a), IntMap a)
maxViewWithKey t = case t of
	Nil -> ?None
	Bin p m l r | m < 0 -> let (result, l`) = go l in ?Just (result, bin p m l` r)
	_ -> ?Just (go t)
where
	go (Bin p m l r) = case go r of (result, r`) -> (result, bin p m l r`)
	go (Tip k y) = ((k, y), Nil)
	go Nil = abort "maxViewWithKey Nil\n"

// | /O(min(n,W))/. Retrieves the minimal (key,value) pair of the map, and
// the map stripped of that element, or '?None' if passed an empty map.
//
// > minViewWithKey (fromList [(5,"a"), (3,"b")]) == ?Just ((3,"b"), singleton 5 "a")
// > minViewWithKey empty == ?None
minViewWithKey :: !(IntMap a) -> ?((Int, a), IntMap a)
minViewWithKey t = case t of
	Nil -> ?None
	Bin p m l r | m < 0 -> let (result, r`) = go r in ?Just (result, bin p m l r`)
	_ -> ?Just (go t)
where
	go (Bin p m l r) = let (result, l`) = go l in (result, bin p m l` r)
	go (Tip k y) = ((k, y), Nil)
	go Nil = abort "minViewWithKey Nil\n"

// | /O(n)/. Map a function over all values in the map.
//
// > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]
map :: (a -> b) (IntMap a) -> IntMap b
map f t
  = case t of
      Bin p m l r -> Bin p m (map f l) (map f r)
      Tip k x     -> Tip k (f x)
      Nil         -> Nil

// | /O(n)/. Fold the keys and values in the map using the given right-associative
// binary operator, such that
// @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
//
// For example,
//
// > keys map = foldrWithKey (\k x ks -> k:ks) [] map
//
// > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
// > foldrWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"
foldrWithKey :: (Int a b -> b) b !(IntMap a) -> b
foldrWithKey f z t =
  case t of Bin _ m l r | m < 0 -> go (go z l) r // put negative numbers before
                        | otherwise -> go (go z r) l
            _ -> go z t
  where
    go z` Nil           = z`
    go z` (Tip kx x)    = f kx x z`
    go z` (Bin _ _ l r) = go (go z` r) l

// | /O(n)/. Build a map from a list of key\/value pairs where
// the keys are in ascending order and all distinct.
// /The precondition (input list is strictly ascending) is not checked./
//
// > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]
fromDistinctAscList :: ![(Int, a)] -> IntMap a
fromDistinctAscList []         = Nil
fromDistinctAscList [z0 : zs0] = work z0 zs0 Nada
  where
    work :: !(!Int, !a) ![(Int, a)] !(Stack a) -> IntMap a
    work (kx,vx) []             stk = finish kx (Tip kx vx) stk
    work (kx,vx) [z=:(kz,_):zs] stk = reduce z zs (branchMask kx kz) kx (Tip kx vx) stk

    reduce :: !(!Int, !a) ![(Int, a)] !Mask !Prefix !(IntMap a) !(Stack a) -> IntMap a
    reduce z zs _ px tx Nada = work z zs (Push px tx Nada)
    reduce z zs m px tx stk=:(Push py ty stk`) =
        let mxy = branchMask px py
            pxy = mask px mxy
        in  if (shorter m mxy)
                 (reduce z zs m pxy (Bin pxy mxy ty tx) stk`)
                 (work z zs (Push px tx stk))

    finish :: !Prefix !(IntMap a) !(Stack a) -> IntMap a
    finish _  t  Nada = t
    finish px tx (Push py ty stk) = finish p (link py ty px tx) stk
        where m = branchMask px py
              p = mask px m

:: Stack a = Push !Prefix !(IntMap a) !(Stack a) | Nada

instance == (IntMap a) | == a where
  (==) t1 t2  = equal t1 t2

equal :: !(IntMap a) !(IntMap a) -> Bool | == a
equal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 == m2) && (p1 == p2) && (equal l1 l2) && (equal r1 r2)
equal (Tip kx x) (Tip ky y)
  = (kx == ky) && (x==y)
equal Nil Nil = True
equal _   _   = False

link :: !Prefix !(IntMap a) !Prefix !(IntMap a) -> IntMap a
link p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m

bin :: !Prefix !Mask !(IntMap a) !(IntMap a) -> IntMap a
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r   = Bin p m l r

zero :: !Int !Mask -> Bool
zero i m = (i bitand m) == 0

nomatch :: !Int !Prefix !Mask -> Bool
nomatch i p m = mask i m <> p

match :: !Int !Prefix !Mask -> Bool
match i p m = mask i m == p

mask :: !Int !Mask -> Prefix
mask i m = i bitand (~m bitxor m)

// we have to treat the masks as unsigned ints
// this means that the sign bit has to be inverted to preserve order
shorter :: !Mask !Mask -> Bool
shorter m1 m2 = (m1 bitxor signBitOnly) > (m2 bitxor signBitOnly)

branchMask :: !Prefix !Prefix -> Mask
branchMask p1 p2 = highestBitMask (p1 bitxor p2)

highestBitMask :: !Int -> Int
highestBitMask x0 =
	// for the right shift `x6` has to be treated as unsigned int, so the highest bit has to be set to 0
	x6 bitxor (allExceptSignBit bitand (x6 >> 1))
where
	x1 = x0 bitor (x0 >> 1)
	x2 = x1 bitor (x1 >> 2)
	x3 = x2 bitor (x2 >> 4)
	x4 = x3 bitor (x3 >> 8)
	x5 = x4 bitor (x4 >> 16)
	x6 = x5 bitor (x5 >> 32)

signBitOnly      =: ~2^(IF_INT_64_OR_32 63 31)
allExceptSignBit =: bitnot signBitOnly
