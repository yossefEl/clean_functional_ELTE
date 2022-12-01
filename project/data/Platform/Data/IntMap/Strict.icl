implementation module Data.IntMap.Strict

// Copied from Haskell's Data.IntMap 13 January 2015 by Jurriën Stutterheim

import StdEnv
from Data.GenEq import generic gEq
import Data.Functor
from Data.IntMap.Base import :: IntMap (..), :: Prefix, :: Mask, nomatch, bin, empty, fromDistinctAscList, mask, shorter, branchMask
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

foldr :: !(a b -> b) !b !(IntMap a) -> b
foldr f z t =
  case t of
    Bin _ m l r
      | m < 0
        #! tmp = go f z l
        = go f tmp r // put negative numbers before
      | otherwise
        #! tmp = go f z r
        = go f tmp l
    _ = go f z t
  where
  go :: !(a b -> b) !b !(IntMap a) -> b
  go _ z` Nil       = z`
  go f z` (Tip _ x) = f x z`
  go f z` (Bin _ _ l r)
    #! tmp = go f z` r
    = go f tmp l

// | /O(min(n,W))/. The expression @('findWithDefault' def k map)@
// returns the value at key @k@ or returns @def@ when the key is not an
// element of the map.
//
// > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
// > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'
findWithDefault :: a !Int !(IntMap a) -> a
findWithDefault def k (Bin p m l r)
  | nomatch k p m = def
  | zero k m      = findWithDefault def k l
  | otherwise     = findWithDefault def k r
findWithDefault def k (Tip kx x)
  | k == kx   = x
  | otherwise = def
findWithDefault def _ _ = def

get :: !Int !.(IntMap .a) -> ? .a
get k (Bin p m l r)
	| nomatch k p m = ?None
	| zero k m  = get k l
	| otherwise = get k r
get k (Tip kx x)
	| k == kx   = ?Just x
	| otherwise = ?None
get k Nil = ?None

getU :: !Int !*(IntMap a) -> *(. ?a, *(IntMap a))
getU k (Bin p m l r)
	| nomatch k p m = (?None, Bin p m l r)
	| zero k m  = getU k l
	| otherwise = getU k r
getU k (Tip kx x)
	| k == kx   = (?Just x, Tip kx x)
	| otherwise = (?None, Tip kx x)
getU k Nil = (?None, Nil)

find :: !Int !(IntMap a) -> a
find k (Bin p m l r)
  | nomatch k p m = not_found k
  | zero k m  = find k l
  | otherwise = find k r
find k (Tip kx x)
  | k == kx   = x
  | otherwise = not_found k
find k Nil = not_found k

del :: !Int !(IntMap a) -> IntMap a
del k m = delete k m

delete :: !Int !(IntMap a) -> IntMap a
delete k t =
  case t of
    Bin p m l r
      | nomatch k p m -> t
      | zero k m      -> bin p m (delete k l) r
      | otherwise     -> bin p m l (delete k r)
    Tip ky _
      | k==ky         -> Nil
      | otherwise     -> t
    Nil -> Nil

not_found k = abort ("IntMap.!: key is not an element of the map")
// | /O(1)/. A map of one element.
//
// > singleton 1 'a'        == fromList [(1, 'a')]
// > size (singleton 1 'a') == 1
singleton :: !Int !.a -> .(IntMap .a)
singleton k x = Tip k x

mapSize :: !(IntMap a) -> Int
mapSize m = size m

size :: !(IntMap a) -> Int
size t
  = case t of
      Bin _ _ l r -> size l + size r
      Tip _ _ -> 1
      Nil     -> 0

newMap :: w:(IntMap u:v), [ w <= u]
newMap = Nil

null :: !(IntMap a) -> Bool
null Nil = True
null _   = False

// | /O(min(n,W))/. Insert a new key\/value pair in the map.
// If the key is already present in the map, the associated value is
// replaced with the supplied value, i.e. 'insert' is equivalent to
// @'insertWith' 'const'@.
//
// > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
// > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
// > insert 5 'x' empty                         == singleton 5 'x'
insert :: !Int !u:a !v:(IntMap u:a) -> w:(IntMap u:a), [w <= u,v <= w]
insert k x t =
  case t of
    Bin p m l r
      | nomatch k p m -> link k (Tip k x) p t
      | zero k m      -> Bin p m (insert k x l) r
      | otherwise     -> Bin p m l (insert k x r)
    Tip ky _
      | k == ky         -> Tip k x
      | otherwise     -> link k (Tip k x) ky t
    Nil -> Tip k x

put :: !Int !u:a !v:(IntMap u:a) -> w:(IntMap u:a), [w <= u,v <= w]
put k v m = insert k v m

member :: !Int !(IntMap a) -> Bool
member k (Bin p m l r)
  | nomatch k p m = False
  | zero k m  = member k l
  | otherwise = member k r
member k (Tip kx _) = k == kx
member k Nil = False

elems :: !.(IntMap a) -> [a]
elems m = foldr (\x xs -> [x:xs]) [] m

keys  :: !.(IntMap a) -> [Int]
keys m = foldrWithKey (\k _ ks -> [k : ks]) [] m

union :: !(IntMap a) !(IntMap a) -> IntMap a
union m1 m2 = mergeWithKey` Bin const id id m1 m2

unions :: ![IntMap a] -> IntMap a
unions xs = foldlStrict union empty xs

foldrWithKey :: !(Int .a -> .(.b -> .b)) !.b !.(IntMap .a) -> .b
foldrWithKey f z t =
  case t of Bin _ m l r | m < 0 -> go f (go f z l) r // put negative numbers before
                        | otherwise -> go f (go f z r) l
            _ -> go f z t
  where
  go :: !(Int .a -> .(.b -> .b)) !.b !.(IntMap .a) -> .b
  go _ z` Nil           = z`
  go f z` (Tip kx x)    = f kx x z`
  go f z` (Bin _ _ l r) = go f (go f z` r) l

// right-biased insertion, used by 'union'
// | /O(min(n,W))/. Insert with a combining function.
// @'insertWith' f key value mp@
// will insert the pair (key, value) into @mp@ if key does
// not exist in the map. If the key does exist, the function will
// insert @f new_value old_value@.
//
// > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
// > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
// > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"
insertWith :: !(a a -> a) !Int !a !(IntMap a) -> IntMap a
insertWith f k x t = insertWithKey (\_ x` y` -> f x` y`) k x t

// | /O(min(n,W))/. Insert with a combining function.
// @'insertWithKey' f key value mp@
// will insert the pair (key, value) into @mp@ if key does
// not exist in the map. If the key does exist, the function will
// insert @f key new_value old_value@.
//
// > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
// > insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
// > insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
// > insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"
//
// If the key exists in the map, this function is lazy in @x@ but strict
// in the result of @f@.
insertWithKey :: !(Int a a -> a) !Int !a !(IntMap a) -> IntMap a
insertWithKey f k x t =
  case t of
    Bin p m l r
      | nomatch k p m -> link k (singleton k x) p t
      | zero k m      -> Bin p m (insertWithKey f k x l) r
      | otherwise     -> Bin p m l (insertWithKey f k x r)
    Tip ky y
      | k == ky       -> Tip k (f k x y)
      | otherwise     -> link k (singleton k x) ky t
    Nil -> singleton k x

// | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
// a member of the map, the original map is returned.
//
// > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
// > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
// > adjust ("new " ++) 7 empty                         == empty
adjust :: !(a -> a) !Int !(IntMap a) -> IntMap a
adjust f k m = adjustWithKey (\_ x -> f x) k m

// | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
// a member of the map, the original map is returned.
//
// > let f key x = (show key) ++ ":new " ++ x
// > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
// > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
// > adjustWithKey f 7 empty                         == empty
adjustWithKey :: !(Int a -> a) !Int !(IntMap a) -> IntMap a
adjustWithKey f k m = updateWithKey (\k` x -> ?Just (f k` x)) k m

// | /O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
// at @k@ (if it is in the map). If (@f k x@) is '?None', the element is
// deleted. If it is (@'?Just' y@), the key @k@ is bound to the new value @y@.
//
// > let f k x = if x == "a" then ?Just ((show k) ++ ":new a") else ?None
// > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
// > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
// > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
updateWithKey :: !(Int a -> ?a) !Int !(IntMap a) -> IntMap a
updateWithKey f k t = case t of
	Bin p m l r
		| nomatch k p m -> t
		| zero k m      -> bin p m (updateWithKey f k l) r
		| otherwise     -> bin p m l (updateWithKey f k r)
	Tip ky y
		| k == ky -> case f k y of
			?Just y` -> Tip ky y`
			?None    -> Nil
		| otherwise -> t
	Nil -> Nil

// | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
// 'alter' can be used to insert, delete, or update a value in an 'IntMap'.
// In short : @'get' k ('alter' f k m) = f ('get' k m)@.
alter :: !((?a) -> ?a) !Int !(IntMap a) -> IntMap a
alter f k t = case t of
	Bin p m l r
		| nomatch k p m -> case f ?None of
			?None   -> t
			?Just x -> link k (Tip k x) p t
		| zero k m  -> bin p m (alter f k l) r
		| otherwise -> bin p m l (alter f k r)
	Tip ky y
		| k == ky -> case f (?Just y) of
			?Just x -> Tip ky x
			?None   -> Nil
		| otherwise -> case f ?None of
			?Just x -> link k (Tip k x) ky t
			?None   -> t
	Nil -> case f ?None of
		?Just x -> Tip k x
		?None   -> Nil

// | The union of a list of maps, with a combining operation.
//
// > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
// >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]
unionsWith :: !(a a -> a) ![IntMap a] -> IntMap a
unionsWith f ts = foldlStrict (unionWith f) empty ts

// | /O(n+m)/. The union with a combining function.
//
// > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]
unionWith :: !(a a -> a) !(IntMap a) !(IntMap a) -> IntMap a
unionWith f m1 m2 = unionWithKey (\_ x y -> f x y) m1 m2

// | /O(n+m)/. The union with a combining function.
//
// > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
// > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]
unionWithKey :: !(Int a a -> a) !(IntMap a) !(IntMap a) -> IntMap a
unionWithKey f m1 m2 = mergeWithKey` Bin g id id m1 m2
where
	g (Tip k1 x1) (Tip _ x2) = Tip k1 (f k1 x1 x2)
	g _           _          = abort "error in unionWithKey\n"

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
// The values can be modified arbitrarily.  Most common variants of @only1@ and
// @only2@ are 'id' and @'const' 'empty'@, but for example @'map' f@ or
// @'filterWithKey' f@ could be used for any @f@.
mergeWithKey :: !(Int a b -> ?c) !((IntMap a) -> IntMap c) !((IntMap b) -> IntMap c)
                !(IntMap a) !(IntMap b) -> IntMap c
mergeWithKey f g1 g2 m1 m2 = mergeWithKey` bin combine g1 g2 m1 m2
where
	combine (Tip k1 x1) (Tip _ x2) = case f k1 x1 x2 of
		?None   -> Nil
		?Just x -> Tip k1 x
	combine _ _ = abort "error in mergeWithKey\n"

// | /O(n)/. Map a function over all values in the map.
//
// > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]
map :: !(a -> b) !(IntMap a) -> IntMap b
map f t
  = case t of
      Bin p m l r -> Bin p m (map f l) (map f r)
      Tip k x     -> Tip k (f x)
      Nil         -> Nil

// | /O(n)/. Map a function over all values in the map.
//
// > let f key x = (show key) ++ ":" ++ x
// > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]
mapWithKey :: !(Int a -> b) !(IntMap a) -> IntMap b
mapWithKey f t
  = case t of
      Bin p m l r -> Bin p m (mapWithKey f l) (mapWithKey f r)
      Tip k x     -> Tip k (f k x)
      Nil         -> Nil

// | /O(n*min(n,W))/. Create a map from a list of key\/value pairs.
//
// > fromList [] == empty
// > fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
// > fromList [(5,"c"), (3,"b"), (5, "a")] == fromList [(5,"a"), (3,"b")]
fromList :: ![(Int, a)] -> IntMap a
fromList xs
  = foldlStrict ins empty xs
  where
  ins t (k,x) = insert k x t

// | /O(n*min(n,W))/. Create a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
//
// > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
// > fromListWith (++) [] == empty
fromListWith :: !(a a -> a) ![(Int, a)] -> IntMap a
fromListWith f xs = fromListWithKey (\_ x y -> f x y) xs

// | /O(n*min(n,W))/. Build a map from a list of key\/value pairs with a combining function. See also fromAscListWithKey'.
//
// > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
// > fromListWith (++) [] == empty
fromListWithKey :: !(Int a a -> a) ![(Int, a)] -> IntMap a
fromListWithKey f xs = foldlStrict ins empty xs
  where
  ins t (k,x) = insertWithKey f k x t

mapSt :: !(a *st -> *(b, *st)) !.(IntMap a) !*st -> *(!IntMap b, !*st)
mapSt _ Nil       st = (Nil, st)
mapSt f (Tip n x) st
  #! (x, st) = f x st
  = (Tip n x, st)
mapSt f (Bin p m l r) st
  #! (l, st) = mapSt f l st
  #! (r, st) = mapSt f r st
  = (Bin p m l r, st)

derive JSONEncode IntMap
derive JSONDecode IntMap
derive gEq IntMap

foldlStrict :: !(a b -> a) !a ![b] -> a
foldlStrict f acc [] = acc
foldlStrict f acc [x:xs]
  #! z` = f acc x
  = foldlStrict f z` xs

zero :: !Int !Mask -> Bool
zero i m = (i bitand m) == 0

link :: !Prefix !u:(IntMap v:a) !Prefix !w:(IntMap v:a) -> x:(IntMap v:a), [x <= v,w u <= x]
link p1 t1 p2 t2
  #! m = branchMask p1 p2
  #! p = mask p1 m
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1

mergeWithKey` :: !(Prefix Mask (IntMap c) (IntMap c) -> IntMap c)
                 !((IntMap a) (IntMap b) -> IntMap c) !((IntMap a) -> IntMap c)
                 !((IntMap b) -> IntMap c) !(IntMap a) !(IntMap b) -> IntMap c
mergeWithKey` bin` f g1 g2 t1=:(Bin p1 m1 l1 r1) t2=:(Bin p2 m2 l2 r2)
  | shorter m1 m2  = merge1 bin` f g1 g2 t1 p1 m1 l1 r1 t2 p2
  | shorter m2 m1  = merge2 bin` f g1 g2 t1 p1 t2 p2 m2 l2 r2
  | p1 == p2       = bin` p1 m1 (mergeWithKey` bin` f g1 g2 l1 l2) (mergeWithKey` bin` f g1 g2 r1 r2)
  | otherwise      = maybe_link p1 (g1 t1) p2 (g2 t2)
  where
    merge1 :: !(Int Int (IntMap a) (IntMap a) -> IntMap a)
              !((IntMap b) (IntMap c) -> IntMap a) !((IntMap b) -> IntMap a)
              !((IntMap c) -> IntMap a) !(IntMap b) !Int !Int !(IntMap b)
              !(IntMap b) !(IntMap c) !Int -> IntMap a
    merge1 bin` f g1 g2 t1 p1 m1 l1 r1 t2 p2
      | nomatch p2 p1 m1  = maybe_link p1 (g1 t1) p2 (g2 t2)
      | zero p2 m1        = bin` p1 m1 (mergeWithKey` bin` f g1 g2 l1 t2) (g1 r1)
      | otherwise         = bin` p1 m1 (g1 l1) (mergeWithKey` bin` f g1 g2 r1 t2)
    merge2 :: !(Int Int (IntMap a) (IntMap a) -> IntMap a)
              !((IntMap b) (IntMap c) -> IntMap a) !((IntMap b) -> IntMap a)
              !((IntMap c) -> IntMap a) !(IntMap b) !Int !(IntMap c) !Int !Int
              !(IntMap c) !(IntMap c) -> IntMap a
    merge2 bin` f g1 g2 t1 p1 t2 p2 m2 l2 r2
      | nomatch p1 p2 m2  = maybe_link p1 (g1 t1) p2 (g2 t2)
      | zero p1 m2        = bin` p2 m2 (mergeWithKey` bin` f g1 g2 t1 l2) (g2 r2)
      | otherwise         = bin` p2 m2 (g2 l2) (mergeWithKey` bin` f g1 g2 t1 r2)

mergeWithKey` bin` f g1 g2 t1`=:(Bin _ _ _ _) t2`=:(Tip k2` _) = merge bin` f g1 g2 t2` k2` t1`
  where
  merge :: !(Int Int (IntMap a) (IntMap a) -> IntMap a) !((IntMap b) c -> IntMap a)
           !((IntMap b) -> IntMap a) !(c -> IntMap a) !c !Int !(IntMap b) -> IntMap a
  merge bin` f g1 g2 t2 k2 t1=:(Bin p1 m1 l1 r1)
    | nomatch k2 p1 m1 = maybe_link p1 (g1 t1) k2 (g2 t2)
    | zero k2 m1 = bin` p1 m1 (merge bin` f g1 g2 t2 k2 l1) (g1 r1)
    | otherwise  = bin` p1 m1 (g1 l1) (merge bin` f g1 g2 t2 k2 r1)
  merge bin` f g1 g2 t2 k2 t1=:(Tip k1 _)
    | k1 == k2 = f t1 t2
    | otherwise = maybe_link k1 (g1 t1) k2 (g2 t2)
  merge bin` f g1 g2 t2 _  Nil = g2 t2

mergeWithKey` bin` f g1 g2 t1=:(Bin _ _ _ _) Nil = g1 t1

mergeWithKey` bin` f g1 g2 t1`=:(Tip k1` _) t2` = merge bin` f g1 g2 t1` k1` t2`
  where
  merge :: !(Int Int (IntMap a) (IntMap a) -> IntMap a) !(b (IntMap c) -> IntMap a)
           !(b -> IntMap a) !((IntMap c) -> IntMap a) !b !Int !(IntMap c) -> IntMap a
  merge bin` f g1 g2 t1 k1 t2=:(Bin p2 m2 l2 r2)
    | nomatch k1 p2 m2 = maybe_link k1 (g1 t1) p2 (g2 t2)
    | zero k1 m2 = bin` p2 m2 (merge bin` f g1 g2 t1 k1 l2) (g2 r2)
    | otherwise  = bin` p2 m2 (g2 l2) (merge bin` f g1 g2 t1 k1 r2)
  merge bin` f g1 g2 t1 k1 t2=:(Tip k2 _)
    | k1 == k2 = f t1 t2
    | otherwise = maybe_link k1 (g1 t1) k2 (g2 t2)
  merge bin` f g1 g2 t1 _  Nil = g1 t1

mergeWithKey` bin` f g1 g2 Nil t2 = g2 t2

maybe_link :: !Int !(IntMap a) !Int !(IntMap a) -> IntMap a
maybe_link _ Nil _ t2 = t2
maybe_link _ t1 _ Nil = t1
maybe_link p1 t1 p2 t2 = link p1 t1 p2 t2

// | /O(n)/. Convert the map to a list of key\/value pairs. Subject to list
// fusion.
//
// > toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
// > toList empty == []
toList :: !(IntMap a) -> [(Int, a)]
toList m = toAscList m

// | /O(n)/. Convert the map to a list of key\/value pairs where the
// keys are in ascending order. Subject to list fusion.
//
// > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
toAscList :: !(IntMap a) -> [(Int, a)]
toAscList m = foldrWithKey (\k x xs -> [(k,x):xs]) [] m
