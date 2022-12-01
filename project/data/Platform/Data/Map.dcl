definition module Data.Map

/**
 * This module provides a dynamic Map type for creating mappings from keys to values
 * Internally it uses an AVL tree to organize the key-value pairs stored in the mapping
 * such that lookup, insert and delete operations can be performed in O(log n).
 *
 * Ported from Haskell's Data.Map by Jurriën Stutterheim, 10-09-2014.
 *
 * @property-bootstrap
 *     import StdBool, StdChar, StdInt, StdString, StdTuple
 *     from StdList import all, isMember, removeDup, reverse, instance length []
 *     from Data.Func import on, `on`
 *     import Data.GenDefault
 *     from Data.List import nubBy, instance Functor [!]
 *     import Data.Maybe
 *
 *     :: Predicate a = ConstTrue | IsMember [a]
 *
 *     pred :: (Predicate a) a -> Bool | Eq a
 *     pred ConstTrue     _ = True
 *     pred (IsMember cs) c = isMember c cs
 *
 *     :: GMap k v =
 *         { gma  :: !v
 *         , gmb  :: !v
 *         , gmc  :: !v
 *         , gmd  :: !v
 *         , gme  :: !v
 *         , gmf  :: !v
 *         , rest :: ![(k,v)]
 *         }
 *
 *     class Key k
 *     where keya :: k; keyb :: k; keyc :: k; keyd :: k; keye :: k; keyf :: k
 *
 *     instance Key Char
 *     where keya = 'a'; keyb = 'b'; keyc = 'c'; keyd = 'd'; keye = 'e'; keyf = 'f'
 *
 *     derive class Gast GMap, Predicate
 *     derive genShow Map, ?
 *     derive gPrint Map, ?
 *
 *     kvs :: (GMap k v) -> [(k,v)] | Key k
 *     kvs gm =
 *         [ (keya,gm.gma)
 *         , (keyb,gm.gmb)
 *         , (keyc,gm.gmc)
 *         , (keyd,gm.gmd)
 *         , (keye,gm.gme)
 *         , (keyf,gm.gmf)
 *         : gm.rest
 *         ]
 *
 *     all_present :: [(k,v)] (Map k v) -> Bool | <, == k & == v
 *     all_present kvs m = all (\(k,v) -> get k m == ?Just v) kvs`
 *     where
 *         kvs` = nubBy ((==) `on` fst) (reverse kvs) // Remove duplicate keys, assuming the last takes precedence
 *
 *     all_from :: (Map k v) [(k,v)] -> Bool | Eq k & Eq v
 *     all_from Tip _ = True
 *     all_from (Bin _ k v l r) kvs = isMember (k,v) kvs && all_from l kvs && all_from r kvs
 *
 * @property-test-with k = Char
 * @property-test-with v = Char
 *
 * @property-test-generator (GMap k v) -> Map k v | Key, <, == k
 *     gen gm = fromList (kvs gm)
 */

from StdOverloaded	import class ==, class <
from StdBool        import not
from StdFunc        import id
from Data.GenEq import generic gEq
from Data.GenLexOrd import generic gLexOrd, :: LexOrd
from Data.Monoid    import class Monoid, class Semigroup
import qualified StdList
from Data.Functor import class Functor (..)
from Data.Set import :: Set
from StdOverloaded import class < (..)
import StdClass

/**
 * The abstract Map type provides the mapping.
 * For example "Map Int String" is a mapping "from" integers "to" strings.
 *
 * Notes on performance:
 * - For maps from `Int`, you can use Data.IntMap in many cases.
 * - Avoid using tuples as keys, since strictness information is lost in their
 *   `<` instance. Instead, define a (local) record with strict fields where
 *   appropriate.
 *
 * @var The key type on which the data structure is indexed.
 * @var The type of the values stored in the mapping.
 *
 * @invariant integrity: A.m :: Map k v:
 *     log_size m /\
 *     sizes_correct m
 *
 * @invariant log_size: A.m :: Map k v:
 *     check (<) nelem (2 ^ depth m)
 *     where
 *         nelem = mapSize m
 *
 *         depth :: (Map a b) -> Int
 *         depth Tip = 0
 *         depth (Bin _ _ _ l r) = 1 + (max `on` depth) l r
 *
 * @invariant sizes_correct: A.m :: Map k v:
 *     case m of
 *         Tip                -> prop True
 *         b=:(Bin _ _ _ l r) ->
 *             mapSize b =.= 1 + mapSize l + mapSize r /\
 *             sizes_correct l /\
 *             sizes_correct r
 */
:: Map k v
  = Bin !Int !k !v !(Map k v) !(Map k v)
  | Tip

instance Monoid (Map k v) | < k
instance Semigroup (Map k v) | < k where
	mappend :: !(Map k v) !(Map k v) -> Map k v | < k

instance == (Map k v) | Eq k  & Eq v
instance <  (Map k v) | Ord k & Ord v

//Basic functions

/**
 * Check if a Map is empty.
 * @type (Map k a) -> Bool
 * @property equivalence with size 0: A.m :: Map k v:
 *     mapSize m == 0 <==> null m
 * @property equivalence with newMap: A.m :: Map k v:
 *     m == newMap <==> null m
 * @complexity O(1)
 */
null mp :== case mp of
              Tip -> True
              _   -> False

/**
 * Create an empty Map.
 * @result An empty map
 * @property is null:
 *     null newMap
 * @complexity O(1)
 */
newMap      :: w:(Map k u:v), [ w <= u]

/**
 * Create a Map with one element.
 * @complexity O(1)
 */
singleton :: !k !v:v -> u:Map k v:v, [u <= v]

/**
 * The number of elements in a Map.
 * @property correctness: A.m :: Map k v:
 *     mapSize m =.= length (removeDup (keys m))
 */
mapSize     :: !(Map k v) -> Int

/**
 * The number of elements in a possibly unique Map.
 * @property corresponds to mapSize: A.m :: Map k v:
 *     fst (mapSizeU m) =.= mapSize m
 */
mapSizeU :: !u:(Map k v:v) -> (!Int, !u:Map k v:v), [u <= v]

/**
 * Adds or replaces the value for a given key.
 *
 * @param The key value to add/update
 * @param The value to add/update at the key position
 * @param The original mapping
 * @result The modified mapping with the added value
 * @property correctness: A.m :: Map k v; k :: k; v :: v:
 *     get k m` =.= ?Just v /\                                              // Correctly put
 *         check all_present [kv \\ kv=:(k`,_) <- toList m | k <> k`] m` /\ // Other elements untouched
 *         integrity m`
 *     where
 *         m` = put k v m
 * @complexity O(log n)
 */
put :: !k !v:v !u:(Map k v:v) -> u:Map k v:v | < k
	special k=Int; k=String

/**
 * Searches for a value at a given key position. Works only for non-unique
 * mappings.
 *
 * @type k (Map k v) -> ?v | < k
 * @param The key to look for
 * @param The orginal mapping
 * @result When found, the value at the key position; if not, `?None`
 * @complexity O(log n)
 */
get k m :== get` k m
where
	get` k (Bin _ kx x l r)
		| k < kx    = get` k l
		| k > kx    = get` k r
		| otherwise = ?Just x
	get` _ Tip = ?None

/**
 * Searches for a value at a given key position. Works also for unique mappings.
 */
getU :: !k !w:(Map k v) -> x:(!?v, !y:(Map k v)) | == k & < k, [ x <= y, w <= y]
	special k=Int; k=String

/**
 * Removes the value at a given key position. The mapping itself can be spine unique.
 *
 * @param The key to remove
 * @param The original mapping
 * @result The modified mapping with the value/key removed
 * @property correctness: A.m :: Map k v; k :: k:
 *     get k m` =.= ?None /\                                                // Correctly deleted
 *         check all_present [kv \\ kv=:(k`,_) <- toList m | k <> k`] m` /\ // Other elements untouched
 *         integrity m`
 *     where
 *         m` = del k m
 */
del :: !k !(Map k a) -> Map k a | < k
	special k=Int; k=String

/**
 * Removes the value at a given key position. The mapping can be unique.
 */
delU :: !k !w:(Map k u:v) -> x:(?u:v, !y:(Map k u:v)) | == k & < k, [ w y <= u, x <= y, w <= y]
	special k=Int; k=String

/**
 * Folds the keys and values in the map using the given right-associative
 * binary operator, such that
 * `foldrWithKey f z == foldr (uncurry f) z o toAscList`.
 * @complexity O(n)
 */
foldrWithKey :: !(k v u:a -> u:a) !u:a !(Map k v) -> u:a

//* Like `foldrWithKey`, but with strict application of the operator.
foldrWithKey` :: !(k v u:a -> u:a) !u:a !(Map k v) -> u:a

/**
 * Folds the keys and values in the map using the given left-associative binary
 * operator, such that
 * `` foldlWithKey f z == foldl (\z` (kx,x) -> f z` kx x) z o toAscList ``.
 * @complexity O(n)
 */
foldlWithKey :: !(.a -> .(k -> .(v -> .a))) !.a !(Map k v) -> .a

//* Like `foldlWithKey`, but with strict application of the operator.
foldlWithKey` :: !(.a -> .(k -> .(v -> .a))) !.a !(Map k v) -> .a

//* @type (v a -> a) a (Map k v) -> a
foldrNoKey f x m :== foldrWithKey (\_ v acc -> f v acc) x m

//* @type (a v -> a) a (Map k v) -> a
foldlNoKey f x m :== foldlWithKey (\acc _ v -> f acc v) x m

/**
 * Filter elements in a Map.
 *
 * @param The predicate function.
 * @param The Map.
 * @result A new Map that contains exactly those pairs (k,v) from the original Map for which p k v holds.
 * @complexity O(n)
 */
filterWithKey :: !(k v -> Bool) !(Map k v) -> Map k v

/**
 * A list of the keys in a Map.
 * @type (Map k v) -> [k]
 */
keys m :== foldrWithKey (\k _ ks -> [k : ks]) [] m

/**
 * A list of the elements in a Map.
 * @type (Map k v) -> [v]
 */
elems m :== foldrNoKey (\x xs -> [x:xs]) [] m

//Conversion functions

/**
 * Converts a mapping to a list of key value pairs.
 * Because of the internal ordering of the mapping the resulting
 * list is sorted ascending on the key part of the tuple.
 *
 * @type (Map k v) -> [(k,v)]
 * @param The original mapping
 * @result A list of key/value tuples in the mapping
 */
toList m :== toAscList m

/**
 * Same as toList.
 * @type (Map k v) -> [(k,v)]
 */
toAscList m :== foldrWithKey (\k x xs -> [(k,x):xs]) [] m

/**
 * Converts a list of key/value tuples to a mapping.
 *
 * @param A list of key/value tuples
 * @result A mapping containing all the tuples in the list
 * @property correctness: A.elems :: [(k,v)]:
 *     check all_present elems m /\ // All elements put
 *         check all_from m elems /\  // No other elements
 *         integrity m
 *     where
 *         m = fromList elems
 * @complexity O(n*log n)
 */
fromList :: !u:[v:(a, b)] -> Map a b | == a & < a, [u <= v]
	special a=Int; a=String

/**
 * The keys of all keys of a map.
 * @complexity log(n)
 */
keysSet :: !(Map k a) -> Set k

/**
 * Build a map from a set of keys and a function which for each key computes its value.
 * @complexity log(n)
 */
fromSet :: !(k -> a) !(Set k) -> Map k a

derive gEq Map
derive gLexOrd Map

/**
 * Check if a key exists in a Map.
 * @property correctness: A.k :: k; m :: Map k v:
 *     member k m <==> isMember k (keys m)
 * @complexity O(log n)
 */
member :: !k !(Map k a) -> Bool | < k
	special k=Int; k=String

/**
 * Checks if a key is not a member of a Map.
 * @type k (Map k v) -> Bool | < k
 * @property correctness: A.k :: k; m :: Map k v:
 *     notMember k m <==> not (isMember k (keys m))
 */
notMember k m :== not (member k m)

/**
 * Find an element in a Map.
 * Aborts when the element is not found.
 * @property correctness: A.k :: k; v :: v; m :: Map k v:
 *     find k (put k v m) =.= v
 * @complexity O(log n)
 */
find :: !k !(Map k a) -> a | < k
	special k=Int; k=String

/**
 * Find an element in a Map.
 * When the key does not exist, return a default.
 *
 * @param The default.
 * @param The key to look up.
 * @property correctness: A.k :: k; v :: v; m :: Map k v:
 *     findWithDefault default k (put k v m) =.= v /\
 *         findWithDefault default k (del k m) =.= default
 *     where default = gDefault{|*|}
 * @complexity O(log n)
 */
findWithDefault :: !a !k !(Map k a) -> a | < k
	special k=Int; k=String


/**
 * Find the `?Just key` of an element in a Map.
 * When the element does not exist, return `?None`.
 *
 * @param The element you're looking for.
 * @property correctness: A.v :: v; m :: Map k v:
 *     case [k \\ (k,v`) <- toList m | v == v`] of
 *         []    -> findKey v m =.= ?None
 *         [k:_] -> findKey v m =.= ?Just k
 * @complexity O(n)
 */
findKey :: !a !(Map k a) -> ?k | == a

/**
 * Find a `?Just key` of an element in a Map, for which the function yields True.
 * When the element does not exist, return `?None`.
 *
 * @param The search function for checking values in the Map.
 * @property correctness: A.p :: Predicate v; m :: Map k v:
 *     case [k \\ (k,v) <- toList m | pred p v] of
 *         []    -> findKeyWith (pred p) m =.= ?None
 *         [k:_] -> findKeyWith (pred p) m =.= ?Just k
 * @complexity O(n)
 */
findKeyWith :: !(a -> Bool) !(Map k a) -> ?k
	special k=Int; k=String

/**
 * Find the key of an element in a Map.
 * If the element is not a member, return the first parameter.
 *
 * @param The result if the second parameter does not occur as a value in the Map.
 * @param The element you're looking for.
 * @property correctness: A.v :: v; m :: Map k v:
 *     case findKey v m of
 *         ?None   -> findKeyWithDefault default v m =.= default
 *         ?Just k -> findKeyWithDefault default v m =.= k
 *     where default = gDefault{|*|}
 * @complexity O(n)
 */
findKeyWithDefault :: !k !a !(Map k a) -> k | == a

/**
 * Find the key of an element in a Map, for which the function yields True.
 * If the element is not a member, return the second parameter.
 *
 * @param The search function for checking values in the Map.
 * @param The result when all values in the Map check as False.
 * @property correctness: A.p :: Predicate v; m :: Map k v:
 *     case findKeyWith (pred p) m of
 *         ?None   -> findKeyWithDefaultWith (pred p) default m =.= default
 *         ?Just k -> findKeyWithDefaultWith (pred p) default m =.= k
 *     where default = gDefault{|*|}
 * @complexity O(n)
 */
findKeyWithDefaultWith :: !(a -> Bool) !k !(Map k a) -> k

/**
 * Put, delete, or update a value in a map. The update function receives or
 * returns a `?None` to signal absence of a value.
 * @complexity O(log n)
 */
alter :: !((?a) -> ?a) !k !(Map k a) -> Map k a | < k
	special k=Int; k=String

/**
 * Get the index of a key in a Map so that it can be retrieved with
 * {{`elemAt`}}. The index is the zero-based index into the sequence sorted by
 * keys.
 */
getIndex :: !k !(Map k a) -> ?Int | < k
	special k=Int; k=String

/**
 * Get the entry at a certain index. To get an index for a certain key, see
 * {{`getIndex`}}.
 */
elemAt :: !Int !(Map k a) -> ?(!k, !a)

/**
 * Update an entry at a certain index. To get an index for a certain key, see
 * {{`getIndex`}}.
 *
 * @param The update function
 * @param The index
 * @param The map
 * @result The new map, or `?None` in case of an index out of range
 */
updateAt :: !(k a -> ?a) !Int !(Map k a) -> ?(Map k a)

/**
 * Finds the minimal key and value of the map, or `abort`s if the map has no
 * elements.
 * @complexity O(log n)
 */
findMin :: !(Map k a) -> (!k, !a)

/**
 * Finds the maximal key and value of the map, or `abort`s if the map has no
 * elements.
 * @complexity O(log n)
 */
findMax :: !(Map k a) -> (!k, !a)

unions :: ![Map k a] -> Map k a | < k
	special k=Int; k=String

unionsWith :: !(a a -> a) ![Map k a] -> Map k a | < k
	special k=Int; k=String

unionWith :: !(a a -> a) !(Map k a) !(Map k a) -> Map k a | < k
	special k=Int; k=String

unionWithKey :: !(k a a -> a) !(Map k a) !(Map k a) -> Map k a | < k
	special k=Int; k=String

/**
 * Compute the left-biased intersection of two maps: a map containing all
 * key-value pairs from the first map of which the key also exists in the
 * second map.
 * @complexity O(m+n)
 */
intersection :: !(Map k a) !(Map k b) -> Map k a | < k
	special k=Int; k=String

/**
 * Like {{`intersection`}}, but with a function to combine values.
 * Also see {{`intersectionWithKey`}}.
 */
intersectionWith :: !(a b -> c) !(Map k a) !(Map k b) -> Map k c | < k
	special k=Int; k=String

/**
 * Like {{`intersection`}}, but with a function to combine values.
 * If the key is not needed to combine values, use {{`intersectionWith`}}.
 */
intersectionWithKey :: !(k a b -> c) !(Map k a) !(Map k b) -> Map k c | < k
	special k=Int; k=String

/**
 * Computes the left-biased union of two maps. This means that the elements of
 * the left map are preferred when a key is present in both maps.
 * @complexity O(m+n)
 */
union :: !(Map k a) !(Map k a) -> Map k a | < k
	special k=Int; k=String

mergeWithKey :: !(k a b -> ?c) !((Map k a) -> Map k c) !((Map k b) -> Map k c)
             !(Map k a) !(Map k b) -> Map k c | < k
	special k=Int; k=String

/**
 * Removes the values at given key positions. The mapping itself can be spine unique.
 *
 * @type [a] (Map a b) -> Map a b | <, == a
 * @param The list of keys to remove
 * @param The original mapping
 * @result The modified mapping with the values/keys removed
 */
delList xs m :== 'StdList'.foldr (\k m -> del k m) m xs

/**
 * Adds or replaces a list of key/value pairs.
 *
 * @type [(a, b)] (Map a b) -> Map a b | ==, < a
 * @param A list of key/value tuples
 * @param The original mapping
 * @result The modified mapping with the added values
 */
putList xs m :== union (fromList xs) m

instance Functor (Map k)
where
	fmap :: !(a -> b) !(Map k a) -> Map k b

/**
 * Computes the difference of two maps: a map containing all key-value pairs
 * from the first map except those of which the key is present in the second
 * map as well.
 * @complexity O(m+n)
 */
difference :: !(Map k a) !(Map k b) -> Map k a | < k
	special k=Int; k=String

/**
 * Map a function over all values in a map.
 * @complexity O(n)
 */
mapWithKey :: !(k a -> b) !(Map k a) -> Map k b

//* This function is defined as `{{isSubmapOfBy}} ({{==}})`.
isSubmapOf :: !(Map k a) !(Map k a) -> Bool | < k & == a

/**
 * The expression `isSubmapOfBy f m1 m2` returns `True` if all keys in `m1` are
 * in `m2`, and when `f` returns `True` when applied to their respective
 * values.
 * @complexity O(m+n)
 */
isSubmapOfBy :: !(a b -> Bool) !(Map k a) !(Map k b) -> Bool | < k
	special k=Int; k=String
