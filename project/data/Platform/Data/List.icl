implementation module Data.List

import StdArray
import StdBool
import StdEnum
import StdFunctions
from StdOverloadedList import class List, instance List [!] e, instance List [ !] e, instance List [!!] e, instance List [#] e, instance List [#!] e, ++|, Foldr
from StdList import ++, prod, isMember, any, scan, last, filter, zip, hd, tl, isEmpty, span, drop, map, flatten, repeat, take, reverse, zip2, instance length []
import qualified StdList
import StdMisc
import StdOrdList
import StdTuple
import _SystemStrictLists

import Data.Func
import Data.Functor
import Data.GenEq
import Data.Maybe
import Data.Monoid
from Data.Foldable import class Foldable(..)
from Data.Traversable import class Traversable(..)
import Control.Applicative
from Control.Monad import class Monad(..)
from Control.Monad.Fail import class MonadFail(..)

instance Functor []
where
	fmap :: (a -> b) ![a] -> [b]
	fmap f l = [f e \\ e <- l]

instance pure []
where
	pure x      = [x]

instance <*> []
where
	(<*>) fs xs = [f x\\f<-fs, x<-xs]

instance Alternative [] where
	empty        = []
	(<|>) fa fa` = fa ++ fa`

instance Monad []
where
	bind m k = 'StdList'.foldr ((++) o k) [] m

instance MonadPlus []
where
	mzero        = []
	mplus xs ys = xs ++ ys

instance MonadFail []
where
	fail _ = []

instance Semigroup [a]
where
	mappend xs ys  = xs ++ ys

instance Monoid [a]
where
	mempty = []

instance Foldable []
where
	foldr a b c = 'StdList'.foldr a b c
	foldr` f x y = strictFoldr f x y
	where
		strictFoldr :: (.a -> .(.b -> .b)) !.b ![.a] -> .b
		strictFoldr _ b []     = b
		strictFoldr f b [x:xs] = f x (strictFoldr f b xs)

	foldl` f x y = strictFoldl f x y
	where
		strictFoldl :: (.a -> .(.b -> .a)) !.a ![.b] -> .a
		strictFoldl _ b []     = b
		strictFoldl f b [x:xs] = strictFoldl f (f b x) xs

instance Traversable []
where
	traverse f x = foldr (\x ys->(\x xs->[x:xs]) <$> f x <*> ys) (pure []) x

instance +++ [a]
where
	(+++) a b = a ++ b

instance Functor [!] where
	fmap :: (a -> b) ![!a] -> [!b]
	fmap f l = [!f e \\ e <|- l]

instance pure [!]
where
	pure :: !a -> [!a]
	pure x = [!x]

instance <*> [!]
where
	(<*>) fs xs = [!f x \\ f <|- fs, x <|- xs]

instance Alternative [!] where
	empty        = [!]
	(<|>) fa fa` = fa ++| fa`

instance Monad [!]
where
	bind m k = Foldr ((++|) o k) [!] m

instance MonadPlus [!]
where
	mzero       = [!]
	mplus xs ys = xs ++| ys

instance MonadFail [!]
where
	fail _ = [!]

instance Semigroup [!a]
where
	mappend xs ys  = xs ++| ys

instance Monoid [!a]
where
	mempty = [!]

instance Foldable [!]
where
	foldr a b c = Foldr a b c
	foldr` f x y = strictFoldr f x y
	where
		strictFoldr :: (.a -> .(.b -> .b)) !.b ![!.a] -> .b
		strictFoldr _ b [|]     = b
		strictFoldr f b [|x:xs] = f x (strictFoldr f b xs)

	foldl` f x y = strictFoldl f x y
	where
		strictFoldl :: (.a -> .(.b -> .a)) !.a ![!.b] -> .a
		strictFoldl _ b [|]     = b
		strictFoldl f b [|x:xs] = strictFoldl f (f b x) xs

instance Traversable [!]
where
	traverse f x = foldr (\x ys->(\x xs->[!x:xs]) <$> f x <*> ys) (pure [!]) x

instance Functor [ !] where
	fmap :: (a -> b) ![a!] -> [b!]
	fmap f l = [f e \\ e <|- l!]

instance pure [ !]
where
	pure x = [x!]

instance <*> [ !]
where
	(<*>) fs xs = [f x \\ f <|- fs, x <|- xs!]

instance Alternative [ !] where
	empty        = [ !]
	(<|>) fa fa` = fa ++| fa`

instance Monad [ !]
where
	bind m k = Foldr ((++|) o k) [ !] m

instance MonadPlus [ !]
where
	mzero       = [ !]
	mplus xs ys = xs ++| ys

instance MonadFail [ !]
where
	fail _ = [ !]

instance Semigroup [a!]
where
	mappend xs ys = xs ++| ys

instance Monoid [a!]
where
	mempty = [ !]

instance Foldable [ !]
where
	foldr a b c = Foldr a b c
	foldr` f x y = strictFoldr f x y
	where
		strictFoldr :: (.a -> .(.b -> .b)) !.b ![.a!] -> .b
		strictFoldr _ b [|]     = b
		strictFoldr f b [|x:xs] = f x (strictFoldr f b xs)

	foldl` f x y = strictFoldl f x y
	where
		strictFoldl :: (.a -> .(.b -> .a)) !.a ![.b!] -> .a
		strictFoldl _ b [|]     = b
		strictFoldl f b [|x:xs] = strictFoldl f (f b x) xs

instance Traversable [ !]
where
	traverse f x = foldr (\x ys->(\x xs->[x:xs!]) <$> f x <*> ys) (pure [ !]) x

instance Functor [!!] where
	fmap :: (a -> b) ![!a!] -> [!b!]
	fmap f l = [!f e \\ e <|- l!]

instance pure [!!]
where
	pure :: !a -> [!a!]
	pure x = [!x!]

instance <*> [!!]
where
	(<*>) fs xs = [!f x \\ f <|- fs, x <|- xs!]

instance Alternative [!!] where
	empty        = [!!]
	(<|>) fa fa` = fa ++| fa`

instance Monad [!!]
where
	bind m k = Foldr ((++|) o k) [!!] m

instance MonadPlus [!!]
where
	mzero       = [!!]
	mplus xs ys = xs ++| ys

instance MonadFail [!!]
where
	fail _ = [!!]

instance Semigroup [!a!]
where
	mappend xs ys = xs ++| ys

instance Monoid [!a!]
where
	mempty = [!!]

instance Foldable [!!]
where
	foldr a b c = Foldr a b c
	foldr` f x y = strictFoldr f x y
	where
		strictFoldr :: (.a -> .(.b -> .b)) !.b ![!.a!] -> .b
		strictFoldr _ b [|]     = b
		strictFoldr f b [|x:xs] = f x (strictFoldr f b xs)

	foldl` f x y = strictFoldl f x y
	where
		strictFoldl :: (.a -> .(.b -> .a)) !.a ![!.b!] -> .a
		strictFoldl _ b [|]     = b
		strictFoldl f b [|x:xs] = strictFoldl f (f b x) xs

instance Traversable [!!]
where
	traverse f x = foldr (\x ys->(\x xs->[!x:xs!]) <$> f x <*> ys) (pure [!!]) x

(!?) infixl 9   :: ![.a] !Int -> ? .a
(!?) [x:_]  0 = ?Just x
(!?) [_:xs] i = xs !? (i-1)
(!?) _      _ = ?None

// Haskell Data.List compat

head :: ![.a] -> .a
head xs = hd xs

tail :: !u:[.a] -> u:[.a]
tail xs = tl xs

isnull :: ![.a] -> Bool
isnull xs = isEmpty xs

product :: !.[a] -> a | * , one  a
product xs = prod xs

// End Haskell Data.List compat

keep :: !Int ![a] -> [a]
keep n xs = drop (length xs - n) xs

unzip3 :: ![(.a,.b,.c)] -> ([.a],[.b],[.c])
unzip3 []        = ([], [], [])
unzip3 [(x,y,z) : xyzs]  = ([x : xs],[y : ys],[z : zs])
where
  (xs,ys,zs) = unzip3 xyzs

unzip4 :: ![(.a,.b,.c,.d)] -> ([.a],[.b],[.c],[.d])
unzip4 []          = ([], [], [], [])
unzip4 [(w,x,y,z) : wxyzs]  = ([w : ws],[x : xs],[y : ys],[z : zs])
where
  (ws,xs,ys,zs) = unzip4 wxyzs
unzip5 :: ![(.a,.b,.c,.d,.e)] -> ([.a],[.b],[.c],[.d],[.e])
unzip5 []            = ([], [], [], [], [])
unzip5 [(v,w,x,y,z) : vwxyzs]  = ([v : vs],[w : ws],[x : xs],[y : ys],[z : zs])
where
  (vs,ws,xs,ys,zs) = unzip5 vwxyzs

replaceInList :: !(a a -> Bool) !a ![a] -> [a]
replaceInList _ _ []         = []
replaceInList cond new [x:xs]
    | cond new x            = [new : xs]
    | otherwise             = [x : replaceInList cond new xs]

sortByIndex :: ![(Int,a)] -> [a]
sortByIndex l = map snd (sortBy (\(a,_) (b,_) -> a < b) l)

intersperse :: !a ![a] -> [a]
intersperse i []      = []
intersperse i [x]     = [x]
intersperse i [x:xs]  = [x,i:intersperse i xs]

intercalate :: !.[a] ![.[a]] -> .[a]
intercalate xs xss = flatten (intersperse xs xss)

transpose :: ![[a]] -> [.[a]]
transpose []  = []
transpose [[]     : xss] = transpose xss
transpose [[x:xs] : xss] = [[x : [h \\ [h:t] <- xss]] : transpose [xs : [t \\ [h:t] <- xss]]]

subsequences :: .[a] -> .[[a]]
subsequences xs = [[] : nonEmptySubsequences xs]

nonEmptySubsequences :: .[a] -> .[[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences [x:xs]  =  [[x] : 'StdList'.foldr f [] (nonEmptySubsequences xs)]
  where f ys r = [ys : [x : ys] : r]

permutations :: [a] -> .[[a]]
permutations xs0        =  [xs0 : perms xs0 []]
  where
    perms []     _  = []
    perms [t:ts] is = 'StdList'.foldr interleave (perms ts [t:is]) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave` id xs r in zs
            interleave` _ []     r = (ts, r)
            interleave` f [y:ys] r = let (us,zs) = interleave` (f o (\xs -> [y:xs])) ys r
                                     in  ([y:us], [f [t:y:us] : zs])

concatMap :: (.a -> [.b]) ![.a] -> [.b]
concatMap f ls = flatten (map f ls)

getItems :: ![a] ![Int] -> [a]
getItems list indexes = [x \\ x <- list & idx <- [0..] | isMember idx indexes]

scanl :: (a -> .(.b -> a)) a [.b] -> .[a]
scanl f q ls            =  [q : (case ls of
                                  []     -> []
                                  [x:xs] -> scanl f (f q x) xs)]

scanl1 :: (a -> .(a -> a)) !.[a] -> .[a]
scanl1 f [x:xs]         =  scanl f x xs
scanl1 _ []             =  []

replicate :: !.Int a -> .[a]
replicate n x           =  take n (repeat x)

cycle :: !.[a] -> [a]
cycle xs                = xs`
  where xs` = xs ++ xs`

unfoldr :: !(.a -> ?(.b,.a)) .a -> [.b]
unfoldr f b  = case f b of
	?Just (a,new_b) -> [a : unfoldr f new_b]
	?None           -> []

break :: (a -> .Bool) !.[a] -> .([a],[a])
break _ xs=:[]           =  (xs, xs)
break p xs=:[x:xs`]
           | p x        =  ([],xs)
           | otherwise  =  let (ys,zs) = break p xs` in ([x:ys],zs)

stripPrefix :: !.[a] u:[a] -> ?v:[a] | == a, [u <= v]
stripPrefix [] ys = ?Just ys
stripPrefix [x:xs] [y:ys]
 | x == y = stripPrefix xs ys
stripPrefix _ _ = ?None

group :: .(.[a] -> [.[a]]) | == a
group                   =  groupBy (==)

groupBy :: (a -> a -> .Bool) !.[a] -> [.[a]]
groupBy _  []           =  []
groupBy eq [x:xs]       =  [[x:ys] : groupBy eq zs]
                           where (ys,zs) = span (eq x) xs

inits :: .[a] -> [.[a]]
inits xs                =  [[] : case xs of
                                  []        -> []
                                  [x : xs`] -> map (\ys -> [x : ys]) (inits xs`)]

tails :: [a] -> .[[a]]
tails xs                =  [xs : case xs of
                                  []        -> []
                                  [_ : xs`] -> tails xs`]

isPrefixOf :: !.[a] .[a] -> .Bool | == a
isPrefixOf [] _          = True
isPrefixOf _  []         = False
isPrefixOf [x:xs] [y:ys] = x == y && isPrefixOf xs ys
isPrefixOf _      _      = abort "error in isPrefixOf\n"

isSuffixOf :: !.[a] .[a] -> .Bool | == a
isSuffixOf x y          =  isPrefixOf (reverse x) (reverse y)

isInfixOf :: .[a] .[a] -> Bool | == a
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

// optimised algorithm x from The String-to-String Correction Problem, Robert A. Wagner and Michael J.Fischer
levenshtein :: !.[a] !.[a] -> Int | == a;
levenshtein xs ys
	# n_x = length xs;
	# ya = {!y \\ y<-ys}; // == is strict
	# n_y = size ya;
	# d_a1 = {#i \\ i<-[0..n_y]};
	# d_a2 = createArray (n_y+1) 0;
	// d_a1 contains row x_i distances and d_a2 row x_i+1 distances
	= levenshtein_1 0 xs d_a1 d_a2 ya;

levenshtein_1 :: !Int ![a] !*{#Int} !*{#Int} !{!a} -> Int | == a;
levenshtein_1 x_i [x:xs] d_a1 d_a2 ya
	# d_a2 & [0] = x_i+1;
	# (d_a2,d_a1) = levenshtein_2 0 x_i d_a1 d_a2 x ya; // swaps d_a1 and d_a2
	= levenshtein_1 (x_i+1) xs d_a1 d_a2 ya;
levenshtein_1 x_i [] d_a1 d_a2 ya
	= d_a1.[size d_a2-1];

levenshtein_2 :: !Int !Int !*{#Int} !*{#Int} !a !{!a} -> (!*{#Int},!*{#Int}) | == a;
levenshtein_2 y_i x_i d_a1 d_a2 x ya
	| y_i<size ya
		| ya.[y_i]==x
			#! d = d_a1.[y_i];
			# y_i=y_i+1;
			= levenshtein_2 y_i x_i d_a1 {d_a2 & [y_i] = d} x ya;
			#! a = d_a1.[y_i+1];
			#! b = d_a2.[y_i];
			| a<b
				#! c = d_a1.[y_i];
				# y_i=y_i+1;
				| c<a
					= levenshtein_2 y_i x_i d_a1 {d_a2 & [y_i] = c+1} x ya;
					= levenshtein_2 y_i x_i d_a1 {d_a2 & [y_i] = a+1} x ya;
				#! c = d_a1.[y_i];
				# y_i=y_i+1;
				| c<b
					= levenshtein_2 y_i x_i d_a1 {d_a2 & [y_i] = c+1} x ya;
					= levenshtein_2 y_i x_i d_a1 {d_a2 & [y_i] = b+1} x ya;
		= (d_a1,d_a2);

elem :: a !.[a] -> .Bool | == a
elem _ []       = False
elem x [y:ys]   = x == y || elem x ys

notElem :: a !.[a] -> .Bool | == a
notElem _ []     =  True
notElem x [y:ys] =  x <> y && notElem x ys

lookup :: a ![(a,.b)] -> ? .b | == a
lookup _   []          = ?None
lookup key [(x,y):xys] = if (key == x) (?Just y) (lookup key xys)

find :: (a -> .Bool) -> .(.[a] -> . ?a)
find p = listToMaybe o filter p

// Several implementations are possible. We do not take these approaches:
// - Using `foldr` or naive recursion: causes stack overflows for larger lists.
// - Using `foldl` + reverse: requires the full list to be in memory.
// - Two independent list comprehensions: requires two applications of the
//   predicate.
// Instead, we store the results of the applications of the predicate in a
// separate list.
// - There is an extra memory cost as long as a reference to either result list
//   remains. But in this case `xs` is also retained in memory, so the memory
//   usage is linear with a fairly low constant (depending on the size of the
//   list elements).
// - The extra cost of building and iterating `matches` is assumed to be
//   outweighed by the cost of applying the predicate twice for all but the
//   most trivial predicates.
// Also observe that the result lists must not be strict to allow things like
// `hd $ fst $ partition ((>) 1000000) [0..]`.
partition :: !(a -> .Bool) !.(l a) -> (.l a, .l a) | List l a
partition p xs = ([|x \\ x <|- xs & True <|- matches], [|x \\ x <|- xs & False <|- matches])
where
	matches = [#p x \\ x <|- xs]

elemIndex :: a -> .(.[a] -> . ?Int) | == a
elemIndex x = findIndex (\y -> x==y)

elemIndices :: a -> .(.[a] -> .[Int]) | == a
elemIndices x   = findIndices (\y -> x==y)

findIndex :: (.a -> .Bool) -> .([.a] -> . ?Int)
findIndex p = listToMaybe o findIndices p

findIndices :: (.a -> .Bool) ![.a] -> .[Int]
findIndices p xs = [ i \\ (x,i) <- zip2 xs [0..] | p x]

zip3 :: ![.a] [.b] [.c] -> [(.a,.b,.c)]
zip3 [a:as] [b:bs] [c:cs]
  = [(a, b, c): zip3 as bs cs]
zip3 _ _ _
  = []

zip4 :: ![.a] [.b] [.c] [.d] -> [(.a,.b,.c,.d)]
zip4 [a:as] [b:bs] [c:cs] [d:ds]
  = [(a, b, c, d): zip4 as bs cs ds]
zip4 _ _ _ _
  = []

zip5 :: ![.a] [.b] [.c] [.d] [.e] -> [(.a,.b,.c,.d,.e)]
zip5 [a:as] [b:bs] [c:cs] [d:ds] [e:es]
  = [(a, b, c, d, e): zip5 as bs cs ds es]
zip5 _ _ _ _ _
  = []

zipWith :: (.a -> .(.b -> .h)) ![.a] [.b] -> [.h]
zipWith z [a:as] [b:bs]
                   = [ z a b : zipWith z as bs]
zipWith _ _ _ = []

zipSt :: (.a -> .(.b -> (.st -> .st))) ![.a] [.b] .st -> .st
zipSt z [a:as] [b:bs] st
  # st = z a b st
  = zipSt z as bs st
zipSt _ _ _ st = st

zipWithSt :: (.a -> .(.b -> (.st -> .(.h, .st)))) ![.a] [.b] .st -> .([.h], .st)
zipWithSt z [a:as] [b:bs] st
  # (x, st)  = z a b st
  # (xs, st) = zipWithSt z as bs st
  = ([x : xs], st)
zipWithSt _ _ _ st = ([], st)

zipWith3 :: (.a -> .(.b -> .(.c -> .h))) ![.a] [.b] [.c] -> [.h]
zipWith3 z [a:as] [b:bs] [c:cs]
                   = [ z a b c : zipWith3 z as bs cs]
zipWith3 _ _ _ _ = []

zipWith4 :: (.a -> .(.b -> .(.c -> .(.d -> .h))))
      ![.a] [.b] [.c] [.d] -> [.h]
zipWith4 z [a:as] [b:bs] [c:cs] [d:ds]
                   = [ z a b c d : zipWith4 z as bs cs ds]
zipWith4 _ _ _ _ _ = []

zipWith5 :: (.a -> .(.b -> .(.c -> .(.d -> .(.e -> .h)))))
      ![.a] [.b] [.c] [.d] [.e] -> [.h]
zipWith5 z [a:as] [b:bs] [c:cs] [d:ds] [e:es]
                   = [ z a b c d e : zipWith5 z as bs cs ds es]
zipWith5 _ _ _ _ _ _ = []

nub :: !.[a] -> .[a] | == a
nub l                   = nub` l []
  where
    nub` [] _           = []
    nub` [x:xs] ls
        | elem x  ls    = nub` xs ls
        | otherwise     = [x : nub` xs [x:ls]]

nubBy :: (a -> .(a -> .Bool)) !.[a] -> .[a]
nubBy eq l              = nubBy` l []
  where
    nubBy` [] _         = []
    nubBy` [y:ys] xs
       | elem_by eq y xs = nubBy` ys xs
       | otherwise       = [y : nubBy` ys [y:xs]]

elem_by :: (a -> .(.b -> .Bool)) a ![.b] -> .Bool
elem_by _  _ []         =  False
elem_by eq y [x:xs]     =  eq y x || elem_by eq y xs

delete :: u:(a -> v:(w:[a] -> x:[a])) | == a, [v <= u,w <= x]
delete                  =  deleteBy (==)

deleteBy :: (a -> .(b -> .Bool)) a !u:[b] -> v:[b], [u <= v]
deleteBy _  _ []        = []
deleteBy eq x [y:ys]    = if (eq x y) ys [y : deleteBy eq x ys]

deleteFirstsBy :: (a -> .(b -> .Bool)) -> u:(v:[b] -> w:(.[a] -> x:[b])), [w <= u,w v <= x]
deleteFirstsBy eq       =  foldl (flip (deleteBy eq))

difference :: u:(v:[a] -> w:(.[a] -> x:[a])) | == a, [w <= u,w v <= x]
difference                    =  differenceBy (==)

differenceBy :: (a -> a -> .Bool) !u:[a] !.[a] -> v:[a], [u <= v]
differenceBy eq as bs             =  foldl (flip (deleteBy eq)) as bs

intersect :: u:(.[a] -> v:(.[a] -> .[a])) | == a, [v <= u]
intersect               =  intersectBy (==)

intersectBy :: (a -> b -> .Bool) !.[a] !.[b] -> .[a]
intersectBy _  [] _     =  []
intersectBy _  _  []    =  []
intersectBy eq xs ys    =  [x \\ x <- xs | any (eq x) ys]

union :: u:(.[a] -> v:(.[a] -> .[a])) | == a, [v <= u]
union                   = unionBy (==)

unionBy :: (a -> .(a -> .Bool)) !.[a] .[a] -> .[a]
unionBy eq xs ys        =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

hasDup :: ![a] -> Bool | Eq a
hasDup []     = False
hasDup [x:xs] = isMember x xs || hasDup xs

isMemberGen :: !a !.[a] -> Bool | gEq{|*|} a
isMemberGen x [hd:tl]	= hd === x || isMemberGen x tl
isMemberGen x []		= False

strictTRMapRev :: !(.a -> .b) ![.a] -> [.b]
strictTRMapRev f xs = strictTRMapAcc f xs []

strictTRMapAcc :: !(u:a -> v:b) !w:[u:a] !x:[v:b] -> y:[v:b], [w <= u,y <= v,x <= y]
strictTRMapAcc f []     acc = acc
strictTRMapAcc f [x:xs] acc = strictTRMapAcc f xs [f x : acc]

strictTRMap :: !(.a -> .b) ![.a] -> [.b]
strictTRMap f xs = reverseTR (strictTRMapAcc f xs [])

reverseTR :: ![.a] -> [.a]
reverseTR xs = rev` xs []
  where
  rev` :: !u:[v:a] !w:[v:a] -> x:[v:a], [x u <= v,w <= x]
  rev` [] acc = acc
  rev` [x:xs] acc = rev` xs [x:acc]

flattenTR :: ![[a]] -> [a]
flattenTR xss = reverseTR (flattenTRAcc xss [])

flattenTRAcc :: ![[a]] [a] -> [a]
flattenTRAcc [] acc = acc
flattenTRAcc [xs:xss] acc
  #! r = reverseTR xs ++ acc
  = flattenTRAcc xss r

strictTRMapSt :: !(a .st -> (b, .st)) ![a] !.st -> (![b], !.st)
strictTRMapSt f xs st
  #! (rs, st) = strictTRMapStAcc f xs [] st
  = (reverseTR rs, st)

strictTRMapStAcc :: !(a .st -> (b, .st)) ![a] ![b] !.st -> (![b], !.st)
strictTRMapStAcc f []     acc st = (acc, st)
strictTRMapStAcc f [x:xs] acc st
  #! (r, st) = f x st
  = strictTRMapStAcc f xs [r : acc] st

strictTRZipWith :: !(a b -> c) ![a] ![b] -> [c]
strictTRZipWith f as bs = reverseTR (strictTRZipWithRev f as bs)

strictTRZipWithRev :: !(a b -> c) ![a] ![b] -> [c]
strictTRZipWithRev f as bs = strictTRZipWithAcc f as bs []

strictTRZipWithAcc :: !(a b -> c) ![a] ![b] ![c] -> [c]
strictTRZipWithAcc f [a:as] [b:bs] acc
  = strictTRZipWithAcc f as bs [f a b : acc]
strictTRZipWithAcc _ _ _ acc = acc

strictTRZip4 :: ![a] ![b] ![c] ![d] -> [(a, b, c, d)]
strictTRZip4 as bs cs ds = reverseTR (strictTRZip4Rev as bs cs ds)

strictTRZip4Rev :: ![a] ![b] ![c] ![d] -> [(a, b, c, d)]
strictTRZip4Rev as bs cs ds = strictTRZip4Acc as bs cs ds []

strictTRZip4Acc :: ![a] ![b] ![c] ![d] ![(a, b, c, d)] -> [(a, b, c, d)]
strictTRZip4Acc [a:as] [b:bs] [c:cs] [d:ds] acc
  = strictTRZip4Acc as bs cs ds [(a, b, c, d):acc]
strictTRZip4Acc _ _ _ _ acc = acc

strictTRZip2 :: ![a] ![b]-> [(a, b)]
strictTRZip2 as bs = reverseTR (strictTRZip2Rev as bs)

strictTRZip2Rev :: ![a] ![b]-> [(a, b)]
strictTRZip2Rev as bs = strictTRZip2Acc as bs []

strictTRZip2Acc :: ![a] ![b] ![(a, b)] -> [(a, b)]
strictTRZip2Acc [a:as] [b:bs] acc
  = strictTRZip2Acc as bs [(a, b):acc]
strictTRZip2Acc _ _ acc = acc

strictTRZipWith3 :: !(a b c -> d) ![a] ![b] ![c] -> [d]
strictTRZipWith3 f as bs cs = reverseTR (strictTRZipWith3Rev f as bs cs)

strictTRZipWith3Rev :: !(a b c -> d) ![a] ![b] ![c] -> [d]
strictTRZipWith3Rev f as bs cs = strictTRZipWith3Acc f as bs cs []

strictTRZipWith3Acc :: !(a b c -> d) ![a] ![b] ![c] ![d] -> [d]
strictTRZipWith3Acc f [a:as] [b:bs] [c:cs] acc
  = strictTRZipWith3Acc f as bs cs [f a b c : acc]
strictTRZipWith3Acc _ _ _ _ acc = acc

qfoldl :: (a -> b -> [b]) (a -> b -> a) a ![b] -> a
qfoldl _ _ a []
	= a
qfoldl f g a [b:bs]
	= let a` = g a b in qfoldl f g a` (bs ++ f a` b)

qfoldr :: (a -> b -> [b]) (b -> a -> a) a ![b] -> a
qfoldr _ _ a []
	= a
qfoldr f g a [b:bs]
	= let a` = g b a in qfoldr f g a` (bs ++ f a` b)

:: St a  = {nextindex :: !Int, stack :: ![a], map :: ![(a, Annot)], sccs :: ![[a]]}
:: Annot = {index :: !Int, lowlink :: !Int, onstack :: !Bool}

scc :: ![(a, [a])] -> [[a]] | Eq a
scc nodes = reverse (foldr (strongconnect nodes) {nextindex=zero,stack=[],map=[],sccs=[]} nodes).sccs
where
	strongconnect :: ![(a, [a])] !(a, [a]) !(St a) -> St a | Eq a
	strongconnect nodes (v, suc) s
		| isMember v (map fst s.map) = s
		# s = foldr (processSucc nodes v)
			{ s & map   = [(v, {index=s.nextindex, lowlink=s.nextindex, onstack=True}):s.map]
			, stack     = [v:s.stack]
			, nextindex = inc s.nextindex
			} suc
		# a = fromJust (lookup v s.map)
		| a.index <> a.lowlink = s
		# (scc, [sl:stack]) = span ((<>) v) s.stack
		# scc = scc ++ [sl]
		= { s & sccs = [scc:s.sccs]
		  , stack    = stack
		  , map      = [(el, if (isMember el scc) {s & onstack=False} s)\\(el, s)<-s.map]
		  }
	where
		processSucc :: ![(a, [a])] !a !a !(St a) -> St a | Eq a
		processSucc nodes v w s = case lookup w s.map of
			?None
				# n = filter ((==)w o fst) nodes
				| n =: [] = s
				# s = strongconnect nodes (hd n) s
				# aw = fromJust (lookup w s.map)
				# av = fromJust (lookup v s.map)
				= {s & map=[(v, {av & lowlink=min av.lowlink aw.lowlink}):s.map]}
			?Just aw=:{onstack=True}
				# av = fromJust (lookup v s.map)
				= {s & map=[(v, {av & lowlink=min aw.index av.lowlink}):s.map]}
			?Just _ = s
