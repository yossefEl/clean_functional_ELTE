// (c) 1999 by Martin Erwig
// | Threading Combinators.
definition module Data.Graph.Inductive.Internal.Thread

//--------------------------------------------------------------------
// CLASSES AND TYPES
//--------------------------------------------------------------------

/*
class Thread t a b where
  split :: a -> t -> (b,t)


instance Thread (Graph a b) Node (MContext a b) where
  split = match

instance (D.Discrete a) => Thread (D.Diet a) a a where
  split x s = (x,D.delete x s)
*/

/*
   Make clear different notions:

   "thread" = data structure + split operation
   ...      = threadable data structure
   ...      = split operation

*/


//--------------------------------------------------------------------
// THREAD COMBINATORS
//--------------------------------------------------------------------


// (A) split along a list of indexes and thread data structure
//
// there are different ways to consume the returned elements:

/*
//  (1) simple collect in a list
//
foldT1` ys []     d = ys
foldT1` ys (x:xs) d = foldT1` (y:ys) xs d`  where (y,d`) = split x d
foldT1 xs d = foldT1` [] xs d

//  (2) combine by a function
//
foldT2` f ys []     d = ys
foldT2` f ys (x:xs) d = foldT2` f (f y ys) xs d`  where (y,d`) = split x d
foldT2 f u xs d = foldT2` f u xs d
*/


// Mnemonics:
//
//  t : thread type
//  i : index type
//  r : result type
//  c : collection type
//
:: Split t i r  :== i -> t -> (r,t)
:: Thread t i r :== (t,Split t i r)
:: Collect r c  :== (r -> c -> c,c)

//  (3) abstract from split
//
threadList` :: (Collect r c) (Split t i r) [i] t -> (c,t)

/*
   Note: threadList` works top-down (or, from left),
         whereas dfs,gfold,... have been defined bottom-up (or from right).

   ==> therefore, we define a correpsonding operator for folding
       bottom-up/from right.
*/
threadList :: (Collect r c) (Split t i r) [i] t -> (c,t)


// (B) thread "maybes", ie, apply f to Just-values and continue
//     threading with "continuation" c, and ignore Nothing-values, ie,
//     stop threading and return current data structure.
//
// threadMaybe` :: (r -> b) -> Split t i r -> (e -> f -> (?i,t))
//                 -> e -> f -> (?b,t)

:: SplitM t i r :== Split t i (?r)

threadMaybe` :: (r -> a) (Split t i r) (Split t j (?i)) -> Split t j (?a)

// extension:  grant f access also to y, the result of split.
//
// threadMaybe :: (a -> b -> c) -> (a -> d -> (b,d)) -> (e -> f -> (?a,d))
//                -> e -> f -> (?c,d)
// threadMaybe :: (i->r->a)->Split t i r->Split t j (?i)->Split t j (?a)
threadMaybe :: (i r -> a) (Split t i r) (SplitM t j i) -> SplitM t j a


// (C) compose splits in parallel (is a kind of generalized zip)
//
// splitPar :: (a -> b -> (c,d)) -> (e -> f -> (g,h))
//             -> (a,e) -> (b,f) -> ((c,g),(d,h))
splitPar :: (Split t i r) (Split u j s) -> Split (t,u) (i,j) (r,s)

splitParM :: (SplitM t i r) (Split u j s) -> SplitM (t,u) (i,j) (r,s)


// (D) merge a thread with/into a computation
//
/*
   Example: assign consecutive numbers to the nodes of a tree

   Input: type d, thread (t,split), fold operation on d
*/
