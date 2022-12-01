definition module Clean.Types.Tree

/**
 * This module provides a type for trees of types. If a type `t1` is a child of
 * `t2`, `t2` is more general than `t1` (e.g. `a -> b` is more general than
 * `a -> Int`). This structure allows for efficient type search in Cloogle.
 *
 * Types in the tree are stored in a normalized form, which is created by
 * `prepare_unification`. The types in the tree are the 'right' types, i.e.
 * created with `prepare_unification False ..`. This means that to search in
 * the tree you need a type prepared as the 'left' type, i.e. with
 * `prepare_unification True ..`.
 *
 * @property-bootstrap
 *   import StdEnv
 *   import StdOverloadedList
 *
 *   import Clean.Types
 *   import Clean.Types.Unify
 *   import Clean.Types.Util
 *   import Data.Functor
 *   import Data.List
 *   import qualified Data.Map
 *   import Data.Maybe
 *
 *   prepare_unification` left :== snd o prepare_unification left (const False) 'Data.Map'.newMap
 *
 *   // NB: the generation of types is duplicated in the tests for
 *   // Clean.Types.Unify. If you make modifications here, consider making them
 *   // there as well.
 *
 *   // A copy of `Type`, to easily derive a ggen instance. There are no
 *   // contexts, no strictness, and no arrows, because these are ignored by
 *   // the unification algorithm anyway.
 *   :: GType
 *       = GType !GTypeName ![GType]
 *       | GFunc ![GType] !GType
 *       | GVar !GTypeVar
 *       | GCons !GTypeVar ![GType]
 *       | GUniq !GType
 *       | GForall !(!Bool, !GTypeVar) ![(Bool, GTypeVar)] !GType
 *
 *   :: GTypeName = GTypeA | GTypeB | GTypeC
 *   :: GTypeVar :== GTypeName
 *
 *   derive class Gast \ genShow GType, GTypeName, ?
 *
 *   genShow{|GType|} _ _ t rest = ["(",toString (toType t),")":rest]
 *
 *   toTypeTree :: ![(Type, v)] -> TypeTree v
 *   toTypeTree pairs = foldl add zero pairs
 *   where
 *       add :: !(TypeTree v) !(!Type, !v) -> TypeTree v
 *       add tree (type, v) = addType typel typer v tree
 *       where
 *           typel = prepare_unification` True type
 *           typer = prepare_unification` False type
 *
 *   toType :: !GType -> Type
 *   toType (GType name types) = Type (toTypeName name) (map toType types)
 *   toType (GFunc args result) = Func (map toType args) (toType result) (TypeContext [])
 *   toType (GVar var) = Var (toTypeVar var)
 *   toType (GCons cons types) = Type (toTypeVar cons) (map toType types)
 *   toType (GUniq type) = Uniq (toType type)
 *   toType (GForall first_var vars type) = Forall
 *       [if u Uniq id (Var (toTypeVar v)) \\ (u,v) <- [first_var:vars]]
 *       (toType type)
 *       (TypeContext [])
 *
 *   toTypeName GTypeA = "A"
 *   toTypeName GTypeB = "B"
 *   toTypeName GTypeC = "C"
 *
 *   toTypeVar GTypeA = "a"
 *   toTypeVar GTypeB = "b"
 *   toTypeVar GTypeC = "c"
 *
 * @property-test-generator [(GType, Int)] -> TypeTree Int
 *   gen pairs = toTypeTree [(toType t, v) \\ (t,v) <- pairs]
 *
 * @property-test-generator GType -> Type
 *   gen t = toType t
 *
 * @property-test-generator [GType] -> [(Type, Int)]
 *   gen ts = [(toType t, i) \\ t <- ts & i <- [1..]]
 */

from StdOverloaded import class zero

from Clean.Types import :: Type, :: Unifier
from Data.Graphviz import :: Digraph
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

:: TypeTree v

instance zero (TypeTree v)
derive JSONEncode TypeTree
derive JSONDecode TypeTree

/**
 * @property zero has one node:
 *   typeTreeNodes zero =.= 1
 */
typeTreeNodes :: !(TypeTree v) -> Int

/**
 * @property zero has no values:
 *   typeTreeSize zero =.= 0
 */
typeTreeSize :: !(TypeTree v) -> Int

/**
 * @property depth is bounded by nodes: A.t :: TypeTree Int:
 *   typeTreeDepth t <= typeTreeNodes t
 */
typeTreeDepth :: !(TypeTree v) -> Int

/**
 * Adds a type to a type tree. The type is assumed to have been prepared for
 * unification with `prepare_unification False .. .. type`.
 *
 * @param The type, prepared for unification as the 'left' type (i.e., with
 *   `prepare_unification True ..`).
 * @param The type, prepared for unification as the 'right' type (i.e., with
 *   `prepare_unification False ..`).
 * @param The value to connect to the type.
 * @param The original type tree.
 * @result The new type tree.
 */
addType :: !Type !Type !v !(TypeTree v) -> TypeTree v

/**
 * Find all types in the type tree that unify with some type.
 * The argument type is assumed to have been prepared for unification with
 * `prepare_unification True .. .. type`.
 * For each result, the type, unifier, and list of related values is returned.
 *
 * @property finds exactly everything: A.t` :: Type; values :: [(Type, Int)]:
 *     sort (concatMap thd3 (findUnifying t tree)) // the values found in the tree
 *   =.=
 *     sort (map snd (filter (matches o fst) values)) // the values found in the list
 * where
 *   t = prepare_unification` True t`
 *   tree = toTypeTree values
 *   matches = isJust o unify t o prepare_unification` False
 */
findUnifying :: !Type !(TypeTree v) -> [(Type,Unifier,[v])]

typeTreeToGraphviz :: !(TypeTree v) -> Digraph
