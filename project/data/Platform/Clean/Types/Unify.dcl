definition module Clean.Types.Unify

/**
 * Functions to unify Clean types.
 *
 * @property-bootstrap
 *   import StdEnv
 *   import Clean.Types.Parse
 *   import Clean.Types.Util
 *   import Data.Either
 *   import Data.Func
 *   import Data.Functor
 *   import Data.List
 *   import qualified Data.Map
 *   import Data.Maybe
 *   import Text
 *
 *   derive genShow ?, UnifyingAssignment, Type, TypeContext, TypeRestriction
 *   derive gPrint ?, UnifyingAssignment, Type, TypeContext, TypeRestriction
 *
 *   instance == UnifyingAssignment
 *   where
 *   	(==) (LeftToRight a) (LeftToRight b) = a == b
 *   	(==) (RightToLeft a) (RightToLeft b) = a == b
 *   	(==) _               _               = False
 *
 *   parse :: !String -> Type
 *   parse s = fromJust (parseType [c \\ c <-: s])
 *
 *   expect :: !String !String !(?[Either (String,String) (String,String)]) -> Property
 *   expect t1 t2 res = name
 *       (concat3 t1 " ≃ " t2)
 *       (unifier =.= (map toUnifyingAssignment <$> res))
 *   where
 *       (_,t1`) = prepare_unification True  (const False) 'Data.Map'.newMap (parse t1)
 *       (_,t2`) = prepare_unification False (const False) 'Data.Map'.newMap (parse t2)
 *       unifier = assignments <$> finish_unification [] <$> unify t1` t2`
 *
 *       assignments u = u.assignments
 *
 *       toUnifyingAssignment (Left (v,t))  = LeftToRight (v, parse t)
 *       toUnifyingAssignment (Right (v,t)) = RightToLeft (v, parse t)
 *
 *   // NB: the generation of types is duplicated in the tests for
 *   // Clean.Types.Tree. If you make modifications here, consider making them
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
 *   derive class Gast \ genShow GType, GTypeName
 *
 *   genShow{|GType|} _ _ t rest = ["(",toString (toType t),")":rest]
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
 * @property-test-generator GType -> Type
 *   gen t = toType t
 */

import Clean.Types
from Data.Map import :: Map

/**
 * Check whether a unification result indicates that the left type generalises
 * the right type.
 */
isGeneralisingUnifier :: ![TVAssignment] -> Bool

/**
 * Check whether a unification result indicates that the left type specialises
 * the right type.
 */
isSpecialisingUnifier :: ![TVAssignment] -> Bool

/**
 * Check whether a unification result indicates that the unified types are
 * isomorphic.
 */
isIsomorphicUnifier :: ![TVAssignment] -> Bool

/**
 * `True` iff the first type is more general or isomorphic to the second type.
 *
 * @property inverse of specialises: A.t :: Type; u :: Type:
 *   classify (t generalises u) "t generalises u" $
 *   classify (u generalises t) "u generalises t" $
 *     (t generalises u <==> u specialises t)
 *   /\
 *     (u generalises t <==> t specialises u)
 *
 * @property more specific type isomorphic after unification: A.t` :: Type; u` :: Type:
 *   isJust mbUnif ==>
 *   (classify (isGeneralisingUnifier unif) "generalisation" $
 *    classify (isSpecialisingUnifier unif) "specialisation" $
 *       (isGeneralisingUnifier unif ==>
 *       isJust newu /\ check (isomorphic_to) (fromJust newu) u)
 *   /\
 *       (isSpecialisingUnifier unif ==>
 *       isJust newt /\ check (isomorphic_to) (fromJust newt) t)
 *   )
 *   where
 *       (_,t) = prepare_unification True  (const False) 'Data.Map'.newMap t`
 *       (_,u) = prepare_unification False (const False) 'Data.Map'.newMap u`
 *       mbUnif = unify t u
 *       unif = fromJust mbUnif
 *       newu = assignAll unif u
 *       newt = assignAll unif t
 *
 * @property tricky case with Forall:
 *   parse "a" generalises parse "A.a: a" /\
 *   not (parse "a" specialises parse "A.a: a")
 */
(generalises) infix 4 :: !Type !Type -> Bool

/**
 * Variant of `generalises` for types on which `prepare_unification` has
 * already been performed.
 */
(generalises`) infix 4 :: !Type !Type -> Bool

/**
 * `True` iff the first type is more specific or isomorphic to the second type.
 */
(specialises) infix 4 :: !Type !Type -> Bool

/**
 * Variant of `specialises` for types on which `prepare_unification` has
 * already been performed.
 */
(specialises`) infix 4 :: !Type !Type -> Bool

/**
 * `True` if two types are isomorphic to each other.
 *
 * @property generalises and specialises: A.t :: Type; u :: Type:
 *   classify (t isomorphic_to u) "isomorphism" $
 *     t isomorphic_to u
 *   <==>
 *     t generalises u && t specialises u
 */
(isomorphic_to) infix 4 :: !Type !Type -> Bool

/**
 * Variant of `isomorphic_to` for types on which `prepare_unification` has
 * already been performed.
 */
(isomorphic_to`) infix 4 :: !Type !Type -> Bool

/**
 * Prepare a type for unification. Unification always happens between a 'left'
 * and a 'right' type. Unification of two left or two right types may yield
 * unexpected results.
 *
 * @param True if this is the left type
 * @param A predicate indicating if a type is always unique, like, e.g., World
 * @param Known type definitions to use for resolving synonyms
 * @param The type to prepare
 * @result The type synonyms used and the type after preparation
 */
prepare_unification :: !Bool (String -> Bool) (Map String [TypeDef]) !Type -> ([TypeDef], Type)

/**
 * Finish unification, yielding a unifier.
 *
 * @param The used type synonyms
 * @param The variable assignments
 * @result The unifier
 */
finish_unification :: ![TypeDef] ![TVAssignment] -> Unifier

/**
 * Core of the unification. An implementation of Martelli and Montanari, 'An
 * Efficient Unification Algorithm'. ACM Transactions on Programming Languages
 * and Systems, Vol. 4, No. 2, April 1982, pp. 258-282.
 * It has been modified slightly to deal with constructor variables, universal
 * quantifiers and uniqueness.
 *
 * @param The left type
 * @param The right type
 * @result A list of type variable assignments, or `?None` if unification failed
 *
 * @property arrows are type constructors:
 *   expect "m T" "((->) T)"
 *     (?Just [Left ("m", "(->)")]) /\
 *   expect "(f (t -> u)) (f t) -> f u" "(a b -> c) (a -> b) a -> c"
 *     (?Just [Left ("f", "(->) a"), Left ("u", "c"), Left ("t", "b")]) /\
 *   expect "(a b -> c) (a -> b) a -> c" "(f (t -> u)) (f t) -> f u"
 *     (?Just [Right ("f", "(->) a"), Right ("u", "c"), Left ("b", "t")])
 *
 * @property uniqueness propagation does not hinder unification:
 *   expect "(a, *b)" "(a, b)"
 *     (?Just [Right ("b", "*b"), Left ("a", "a")])
 *
 * @property universally quantified variables:
 *   expect "A.a b: a -> b" "cat a a"
 *     ?None
 */
unify :: !Type !Type -> ?[TVAssignment]
