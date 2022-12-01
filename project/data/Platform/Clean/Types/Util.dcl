definition module Clean.Types.Util

/**
 * Utility functions for Clean types.
 */

from StdFunc import flip
from StdOverloaded import class toString (toString)

import Clean.Types
from Control.Monad import class pure, class <*>, class Applicative, class Monad, foldM
from Data.Functor import class Functor
from Data.Map import :: Map

/**
 * Pretty printer.
 *
 * @var The type to print
 * @param True iff parentheses should be placed around compound elements
 * @param The element to print
 * @result A list of strings that should be concatenated, the final string should not be or end with a newline
 */
class print a :: !Bool !a -> [String]

instance print String
instance print Int

instance print [a] | print a
instance print (?a) | print a

instance print TypeRestriction
instance print TypeContext
instance print Type
instance print Kind
instance print TypeDef
instance print Priority

instance toString Type

/**
 * Propagate uniqueness up, as described in section 9.2 of the Clean language
 * report.
 * @param A predicate function indicating a type by that name is always unique
 *   (like, e.g., World)
 * @param The type to modify
 */
propagate_uniqueness :: (String -> Bool) !Type -> Type

/**
 * Remove all `Uniq` attributes that can be inferred by `propagate_uniqueness`.
 * However, uniqueness attributes based on the first parameter are still added
 * (e.g., `Type "World" []` is transformed into `Uniq ..`.
 *
 * This function is useful for unification. When a type variable is unified
 * with a unique type, uniqueness propagation would break the recursive nature
 * of unification (the unifier would stop in a higher node because the not-yet-
 * propagated type cannot be unified with the already-propagated type). For
 * this reason, unification should be performed on types in which all inferable
 * uniqueness attributes have been removed.
 *
 * @param A predicate function indicating a type by that name is always unique
 *   (like, e.g., World)
 * @param The type to modify
 */
unpropagate_uniqueness :: (String -> Bool) !Type -> Type

/**
 * Resolve all synonyms in a type.
 *
 * @param The type synonyms to use
 * @param The type to resolve
 * @param The used synonyms and the new type
 */
resolve_synonyms :: (Map String [TypeDef]) !Type -> ([TypeDef], Type)

/**
 * Apply a variable assignment on a type, if possible.
 */
assign :: !TVAssignment !Type -> ?Type

/**
 * Apply a list of variable assignments on a type.
 *
 * @type [TVAssignment] Type -> ?Type
 */
assignAll :== flip (foldM (flip assign))

/**
 * Make all functions arity 1 by transforming `a b -> c` to `a -> b -> c`.
 */
reduceArities :: !Type -> Type

/**
 * Normalise a type, that is, rewrite it to an equivalent type that can be
 * compared to other types for equality using {{`==`}}. The transformations
 * applied:
 *
 * - Resolve always-unique types (like {{`World`}}; cf. {{`propagate_uniqueness`}}).
 * - Propagate uniqueness (cf. {{`propagate_uniqueness`}}).
 * - Resolve synonyms (cf. {{`resolve_synonyms`}}.
 * - Rewrite {{`Cons`}}es without arguments to {{`Var`}}s.
 * - Rewrite functions to arity 1 (cf. {{`reduceArities`}}).
 * - Rewrite variables to v1, v2, v3, ... s.t. a left first depth first
 *   iteration over the node does not introduce higher variables before lower
 *   ones (i.e., you will encounter v2 before v3).
 *
 * @param A predicate function indicating a type by that name is always unique
 *   (like, e.g., World)
 * @param The type synonyms to use
 * @param The type to normalise
 * @result The normalised types
 * @result The used type synonyms
 * @result The renamed type variables, in order
 */
normalise_type :: (String -> Bool) !(Map String [TypeDef]) !Type -> (!Type, ![TypeDef], ![TypeVar])
