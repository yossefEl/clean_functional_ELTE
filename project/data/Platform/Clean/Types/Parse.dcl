definition module Clean.Types.Parse

/**
 * A parser for Clean types.
 *
 * @property-bootstrap
 *   import StdEnv
 *   import Clean.Types
 *   import Data.Maybe
 *
 *   derive genShow ?, Type, TypeContext, TypeRestriction
 *   derive gPrint ?, Type, TypeContext, TypeRestriction
 *
 *   expect :: !String !(?Type) -> Property
 *   expect s t = name s (parseType [c \\ c <-: s] =.= t)
 */

from Clean.Types import :: Type

/**
 * Parse a Clean type.
 *
 * For special builtin types (arrays, lists, and tuples), a non-infix notation
 * is accepted as well (e.g. `[] a`). Also cases where the kind is incorrect
 * are accepted (e.g. `[]`, `[] a b`). It is up to the application to handle
 * these.
 *
 * @property arrow constructors:
 *   expect "(->)"
 *     (?Just (Arrow ?None)) /\
 *   expect "((->) a)"
 *     (?Just (Arrow (?Just (Var "a")))) /\
 *   expect "(->) a b"
 *     (?Just (Func [Var "a"] (Var "b") (TypeContext [])))
 * @property builtin types:
 *   expect "[ !]"
 *     (?Just (Type "_List!" [])) /\
 *   expect "[!] a"
 *     (?Just (Type "_!List" [Var "a"])) /\
 *   expect "{32#} Int Real"
 *     (?Just (Type "_32#Array" [Type "Int" [], Type "Real" []]))
 * @property builtin maybe types:
 *   expect "?Int"
 *     (?Just (Type "_!Maybe" [Type "Int" []])) /\
 *   expect "?^Int"
 *     (?Just (Type "_Maybe"  [Type "Int" []])) /\
 *   expect "?#Int"
 *     (?Just (Type "_#Maybe" [Type "Int" []])) /\
 *   expect "??Int"
 *     (?Just (Type "_!Maybe" [Type "_!Maybe" [], Type "Int" []])) /\
 *   expect "?(?Int)"
 *     (?Just (Type "_!Maybe" [Type "_!Maybe" [Type "Int" []]])) /\
 *   expect "(?Int) -> ?Int"
 *     (?Just (Func [Type "_!Maybe" [Type "Int" []]] (Type "_!Maybe" [Type "Int" []]) (TypeContext [])))
 */
parseType :: ![Char] -> ?Type
