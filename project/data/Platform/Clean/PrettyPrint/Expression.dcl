definition module Clean.PrettyPrint.Expression

from syntax import :: ParsedExpr, :: Rhs, :: OptGuardedAlts, :: FunKind

from Clean.PrettyPrint.Util import class print

instance print ParsedExpr, Rhs, FunKind

/**
 * `True` iff the right-hand side is a {{`GuardedAlts`}} or {{`UnguardedExpr`}}
 * with at least one {{`ewl_node`}}.
 */
compound_rhs :: !OptGuardedAlts -> Bool
