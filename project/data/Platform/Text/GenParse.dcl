definition module Text.GenParse

import StdGeneric
from StdOverloaded import class toString, class ==

class ParseInput s where parseInput :: !s -> (!?Char, !s)

:: StringInput = { si_str :: !String, si_pos :: !Int}
mkStringInput :: !String -> StringInput

instance ParseInput	StringInput
instance ParseInput File

:: Expr
	= ExprInt Int
	| ExprChar Char
	| ExprReal Real
	| ExprBool Bool
	| ExprString String
	| ExprIdent String
	| ExprApp {Expr}
	| ExprTuple {Expr}
	| ExprField String Expr
	| ExprRecord (?String) {Expr}
	| ExprList [Expr]
	| ExprArray [Expr]
	| ExprUnitBuiltin
	| ExprError String
	// aux
	| ExprUnit
	| ExprAppInInfix {Expr} GenConsAssoc Int GenConsAssoc
	| ExprPair Expr Expr

instance toString Expr
instance == Expr

generic gParse a :: !Expr -> ?a

derive gParse Int, Char, Bool, Real, String, UNIT, PAIR, EITHER, CONS of d, RECORD of {grd_name}, FIELD of {gfd_name}, OBJECT of {gtd_num_conses,gtd_conses}, [], {!}, {}, ()

preParseString :: !String -> Expr
preParseFile :: !File -> Expr

parseString :: !String -> ?a | gParse{|*|} a
parseFile :: !File -> ?a | gParse{|*|} a
