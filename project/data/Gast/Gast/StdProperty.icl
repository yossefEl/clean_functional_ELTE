implementation module Gast.StdProperty

/*
	GAST: A Generic Automatic Software Test-system
	
	stdProperty: opertors on logical properties

	Pieter Koopman, 2004..2008
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

import StdEnv, StdOverloadedList

import Data.Func
from Math.Random import genRandInt
import System.OS
import Testing.TestEvents
import Text
import Text.GenPrint

import Gast.Gen
import Gast.Testable
import Gast.ThunkNames

instance ==> Bool
where
	(==>) c p
		| c	= Prop ("Bool ==> " +++ testname p) (testlocation p) (evaluate p)
			= Prop ("Bool ==> " +++ testname p) (testlocation p) (\rs r = [!{r & res = Rej}])

instance ==> Property
where
	(==>) c=:(Prop n _ _) p = Prop (n +++ " ==> " +++ testname p) (testlocation p) imp
	where
		imp rs r
		# r1 = testAnalysis r (evaluate c rs r)
		= case  r1.res of
			OK	= evaluate p rs r1
				= [!{r & res = Rej}]

class (\/) infixr 2 a b	:: !a b -> Property	//	Conditional or  of arg1 and arg2
class (/\) infixr 3	a b :: !a b -> Property	//	Conditional and of arg1 and arg2

instance /\ Bool     Bool      where (/\) x y = prop x /\ prop y
instance /\ Property Bool      where (/\) x y = x /\ prop y
instance /\ Bool     Property  where (/\) x y = prop x /\ y
instance /\ Property Property
where (/\) x y = Prop ("(" +++ testname x +++ " /\\ " +++ testname y +++ ")") ?None (and x y)
	  where
		and x y genState r 
			# r1 = testAnalysis r (evaluate x genState r)
			  r2 = testAnalysis r (evaluate y genState r)
			= case (r1.res,r2.res) of // collect labels  !! 
				(CE _ ,_    )	= [!r1] // to fix the evaluation order
				(_    ,CE _ )	= [!r2]
				(Undef,_    )	= [!r2]
				(Rej  ,OK   )	= [!r2]
								= [!r1]

instance \/ Bool     Bool      where (\/) x y = prop x \/ prop y
instance \/ Property Bool      where (\/) x y = x \/ prop y
instance \/ Bool     Property  where (\/) x y = prop x \/ y
instance \/ Property Property
where
    (\/) x y = Prop ("(" +++ testname x +++ " \\/ " +++ testname y +++ ")") ?None (or x y)
    where
        or x y genState r = case testAnalysis r (evaluate x genState r) of
            r=:{res=OK}   -> [!r]
            r=:{res=Pass} -> case testAnalysis r (evaluate y genState r) of
									r2=:{res=OK} = [!r2]
												 = [!r]
            r             -> evaluate y genState r

(<==>) infix 1 :: !a !b -> Property	| Testable a & Testable b		//	True if properties are equivalent
(<==>) p q 
		# r  = newAdmin
		  b  = testAnalysis r (evaluate p genState r)
		  c  = testAnalysis r (evaluate q genState r)
		= prop (b.res == c.res) // can this be improved?

(===>) infix 1 :: Bool Bool -> Bool
(===>) p q = (not p) || q 

ExistsIn :: (x->p) [x] -> Property | Testable p & TestArg x
ExistsIn f l = Prop ("ExistsIn " +++ thunk_name_to_string f +++ " " +++ thunk_name_to_string l) ?None p
where p rs r = [!exists r [testAnalysis r (evaluate (f a) rs r)\\a <- l] MaxExists]

Exists :: (x->p) -> Property | Testable p & TestArg x
Exists f = Prop ("Exists " +++ thunk_name_to_string f) ?None p
where
    p genState r =
        [! exists
            r
            [ testAnalysis r (evaluate (f a) genState r)
            \\ a <|- generateAll {GenState| genState & recFieldValueNrLimits = r.Admin.recFieldValueNrLimits}
            ]
            MaxExists
        ]
exists r []				n = {r & res = CE []}
exists r _				0 = {r & res = Undef}
exists _ [r=:{res}:x]	n = case res of
								OK	= r
								Pass	= r
										= exists r x (n-1)

noCE r []                n = {r & res = OK}
noCE r _                 0 = {r & res = Pass}
noCE _ [r=:{res=CE _}:x] n = r
noCE _ [r=:{res=OK}:x]   n = noCE {r&res=Pass} x (n-1)
noCE r [_:x]             n = noCE r x (n-1)

testAnalysis :: !Admin ![!Admin] -> Admin // maakt van een lijst resultaten een enkel resultaat
testAnalysis r l = analysis l MaxExists Undef OK
where
	analysis [!]    n min max = {r & res = max}
	analysis _      0 min max = {r & res = min}
	analysis [!s:x] n min max = case s.res of
		CE _  = s
		OK    = analysis x (n-1) Pass max
		Pass  = analysis x (n-1) Pass Pass
		Undef = analysis x (n-1) min  max
		Rej   = case min of
			OK   = analysis x (n-1) OK   max
			Pass = analysis x (n-1) Pass max
			_    = analysis x (n-1) Rej  max
		_     = abort "Unknow result in testAnalysis"

ForAll :: !(x->p) -> Property | Testable p & TestArg x
ForAll f = Prop ("ForAll " +++ thunk_name_to_string f) ?None (evaluate f)

ForEach :: ![!x] !(x->p) -> Property | Testable p & TestArg x
ForEach list f =
	Prop ("ForEach " +++ thunk_name_to_string list +++ " " +++ thunk_name_to_string f) ?None (forAll f list)

(For) infixl 0 :: !(x->p) ![!x] -> Property | Testable p & TestArg x
(For) p list = ForEach list p

check :: !(a b -> Bool) !a !b -> Property | genShow{|*|}, gPrint{|*|} a & genShow{|*|}, gPrint{|*|} b
check op x y = Prop name ?None \gs a -> affirm op (Other relName) x y gs {a & namePath=[name:a.namePath]}
where
	name = thunk_name_to_string op
	relName = concat [name, "{", thunk_to_module_name_string op, "}"]

(=.=) infix 4 :: !a !a -> Property | Eq, genShow{|*|}, gPrint{|*|} a
(=.=) x y = Prop "=.=" ?None (affirm (==) Eq x y)

(<.>) infix 4 :: !a !a -> Property | Eq, genShow{|*|}, gPrint{|*|} a
(<.>) x y = Prop "<.>" ?None (affirm (<>) Ne x y)

(<.)  infix 4 :: !a !a -> Property | Ord, genShow{|*|}, gPrint{|*|} a
(<.) x y = Prop "<." ?None (affirm (<) Lt x y)

(<=.) infix 4 :: !a !a -> Property | Ord, genShow{|*|}, gPrint{|*|} a
(<=.) x y = Prop "<=." ?None (affirm (<=) Le x y)

(>.)  infix 4 :: !a !a -> Property | Ord, genShow{|*|}, gPrint{|*|} a
(>.) x y = Prop ">." ?None (affirm (>) Gt x y)

(>=.) infix 4 :: !a !a -> Property | Ord, genShow{|*|}, gPrint{|*|} a
(>=.) x y = Prop ">=." ?None (affirm (>=) Ge x y)

affirm :: !(a b->Bool) !Relation a b .GenState !.Admin -> [!Admin] | genShow{|*|}, gPrint{|*|} a & genShow{|*|}, gPrint{|*|} b
affirm op rel x y rs admin
    | op x y = evaluate True rs admin
    | otherwise = evaluate
        False
        rs
        { Admin | admin
        & failedAssertions = [ ( ExpectedRelation (GPrint (printToString x)) rel (GPrint (printToString y))
                               , concat $ genShow{|*|} "" False x []
                               , concat $ genShow{|*|} "" False y []
                               )
                             : admin.Admin.failedAssertions
                             ]
        }

(ForAndGen) infixl 0 :: !(x->p) ![!x] -> Property | Testable p & TestArg x
(ForAndGen) p list = Prop (thunk_name_to_string p +++ " ForAndGen " +++ thunk_name_to_string list) ?None (evaluate p)
where
    evaluate f rs result =
        forAll f
               (list++|generateAll {GenState| genState & recFieldValueNrLimits = result.Admin.recFieldValueNrLimits})
               rs
               result

classify :: !Bool l !p -> Property | Testable p & genShow{|*|} l
classify c l p
	| c	= Prop (testname p) ?None (\rs r = evaluate p rs {r & labels = [show1 l:r.labels]})
		= Prop (testname p) ?None (evaluate p)

label ::  !l !p -> Property | Testable p & genShow{|*|} l
label l p = Prop (testname p) ?None (\rs r = evaluate p rs {r & labels = [show1 l:r.labels]})

name :: !n !p -> Property | Testable p & toString n
name n p = Prop (toString n) ?None (\rs r -> evaluate p rs {r & namePath=[toString n:r.namePath]})

location_and_name :: !TestLocation !n !p -> Property | Testable p & toString n
location_and_name l n p = Prop (toString n) (?Just l) \rs r -> evaluate p rs {r & namePath=[toString n:r.namePath]}

possibleFailReasons :: ![String] !Property -> Property
possibleFailReasons reasons (Prop name loc f) = Prop name loc f`
where
	f` genState admin =
		[! case a.res of
			CE reasons` = {a & res = CE (reasons ++ reasons`)}
			_           = a
		\\ a <|- f genState admin
		]

limitNrOfRecFieldValues :: !(Map (TypeName, RecFieldName) Int) !p -> Property | Testable p
limitNrOfRecFieldValues limits p = Prop (testname p) ?None (\rs r = evaluate p rs {Admin| r & recFieldValueNrLimits = limits})

instance ~ Bool where ~ b = not b

instance ~ Result
where
	~ (CE _) = OK
	~ OK = CE []
	~ Pass = CE []
	~ Rej = Rej
	~ Undef = Undef

instance ~ Property
where ~ (Prop n m p) = Prop ("~" +++ n) m (\rs r = let r` = testAnalysis r (p rs r) in [!{r` & res = ~r`.res}])

approxEqual :: !a !a !a -> Property | abs, Ord, -, genShow{|*|}, gPrint{|*|} a
approxEqual err x y = Prop "approximately equals" ?None
                           (affirm (\x y -> abs (x - y) <= err)
                           (Other $ concat ["approximately equals (error = ", printToString err, ")"]) x y)
