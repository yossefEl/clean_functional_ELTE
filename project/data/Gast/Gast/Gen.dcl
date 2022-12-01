definition module Gast.Gen

/*
	GAST: A Generic Automatic Software Test-system

	gen: generic generation of values of a type

	Pieter Koopman, 2004, 2017
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

import StdGeneric
from StdOverloadedList import
	class List, instance List [!] e
import qualified StdOverloadedList
import Control.GenBimap
from Data.List import
	instance Functor [!], instance Functor []
from Data.Func import
	$
from Data.Functor import
	<$>, class Functor (fmap)
from StdEnv import
	class + (+), ++, >=, class < (<), class Ord, instance + Int, instance < Int
from StdEnv import qualified
	uncurry, abort, isEmpty, drop, or, sortBy, hd, filter, not
from Data.Map import qualified
	get
from Data.Set import :: Set
from Data.Map import :: Map

:: RandomStream :== [Int]
aStream :: RandomStream
splitRandomStream :: !RandomStream -> (RandomStream,RandomStream)
randomize :: ![a] [Int] Int ([Int] -> [a]) -> [a]

generic ggen a :: !GenState -> [!a]
ggen{|UNIT|} s = [!UNIT]
ggen{|PAIR|} f g s
	= case s.pairTree of
		PTNode ptl sl sr ptr = case s.mode of
			SkewGeneration p = diagSkew (if sl p.skewr p.skewl)  (if sr p.skewr p.skewl)
                                        (f {s & pairTree = ptl}) (g {s & pairTree = ptr})
                                        PAIR
			BentGeneration   = 'StdEnv'.uncurry PAIR <$> diagBent (f {s & pairTree = ptl}) (g {s & pairTree = ptr})
		_ = 'StdEnv'.abort "ggen{|PAIR|}: invalid pairTree: PTNode"

ggen{|EITHER|} f g s
		# path	= s.path
		| 'StdEnv'.isEmpty path
			= interleave ('StdOverloadedList'.Map LEFT (f s)) ('StdOverloadedList'.Map RIGHT (g s))
			# s	= { s & path = 'StdEnv'.drop 1 path }
			  gs = 'StdOverloadedList'.Map RIGHT (g s)
			  fs = 'StdOverloadedList'.Map LEFT (f s)
			= case path of
				[ConsRight:_]	= interleave gs fs
				_				= interleave fs gs

ggen{|CONS of gcd|} f s
	= 'StdOverloadedList'.Map CONS (f {s & pairTree = pairTree})
where
	typeName	= gcd.gcd_type_def.gtd_name
	type		= gcd.gcd_type
	recCount	= recArgs typeName s.recInfo type
	pairTree	= genPairTree recCount gcd.gcd_arity
ggen{|OBJECT of gtd|} f s
	| s.depth >= s.maxDepth
		= [!]
		= [!OBJECT o \\ o <|- (f {s & depth = s.depth + 1, path = path, recInfo = ri2})]
where
	path = 'StdEnv'.hd
		(	[	getConsPath gcd
			\\	gcd <- 'StdEnv'.sortBy argCount
				( 'StdEnv'.filter
					(\gcd -> 'StdEnv'.not ('StdEnv'.or (recArgs gtd.gtd_name s.recInfo gcd.gcd_type))) gtd.gtd_conses
				)
			] ++ [[]])
	argCount gcd1 gcd2 = gcd1.gcd_arity < gcd2.gcd_arity
	ri2 = addRecInfo gtd.gtd_name (directNames gtd) s.recInfo

ggen{|RECORD of grd|} f s
	= 'StdOverloadedList'.Map RECORD (f {s & pairTree = pairTree})
where
	typeName	= grd.grd_name
	type		= grd.grd_type
	recCount	= recArgs typeName s.recInfo type
	pairTree	= genPairTree recCount grd.grd_arity

ggen{|FIELD of d|}  f s = [!FIELD fi \\ fi <|- vals]
where
	vals = case 'Data.Map'.get (d.gfd_cons.grd_name, d.gfd_name) s.recFieldValueNrLimits of
		?None       -> f s
		?Just limit -> 'StdOverloadedList'.Take limit $ f s

:: GenState =
	{ depth                 :: !Int //* depth
	, maxDepth              :: !Int
	, maxStringLength       :: !Int
	, path                  :: ![ConsPos] //* path to nonrecursive constructor
	, mode                  :: !Mode
	, recInfo               :: !Map TypeName (Set TypeName)
	, pairTree              :: !PairTree
	, recFieldValueNrLimits :: !Map (TypeName, RecFieldName) Int //* Restricts the number of values generated for record fields
	}

:: Mode = SkewGeneration !SkewParameters | BentGeneration

:: SkewParameters = { skewl :: !Int
                    , skewr :: !Int
                    }

:: TypeName	    :== String
:: RecFieldName :== String
:: PairTree       = PTLeaf | PTNode PairTree Bool Bool PairTree

genState :: GenState

/**
 * Generates an infinite list of random strings
 *
 * @param The maximum length of the strings
 * @param The factor for which the probability decreases for longer strings
 * @param Minimum character value
 * @param Maximum character value
 * @param A list of random numbers (e.g. aStream)
 * @result An inifinite list of strings
 */
ggenString :: !Int !Real !Int !Int !RandomStream -> [!String]

derive ggen Int, Bool, Real, Char, String
derive ggen (), (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
derive ggen [], [!], [ !], [!!]
derive ggen {}, {!}

defaultMaxStrLen :== 16384

// The following internal functions are exported for use in the exported definition of `ggen`.
genPairTree :: [Bool] Int -> PairTree
directNames :: GenericTypeDefDescriptor -> Set TypeName
diagBent :: ![!a] ![!b] -> [!(a, b)]
interleave :: ![!a] [!a] -> [!a]
diagSkew :: !Int !Int [!a] [!b] (a b-> c) -> [!c]
recArgs :: TypeName (Map TypeName (Set TypeName)) GenType -> [Bool]
addRecInfo :: TypeName (Set TypeName) (Map TypeName (Set TypeName)) -> Map TypeName (Set TypeName)
