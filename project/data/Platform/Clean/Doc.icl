implementation module Clean.Doc

import _SystemArray
import StdBool
import StdChar
import StdDebug
from StdFunc import flip, o, twice
import StdList
import StdMisc
import StdOrdList
import StdString
import StdTuple

import Control.Applicative
from Control.Monad import mapM, class Monad(..), >>=
import Data.Either
import Data.Error
from Data.Func import $
import Data.Functor
import Data.GenDefault
import Data.List
import Data.Maybe
import Data.Tuple
from Text import <+,
	class Text(join,split,trim,rtrim,replaceSubString,endsWith,startsWith),
	instance Text String, instance Text [Char],
	concat3, concat4
import Text.Language
import Text.Parsers.Simple.ParserCombinators

from Clean.Types import :: Type, :: TypeRestriction, :: TypeContext
from Clean.Types.Parse import parseType
from Clean.Types.Util import instance toString Type

gDefault{|(?)|} _ = ?None

fromMultiLine :: !MultiLineString -> String
fromMultiLine (MultiLine s) = s

instance docDescription       ModuleDoc where docDescription       d = d.ModuleDoc.description
instance docPropertyBootstrap ModuleDoc where docPropertyBootstrap d = d.property_bootstrap
instance docPropertyTestWith  ModuleDoc where docPropertyTestWith  d = d.ModuleDoc.property_test_with
instance docPropertyTestGenerators ModuleDoc where docPropertyTestGenerators d = d.property_test_generators

instance docDescription      FunctionDoc where docDescription      d = d.FunctionDoc.description
instance docComplexity       FunctionDoc where docComplexity       d = d.FunctionDoc.complexity
instance docParams           FunctionDoc where docParams           d = d.FunctionDoc.params
instance docVars             FunctionDoc where docVars             d = d.FunctionDoc.vars
instance docResults          FunctionDoc where docResults          d = d.FunctionDoc.results
instance docType             FunctionDoc where docType             d = d.FunctionDoc.type
instance docThrows           FunctionDoc where docThrows           d = d.FunctionDoc.throws
instance docProperties       FunctionDoc where docProperties       d = d.FunctionDoc.properties
instance docPropertyTestWith FunctionDoc where docPropertyTestWith d = d.FunctionDoc.property_test_with
instance docPreconditions    FunctionDoc where docPreconditions    d = d.FunctionDoc.preconditions

instance docDescription      InstanceDoc where docDescription      d = d.InstanceDoc.description
instance docComplexity       InstanceDoc where docComplexity       d = d.InstanceDoc.complexity
instance docProperties       InstanceDoc where docProperties       d = d.InstanceDoc.properties
instance docPropertyTestWith InstanceDoc where docPropertyTestWith d = d.InstanceDoc.property_test_with
instance docPreconditions    InstanceDoc where docPreconditions    d = d.InstanceDoc.preconditions

instance docDescription ParamDoc where docDescription d = d.ParamDoc.description

instance docDescription ClassMemberDoc where docDescription d = d.ClassMemberDoc.description
instance docComplexity  ClassMemberDoc where docComplexity  d = d.ClassMemberDoc.complexity
instance docParams      ClassMemberDoc where docParams      d = d.ClassMemberDoc.params
instance docResults     ClassMemberDoc where docResults     d = d.ClassMemberDoc.results
instance docType        ClassMemberDoc where docType        d = d.ClassMemberDoc.type
instance docThrows      ClassMemberDoc where docThrows      d = d.ClassMemberDoc.throws

instance docDescription ConstructorDoc where docDescription d = d.ConstructorDoc.description
instance docParams      ConstructorDoc where docParams      d = d.ConstructorDoc.params

instance docDescription ClassDoc where docDescription d = d.ClassDoc.description
instance docVars        ClassDoc where docVars        d = d.ClassDoc.vars
instance docMembers     ClassDoc where docMembers     d = d.ClassDoc.members

instance docDescription    TypeDoc where docDescription    d = d.TypeDoc.description
instance docVars           TypeDoc where docVars           d = d.TypeDoc.vars
instance docFields         TypeDoc where docFields         d = d.TypeDoc.fields
instance docConstructors   TypeDoc where docConstructors   d = d.TypeDoc.constructors
instance docRepresentation TypeDoc where docRepresentation d = d.TypeDoc.representation

instance toString ParamDoc
where
	toString pd=:{name= ?Just n,description= ?Just d} = n +++ ": " +++ d
	toString {ParamDoc | description= ?Just d} = d
	toString _ = ""

derive gDefault Type, TypeRestriction, ModuleDoc, FunctionDoc, InstanceDoc, TypeContext,
	ClassMemberDoc, ConstructorDoc, ClassDoc, TypeDoc, Property, PropertyBootstrapDoc,
	PropertyVarInstantiation, MultiLineString, PropertyTestGenerator, ParamDoc

constructorToFunctionDoc :: !ConstructorDoc -> FunctionDoc
constructorToFunctionDoc d =
	{ FunctionDoc
	| gDefault{|*|}
	& description = d.ConstructorDoc.description
	, params      = d.ConstructorDoc.params
	}

functionToClassMemberDoc :: !FunctionDoc -> ClassMemberDoc
functionToClassMemberDoc d =
	{ ClassMemberDoc
	| description = d.FunctionDoc.description
	, complexity  = d.FunctionDoc.complexity
	, params      = d.FunctionDoc.params
	, results     = d.FunctionDoc.results
	, type        = d.FunctionDoc.type
	, throws      = d.FunctionDoc.throws
	}

addClassMemberDoc :: !ClassDoc !(?ClassMemberDoc) -> ClassDoc
addClassMemberDoc d m = {d & members=d.members ++ [m]}

parseSingleLineDoc :: (String -> String)
parseSingleLineDoc = toString o trim o dropWhile ((==) '*') o fromString

parseDoc :: !String -> Either ParseError (!d, ![ParseWarning]) | docBlockToDoc{|*|} d
parseDoc s = docBlockToDoc{|*|} (Left [s])

generic docBlockToDoc d :: !(Either [String] DocBlock) -> Either ParseError (!d, ![ParseWarning])
docBlockToDoc{|String|} (Left []) = Left InternalNoDataError
docBlockToDoc{|String|} (Left ss) = Right (trim $ last ss, [])
docBlockToDoc{|String|} _         = abort "error in docBlockToDoc{|String|}\n"
docBlockToDoc{|[]|} fx (Left ss) = (\vws -> (map fst vws, flatten (map snd vws)) ) <$> mapM fx (map (Left o pure) ss)
docBlockToDoc{|[]|} _  _         = abort "error in docBlockToDoc{|[]|}\n"
docBlockToDoc{|(?)|} fx (Left [])    = Right (?None, [])
docBlockToDoc{|(?)|} fx ss=:(Left _) = appFst ?Just <$> fx ss
docBlockToDoc{|(?)|} _  _            = abort "error in docBlockToDoc{|(?)|}\n"
docBlockToDoc{|UNIT|} _ = Right (UNIT, [])
docBlockToDoc{|PAIR|} fx fy db=:(Right _) = liftA2 (\(x,ws) (y,ws`) -> (PAIR x y, ws ++ ws`)) (fx db) (fy db)
docBlockToDoc{|PAIR|} _  _  _             = abort "error in docBlockToDoc{|PAIR|}\n"
docBlockToDoc{|FIELD of {gfd_name}|} fx (Right db) = case fx (Left [v \\ (k,v) <- db | k matches gfd_name]) of
	Right (f, ws)            -> Right (FIELD f, ws)
	Left InternalNoDataError -> Left (MissingField gfd_name)
	Left e                   -> Left e
where
	(matches) infix 4 :: !String !String -> Bool
	(matches) k name =
		k` == name ||
		pluralise English k` == name ||
		k` == "return" && name == "result" ||
		k` == "return" && name == "results"
	where
		k` = {if (c == '-') '_' c \\ c <-: k}
docBlockToDoc{|FIELD of {gfd_name}|} _ _ = abort "error in docBlockToDoc{|FIELD|}\n"
docBlockToDoc{|RECORD|} fx (Left [s]) = case parseDocBlock s of
	Right (db, ws) -> case fx (Right db) of
		Right (v, ws`) -> Right (RECORD v, ws ++ ws`)
		Left e -> Left e
	Left e -> Left e
docBlockToDoc{|RECORD|} fx doc = appFst RECORD <$> fx doc
docBlockToDoc{|CONS|} fx doc = appFst CONS <$> fx doc
docBlockToDoc{|EITHER|} fl fr doc = case fl doc of
	Right (v, ws) -> Right (LEFT v, ws)
	Left e -> case fr doc of
		Right (v, ws) -> Right (RIGHT v, ws)
		Left _ -> Left e
docBlockToDoc{|OBJECT|} fx doc = appFst (\x -> OBJECT x) <$> fx doc

docBlockToDoc{|MultiLineString|} (Left [s]) = Right (MultiLine (trimMultiLine $ split "\n" s), [])
docBlockToDoc{|MultiLineString|} _          = abort "error in docBlockToDoc{|MultiLineString|}\n"

docBlockToDoc{|ParamDoc|} (Left [s]) = case findName (fromString s) of
	?Just (name,rest) -> Right (
		{ name        = ?Just $ toString name
		, description = case rest of
			[] -> ?None
			_  -> ?Just $ toString rest
		}, [])
	_                -> Right ({name= ?None, description= ?Just s}, [])
where
	findName cs
	# (name,cs) = span (\c -> isAlphanum c || c == '`') cs
	| not (isEmpty name) && not (isEmpty cs) && hd cs == ':'
		= ?Just (toString name, dropWhile isSpace (tl cs))
		= ?None
docBlockToDoc{|ParamDoc|} _ = abort "error in docBlockToDoc{|ParamDoc|}\n"

docBlockToDoc{|Type|} (Left []) = Left InternalNoDataError
docBlockToDoc{|Type|} (Left ss) = case [v \\ ?Just v <- map (parseType o fromString) ss] of
	[] -> Left (UnknownError "no parsable type")
	vs -> Right (last vs, [])
docBlockToDoc{|Type|} _ = abort "error in docBlockToDoc{|Type|}\n"

docBlockToDoc{|PropertyBootstrapDoc|} (Left [s]) = Right
	(
		{ bootstrap_content                 = MultiLine (trimMultiLine content)
		, bootstrap_without_default_imports = without_imports
		}
	, []
	)
where
	lines = split "\n" s
	without_imports = hd lines == "without default imports"
	content = if without_imports (tl lines) lines
docBlockToDoc{|PropertyBootstrapDoc|} _ = abort "error in docBlockToDoc{|PropertyBootstrapDoc|}\n"

docBlockToDoc{|Property|} (Left [s]) = let [signature:property] = split "\n" s in
		parseSignature signature >>= \(sig,ws1) ->
		parseProperty property >>= \(prop,ws2) ->
		Right (sig prop, ws1 ++ ws2)
where
	parseSignature :: !String -> Either ParseError (!String -> Property, ![ParseWarning])
	parseSignature s = case parse parser (fromString s) of
		Left es           -> Left (UnknownError "failed to parse property signature")
		Right (name,args) -> Right (ForAll name args, [])
	where
		parser :: Parser Char (!String, ![(String, Type)])
		parser = skipSpaces *>
			pMany (pSatisfy ((<>) ':')) >>= \name ->
			skipSpaces *> pToken ':' *>
			((skipSpaces *> pToken 'A' *> pToken '.' *>
			pMany
				(skipSpaces *>
				(liftA2 tuple
					(toString <$> pMany (pSatisfy (not o isSpace)))
					(pList [skipSpaces,pToken ':',pToken ':',skipSpaces] *> pTypeWithColonOrSemicolon)
				) <* skipSpaces)) <|> skipSpaces *> pure []) >>= \args ->
			pure (toString name, args)

		skipSpaces = pMany (pSatisfy isSpace) *> pYield undef
		pTypeWithColonOrSemicolon = (pMany (pSatisfy \c -> c <> ':' && c <> ';') <* pOneOf [':;'])
			>>= \t -> case parseType t of
				?None   -> pError "type could not be parsed"
				?Just t -> pure t

	parseProperty :: ![String] -> Either ParseError (!String, ![ParseWarning])
	parseProperty ss = Right (trimMultiLine ss, [])
docBlockToDoc{|Property|} _ = abort "error in docBlockToDoc{|Property|}\n"

docBlockToDoc{|PropertyVarInstantiation|} (Left [s]) = case split "=" s of
	[var:type:[]] -> case parseType (fromString type) of
		?Just t -> Right (PropertyVarInstantiation (trim var, t), [])
		?None   -> Left (UnknownError "type could not be parsed")
	_ -> Left (UnknownError "property var instantiation could not be parsed")
docBlockToDoc{|PropertyVarInstantiation|} _ = abort "error in docBlockToDoc{|PropertyVarInstantiation|}\n"

docBlockToDoc{|PropertyTestGenerator|} (Left [s])
| startsWith "list: " sig = case parseType [c \\ c <-: sig & i <- [0..] | i > 4] of
	?Just t -> Right (PTG_List t imp, [])
	?None   -> error
| otherwise = case parseType (fromString sig) of
	?Just t -> Right (PTG_Function t imp, [])
	?None   -> error
where
	sig = trim sig`
	imp = trimMultiLine imp`
	[sig`:imp`] = split "\n" s
	error = Left (UnknownError "test generator could not be parsed")
docBlockToDoc{|PropertyTestGenerator|} _ = abort "error in docBlockToDoc{|PropertyTestGenerator|}\n"

derive docBlockToDoc ModuleDoc, FunctionDoc, InstanceDoc, ClassMemberDoc,
	ConstructorDoc, ClassDoc, TypeDoc

printDoc :: !d -> String | docToDocBlock{|*|} d
printDoc d = case lines of
	[line] -> "//* " +++ line
	lines  -> concat3 "/**\n * " (join "\n * " lines) "\n */"
where
	fields` = case docToDocBlock{|*|} d of
		Right fs -> fs
		_        -> abort "error in printDoc\n"
	fields = filter ((<>) "description" o fst) fields`
	desc = lookup "description" fields`

	lines =
		maybe [] (split "\n") desc ++
		flatten
			[ [concat4 "@" f " " firstline:map ((+++) "  ") nextlines]
			\\ (f,v) <- fields
			, let [firstline:nextlines] = split "\n" v
			]

generic docToDocBlock a :: !a -> Either [String] DocBlock
docToDocBlock{|String|} s = Left [s]
docToDocBlock{|[]|} fx xs = Left [x \\ Left xs` <- map fx xs, x <- xs`]
docToDocBlock{|(?)|} fx mb = case mb of
	?None   -> Left []
	?Just x -> fx x

docToDocBlock{|PAIR|} fx fy (PAIR x y) = case fx x of
	Right xs -> case fy y of
		Right ys -> Right (xs ++ ys)
		_        -> abort "error in docToDocBlock{|PAIR|}\n"
	_            -> abort "error in docToDocBlock{|PAIR|}\n"
docToDocBlock{|FIELD of d|} fx (FIELD x) = case fx x of
	Left xs -> Right [(name,x) \\ x <- xs]
	_       -> abort "error in docToDocBlock{|FIELD|}\n"
where
	name = {if (c=='_') '-' c \\ c <-: name`}
	name`
	| endsWith "ies" d.gfd_name = d.gfd_name % (0,size d.gfd_name-4) +++ "y"
	| endsWith "s" d.gfd_name   = d.gfd_name % (0,size d.gfd_name-2)
	| otherwise                 = d.gfd_name
docToDocBlock{|RECORD|} fx (RECORD x) = fx x

docToDocBlock{|ParamDoc|} pd = case pd.ParamDoc.name of
	?None -> case pd.ParamDoc.description of
		?None   -> Left []
		?Just d -> Left [d]
	?Just n -> case pd.ParamDoc.description of
		?None   -> Left [n]
		?Just d -> Left [n +++ ": " +++ d]
docToDocBlock{|MultiLineString|} (MultiLine s) = Left [s]
docToDocBlock{|Type|} t = Left [toString t]
docToDocBlock{|PropertyBootstrapDoc|} bs_doc = Left $ [ if bs_doc.bootstrap_without_default_imports " without default imports" ""
	: map ((+++) "    ") $ split "\n" $ fromMultiLine bs_doc.bootstrap_content
	]
docToDocBlock{|Property|} (ForAll name args impl) = Left
	[name +++ ": A." +++ join "; " [a +++ " :: " <+ t \\ (a,t) <- args] +++ ":\n" +++ impl]
docToDocBlock{|PropertyVarInstantiation|} (PropertyVarInstantiation (a,t)) = Left [a +++ " = " <+ t]
docToDocBlock{|PropertyTestGenerator|} ptg = Left [t <+ "\n" +++ imp]
where
	(t,imp) = case ptg of
		PTG_Function t imp -> (t,imp)
		PTG_List t imp     -> (t,imp)

derive docToDocBlock ModuleDoc, FunctionDoc, InstanceDoc, ClassMemberDoc,
	ClassDoc, ConstructorDoc, TypeDoc

trimMultiLine :: ![String] -> String
trimMultiLine ss = join "\n" [s % (trimn, size s - 1) \\ s <- ss]
where
	trimn = minList [i \\ ?Just i <- map (firstNonSpace 0) ss]

	firstNonSpace :: !Int !String -> ?Int
	firstNonSpace i s
	| i >= size s   = ?None
	| isSpace s.[i] = firstNonSpace (i+1) s
	| otherwise     = ?Just i

parseDocBlock :: !String -> Either ParseError (!DocBlock, ![ParseWarning])
parseDocBlock b = prepareString b >>= parsef
where
	parsef :: ![[Char]] -> Either ParseError (!DocBlock, ![ParseWarning])
	parsef [] = Right ([], [])
	parsef lines = case span (\l -> isEmpty l || hd l <> '@') lines of
		([],   [ln]) = parseFields [ln]
		([],   rest) = appSnd (\ws -> [NoDescription:ws]) <$> parseFields rest
		(desc, rest) = appFst (\d -> [("description", linesToString desc):d]) <$> parseFields rest

	parseFields :: ![[Char]] -> Either ParseError (!DocBlock, ![ParseWarning])
	parseFields []
		= Right ([], [])
	parseFields [['@':line]:rest]
		= parseFields rest` >>=
			\(d,ws) -> appSnd ((++) ws) <$> parseFs field desc d
	where
		(field, descline) = span (not o isSpace) line
		(restdesc, rest`) = span (\l -> isEmpty l || hd l <> '@') rest
		desc = flatten $ intersperse ['\n'] $ if (isEmpty descline) restdesc [tl descline:restdesc]

		parseFs :: ![Char] ![Char] !DocBlock -> Either ParseError (!DocBlock, ![ParseWarning])
		parseFs field val d = Right ([(toString field,toString (rtrim val)):d], [])
	parseFields _
		= abort "error in parseDocBlock\n"

prepareString :: (String -> Either ParseError [[Char]])
prepareString = checkAsterisks o map trim o break '\n' o fromString
where
	checkAsterisks :: ![[Char]] -> Either ParseError [[Char]]
	checkAsterisks [['*':line]] = Right [safetl line]
	checkAsterisks [line] = Right [line]
	checkAsterisks lines
	| all (\l -> isEmpty l || hd l == '*') lines
		= Right $ map (safetl o dropWhile ((==) '*')) lines
		= Left $ MissingAsterisk $ toString $ hd $ filter (\l -> not (isEmpty l) && hd l <> '*') lines

	safetl :: ![a] -> [a]
	safetl []     = []
	safetl [_:xs] = xs

	break :: !a -> [a] -> [[a]] | == a
	break e = foldr f []
	where
		f x []     = if (x == e) []        [[x]]
		f x [y:ys] = if (x == e) [[]:y:ys] [[x:y]:ys]

linesToString :: ([[Char]] -> String)
linesToString = toString o flatten o intersperse ['\n']

instance toString ParseWarning
where
	toString (UnknownField f)   = "Doc warning: unknown field '" +++ f +++ "'"
	toString (IllegalField f)   = "Doc warning: illegal field '" +++ f +++ "'"
	toString NoDescription      = "Doc warning: missing description"
	toString UsedReturn         = "Doc warning: @return is deprecated, use @result"
	toString (UnparsableType t) = "Doc warning: could not parse type '" +++ t +++ "'"

instance toString ParseError
where
	toString (MissingAsterisk l) = "Doc error: missing leading asterisk in '" +++ l +++ "'"
	toString (MissingField f)    = "Doc error: required field '" +++ f +++ "' was missing"
	toString (UnknownError e)    = "Doc error: " +++ e
	toString InternalNoDataError = "Doc error: internal parsing error"

traceParseWarnings :: ![ParseWarning] !a -> a
traceParseWarnings []     x = x
traceParseWarnings [w:ws] x
| trace_tn w = traceParseWarnings ws x
| otherwise  = undef

traceParseError :: !ParseError !a -> a
traceParseError e x
| trace_tn e = x
| otherwise  = undef
