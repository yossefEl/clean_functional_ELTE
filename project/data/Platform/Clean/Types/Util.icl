implementation module Clean.Types.Util

import StdArray
import StdBool
import StdChar
import StdFunctions
import StdInt
import StdMisc
import StdOrdList
import StdString
import StdTuple

import Clean.Types
import Control.Applicative
import Control.Monad
import Data.Func
import Data.Functor
import Data.List
from Data.Map import :: Map(..), get
import Data.Maybe
import Data.Tuple
from Text import class Text (concat), instance Text String

(--) infixr 1 :: !a !b -> [String] | print a & print b
(--) a b = print False a ++ print False b
(+-) infixr 1 :: !a !b -> [String] | print a & print b
(+-) a b = print True a ++ print False b
(-+) infixr 1 :: !a !b -> [String] | print a & print b
(-+) a b = print False a ++ print True b
(+-+) infixr 1 :: !a !b -> [String] | print a & print b
(+-+) a b = print True a ++ print True b

printersperse :: !Bool !a ![b] -> [String] | print a & print b
printersperse ia a bs = intercalate (print False a) (map (print ia) bs)

instance toInt Bool where toInt True = 1; toInt False = 0

instance print String where print _ s = [s]
instance print Int where print _ i = [toString i]
instance print Char where print _ c = [{c}]
instance print [a] | print a
where print _ xs = [concat e \\ e <- map (print False) xs]

instance print (?a) | print a
where print _ ?None = []; print b (?Just x) = print b x

instance print Kind
where
	print _ KStar = ["*"]
	print b (k1 KArrow k2) = parlft -- print True k1 ++ ["->"] ++ print b k2 -- parrgt
	where (parlft,parrgt) = if b ("(",")") ("","")

instance print TypeRestriction
where
	print _ (Instance c ts)  = "instance " -- c -- " " -- printersperse True " " ts
	print _ (Derivation g t) = "derive " -- g -- " " -+ t

instance print TypeContext
where
	print _ (TypeContext []) = []
	print _ (TypeContext crs) = printersperse False " & "
		[printersperse False ", " (map corg gr) -- " " -- printersperse False " " (types $ hd gr) \\ gr <- grps]
	where
		grps = groupBy (\a b -> types a == types b && length (types a) == 1) crs
		types (Instance _ ts) = ts; types (Derivation _ t) = [t]
		corg (Instance c _) = c; corg (Derivation g _) = g +++ "{|*|}"

instance print Type
where
	print ia (Type s vs) = typeConstructorName True ia s vs
	print _ (Var v) = [v]
	print ia (Func [] r (TypeContext [])) = print ia r
	print _ (Func [] r tc) = r -- " | " -- tc
	print ia (Func ts r (TypeContext [])) = parens ia (printersperse True " " ts -- " -> " -- r)
	print _ (Func ts r tc) = (Func ts r (TypeContext [])) -- " | " -- tc
	print ia (Cons tv [])  = print ia tv
	print ia (Cons tv ats) = parens ia (tv -- " " -- printersperse True " " ats)
	print _ (Uniq t)       = case t of
		Type _ _ -> "*" -- t
		_        -> "*" -+ t
	print _ (Forall tvs t (TypeContext [])) = "(A." -- printersperse True " " tvs -- ": " -- t -- ")"
	print _ (Forall tvs t tc) = "(A." -- printersperse True " " tvs -- ": " -- t -- " | " -- tc -- ")"
	print _ (Arrow ?None)     = ["(->)"]
	print _ (Arrow (?Just t)) = "((->) " -+ t +- ")"
	print ia (Strict t) = "!" -- print ia t

instance toString Type where toString t = concat $ print False t

parens :: !Bool ![String] -> [String]
parens False ss = ss
parens True ss  = ["(":ss] ++ [")"]

instance print TypeDef
where
	print _ {td_name,td_uniq,td_args,td_rhs}
		= ":: " -- if td_uniq "*" "" -- typeConstructorName False False td_name td_args -- td_rhs

instance print TypeDefRhs
where
	print _ (TDRCons ext cs)         = "\n\t= " -- printADT ext cs
	print _ (TDRMoreConses cs)       = "\n\t| " -- printADT False cs
	print _ (TDRNewType c)           = " =: " -- c
	print _ (TDRRecord _ exi fields) = " =" --
		if (isEmpty exi) [] (" E." -- printersperse False " " exi -- ":") --
		"\n\t" -- makeRecord exi fields
	where
		makeRecord :: ![TypeVar] ![RecordField] -> String
		makeRecord _ [] = "{}"
		makeRecord exi [f1:fs]
			= concat ("{ " -- printRf f1 -- "\n" --
				concat [concat ("\t, " -- printRf f -- "\n")
				        \\ f <- fs] -- "\t}")
		where
			padLen = maxList (map (\f -> size f.rf_name) [f1:fs])
			pad i s = s +++ toString (repeatn (i - size s) ' ')

			printRf {rf_name,rf_type} = pad padLen rf_name -- " :: " -- rf_type
	print _ (TDRSynonym t)            = " :== " -- t
	print _ (TDRAbstract ?None)       = []
	print _ (TDRAbstract (?Just rhs)) = " /*" -- rhs -- " */"
	print _ (TDRAbstractNewType c)    = " (=: " -- c -- ")"
	print _ (TDRAbstractSynonym t)    = " (:== " -- t -- ")"

printADT :: !Bool ![Constructor] -> String
printADT True cs = case cs of
	[] -> ".."
	cs -> concat (printADT False cs -- "\t| ..")
printADT False cs = case cs of
	[]      -> ""
	[c1:cs] -> concat (c1 -- concat [concat ("\n\t| " -- c) \\ c <- cs])

typeConstructorName :: !Bool !Bool !String ![Type] -> [String]
typeConstructorName isInfix isArg t as
# isInfix = isInfix && not (isEmpty as)
// Lists
| t == "_List"   = if isInfix ("["  -- as_spaces --  "]") ("[]"   -- sp_as)
| t == "_!List"  = if isInfix ("[!" -- as_spaces --  "]") ("[! ]" -- sp_as)
| t == "_List!"  = if isInfix ("["  -- as_spaces -- "!]") ("[ !]" -- sp_as)
| t == "_!List!" = if isInfix ("[!" -- as_spaces -- "!]") ("[!!]" -- sp_as)
| t == "_#List"  = if isInfix ("[#" -- as_spaces --  "]") ("[#]"  -- sp_as)
| t == "_#List!" = if isInfix ("[#" -- as_spaces -- "!]") ("[#!]" -- sp_as)
| t == "_|List"  = if isInfix ("[|" -- as_spaces --  "]") ("[|]"  -- sp_as)
// Arrays
| t == "_#Array"   = if isInfix ("{#"   -- as_spaces -- "}") ("{#}"   -- sp_as)
| t == "_32#Array" = if isInfix ("{32#" -- as_spaces -- "}") ("{32#}" -- sp_as)
| t == "_Array"    = if isInfix ("{"    -- as_spaces -- "}") ("{}"    -- sp_as)
| t == "_!Array"   = if isInfix ("{!"   -- as_spaces -- "}") ("{!}"   -- sp_as)
// Maybes
| t == "_Maybe"  = if (isEmpty as) ["(?^)"] (if isInfix (parens isArg) id ("?^ " -- as_spaces))
| t == "_#Maybe" = if (isEmpty as) ["(?#)"] (if isInfix (parens isArg) id ("?# " -- as_spaces))
| t == "_!Maybe" = if (isEmpty as) ["(?)"]  (if isInfix (parens isArg) id ("? "  -- as_spaces))
// Tuples
| t % (0,5) == "_Tuple"
	# n = toInt (t % (6, size t - 1))
	| isEmpty as                   = "(" -- repeatn (n-1) ',' -- ")"
	| n > length as || not isInfix = parens isArg ("(" -- repeatn (n-1) ',' -- ") " -- as_spaces)
	| otherwise                    = "(" -- printersperse False ", " as -- ")"
// Other predefined types
| t == "_Unit"   = ["()"]
| t.[0] == '_'   = [t % (1, size t - 1)]
// Other types
| isEmpty as     = print isArg t
| otherwise      = parens isArg (t -- " " -- as_spaces)
where
	sp_as = if (isEmpty as) [] (" " -- as_spaces) // the arguments with a leading space
	as_spaces = printersperse True " " as // the arguments interspersed with spaces

instance print Constructor
where
	print _ {cons_name,cons_args,cons_exi_vars=evars,cons_context=tc=:(TypeContext c),cons_priority}
		= if (isEmpty evars) [] ("E." -- printersperse False " " evars -- ": ") --
			name -- " " -- prio -- printersperse True " " cons_args --
			if (isEmpty c) [] (" & " -- tc)
	where
		(name,prio) = case cons_priority of
			?None   -> ([cons_name],             [])
			?Just p -> ("(" -- cons_name -- ")", p -- " ")

instance print Priority
where
	print _ (LeftAssoc i)  = "infixl " -- i
	print _ (RightAssoc i) = "infixr " -- i
	print _ (NoAssoc i)    = "infix " -- i

propagate_uniqueness :: (String -> Bool) !Type -> Type
propagate_uniqueness p (Type t ts)
	# ts = map (propagate_uniqueness p) ts
	= if (p t || any isUniq ts) Uniq id (Type t ts)
propagate_uniqueness p (Func is r tc)
	= Func (map (propagate_uniqueness p) is) (propagate_uniqueness p r) tc
propagate_uniqueness _ t=:(Var _)
	= t
propagate_uniqueness p (Cons v ts)
	# ts = map (propagate_uniqueness p) ts
	= if (any isUniq ts) Uniq id (Cons v ts)
propagate_uniqueness p (Uniq t)
	= Uniq (propagate_uniqueness p t)
propagate_uniqueness p (Forall vs t tc)
	= Forall vs (propagate_uniqueness p t) tc
propagate_uniqueness p (Arrow mbT)
	= Arrow (propagate_uniqueness p <$> mbT)
propagate_uniqueness p (Strict t)
	= Strict $ propagate_uniqueness p t

unpropagate_uniqueness :: (String -> Bool) !Type -> Type
unpropagate_uniqueness p (Type t ts)
	# ts = map (unpropagate_uniqueness p) ts
	= if (p t) (Uniq (Type t ts)) (Type t ts)
unpropagate_uniqueness p (Func is r tc)
	= Func (map (unpropagate_uniqueness p) is) (unpropagate_uniqueness p r) tc
unpropagate_uniqueness _ t=:(Var _)
	= t
unpropagate_uniqueness p (Cons v ts)
	= Cons v (map (unpropagate_uniqueness p) ts)
unpropagate_uniqueness p (Uniq t)
	| has_inferable_uniqueness t
		= unpropagate_uniqueness p t
		= Uniq (unpropagate_uniqueness p t)
unpropagate_uniqueness p (Forall vs t tc)
	= Forall vs (unpropagate_uniqueness p t) tc
unpropagate_uniqueness p (Arrow mbT)
	= Arrow (unpropagate_uniqueness p <$> mbT)
unpropagate_uniqueness p (Strict t)
	= Strict (unpropagate_uniqueness p t)

has_inferable_uniqueness :: !Type -> Bool
has_inferable_uniqueness (Type t ts)
	= any has_inferable_uniqueness ts
has_inferable_uniqueness (Func _ _ _)
	= False
has_inferable_uniqueness (Var _)
	= False
has_inferable_uniqueness (Cons _ ts)
	= any has_inferable_uniqueness ts
has_inferable_uniqueness (Uniq _)
	= True
has_inferable_uniqueness (Forall _ _ _)
	= False
has_inferable_uniqueness (Arrow _)
	= False
has_inferable_uniqueness (Strict t)
	= has_inferable_uniqueness t

resolve_synonyms :: (Map String [TypeDef]) !Type -> ([TypeDef], Type)
resolve_synonyms tds (Type t ts)
	# (syns, ts) = appFst (removeDupTypedefs o flatten) $ unzip $ map (resolve_synonyms tds) ts
	= case candidates of
		[] -> (syns, Type t ts)
		[syn=:{td_args, td_rhs=TDRSynonym synt}:_]
			# newargs = map ((+++) "__" o fromVar) td_args
			# t = case assignAll [(fromVar a, Var n) \\ a <- td_args & n <- newargs] synt
					>>= assignAll [(a,r) \\ a <- newargs & r <- ts] of
				?Just t -> t
				_       -> abort "error in resolve_synonyms_Type\n"
			| length td_args <> length ts -> case t of
				Type r rs
					# t = Type r $ rs ++ drop (length td_args) ts
					-> appFst ((++) [syn:syns]) $ resolve_synonyms tds t
				_   -> abort "error in resolve_synonyms_Type\n"
			-> appFst ((++) [syn:syns]) $ resolve_synonyms tds t
		_ -> abort "error in resolve_synonyms_Type\n"
where
	candidates = [td \\ td=:{td_rhs=TDRSynonym syn} <- fromMaybe [] $ get t tds
		| length td.td_args <= tslen && (isType syn || length td.td_args == tslen)]
	where tslen = length ts
resolve_synonyms tds (Func is r tc)
	= case appFst (removeDupTypedefs o flatten) $ unzip $ map (resolve_synonyms tds) [r:is] of
		(syns, [r:is]) -> (syns, Func is r tc)
		_              -> abort "error in resolve_synonyms_Func\n"
resolve_synonyms _ (Var v)
	= ([], Var v)
resolve_synonyms tds (Cons v ts)
	# (syns, ts) = appFst (removeDupTypedefs o flatten) $ unzip $ map (resolve_synonyms tds) ts
	= (syns, Cons v ts)
resolve_synonyms tds (Forall vs t tc)
	# (syns, t) = resolve_synonyms tds t
	= (syns, Forall vs t tc)
resolve_synonyms tds (Arrow (?Just t))
	= Arrow o ?Just <$> resolve_synonyms tds t
resolve_synonyms tds (Arrow ?None)
	= ([], Arrow ?None)
resolve_synonyms tds (Uniq t)
	= Uniq <$> resolve_synonyms tds t
resolve_synonyms tds (Strict t)
	= Strict <$> resolve_synonyms tds t

// Apply a TVAssignment to a Type
assign :: !TVAssignment !Type -> ?Type
assign va (Type s ts) = Type s <$> mapM (assign va) ts
assign va (Func ts r tc)
	= liftM3 Func (mapM (assign va) ts) (assign va r) (pure tc) // TODO tc
assign (v,a) (Var v`) = pure $ if (v == v`) a (Var v`)
assign va=:(v,Type s ts) (Cons v` ts`)
	| v == v`   = Type s <$> (++) ts <$> mapM (assign va) ts`
assign va=:(v,Cons c ts) (Cons v` ts`)
	| v == v`   = Cons c <$> (++) ts <$> mapM (assign va) ts`
assign va=:(v,Var v`) (Cons v`` ts)
	| v == v``  = Cons v` <$> mapM (assign va) ts
assign va=:(v,Arrow (?Just a)) (Cons v` ts)
	| v == v`   = (\r -> Func [a] r (TypeContext [])) <$> (makeFunc ts)
		with
			makeFunc [t]    = assign va t
			makeFunc [t:ts] = liftM2 (\a r -> Func [a] r (TypeContext [])) (assign va t) (makeFunc ts)
assign va=:(v,_) (Cons v` ts)
	| v == v`   = empty
	| otherwise = Cons v` <$> mapM (assign va) ts
assign va (Uniq t) = Uniq <$> assign va t
assign va=:(v,Var v`) (Forall tvs t tc)
	= liftM3 Forall (mapM (assign va) tvs) (assign va t) (pure tc) // TODO tc
assign va=:(v,_) (Forall tvs t tc)
	| isMember (Var v) tvs = empty
	| otherwise = flip (Forall tvs) tc <$> assign va t
assign va (Arrow (?Just t)) = Arrow o ?Just <$> assign va t
assign va (Arrow ?None) = ?Just $ Arrow ?None
assign va (Strict t) = Strict <$> assign va t

reduceArities :: !Type -> Type
reduceArities f=:(Func [] r (TypeContext tc))
	| isEmpty tc
		= reduceArities r
		= case reduceArities r of
			Func args r (TypeContext tc2)
				-> reduceArities (Func args r (TypeContext (removeDup (tc++tc2))))
			r
				-> Func [] r (TypeContext tc)
reduceArities (Func [arg1:args=:[_:_]] r tc) = Func [reduceArities arg1] (reduceArities (Func args r (TypeContext []))) tc
reduceArities (Func [arg] r tc) = Func [reduceArities arg] (reduceArities r) tc
reduceArities (Type s ts) = Type s $ map reduceArities ts
reduceArities (Cons v ts) = Cons v $ map reduceArities ts
reduceArities (Uniq t) = Uniq $ reduceArities t
reduceArities (Var v) = Var v
reduceArities (Forall [] t (TypeContext [])) = reduceArities t
reduceArities (Forall tvs t tc) = Forall tvs (reduceArities t) tc
reduceArities (Arrow mt) = Arrow (reduceArities <$> mt)
reduceArities (Strict t) = Strict $ reduceArities t

normalise_type :: (String -> Bool) !(Map String [TypeDef]) !Type -> (!Type, ![TypeDef], ![TypeVar])
normalise_type alwaysUnique tds t
# t        = reduceArities t
# (syns,t) = resolve_synonyms tds t
# t        = propagate_uniqueness alwaysUnique t
# t        = optConses t
# (t,vars) = rename t
= (t,syns,vars)
where
	rename :: !Type -> (!Type, ![TypeVar])
	rename t = (renameVars t, map fst renames)
	where
		renames :: [(TypeVar, TypeVar)]
		renames = [(o, "v" +++ toString n) \\ o <- removeDup $ allVars t & n <- [1..]]

		renameVars :: !Type -> Type
		renameVars (Type s ts)                    = Type s $ map renameVars ts
		renameVars (Func is r (TypeContext tc))   = Func (map renameVars is) (renameVars r) $ TypeContext (map renameVarsInTC tc)
		renameVars (Var tv)                       = Var $ fromJust $ lookup tv renames
		renameVars (Cons cv ts)                   = Cons (fromJust $ lookup cv renames) $ map renameVars ts
		renameVars (Uniq t)                       = Uniq $ renameVars t
		renameVars (Forall vs t (TypeContext tc)) = Forall (map renameVars vs) (renameVars t) $ TypeContext (map renameVarsInTC tc)
		renameVars (Arrow t)                      = Arrow $ renameVars <$> t
		renameVars (Strict t)                     = Strict $ renameVars t

		renameVarsInTC :: !TypeRestriction -> TypeRestriction
		renameVarsInTC (Instance c ts)  = Instance c $ map renameVars ts
		renameVarsInTC (Derivation g t) = Derivation g $ renameVars t

		allVars :: !Type -> [TypeVar]
		allVars (Type _ ts)       = allVars` ts
		allVars (Func is r _)     = allVars` is ++ allVars r
		allVars (Var tv)          = [tv]
		allVars (Cons cv ts)      = [cv:allVars` ts]
		allVars (Uniq t)          = allVars t
		allVars (Forall vs t _)   = allVars` vs ++ allVars t
		allVars (Arrow (?Just t)) = allVars t
		allVars (Arrow ?None)     = []
		allVars (Strict t)        = allVars t

		allVars` :: ([Type] -> [TypeVar])
		allVars` = concatMap allVars

	optConses :: !Type -> Type
	optConses (Type s ts)                    = Type s $ map optConses ts
	optConses (Func is r (TypeContext tc))   = Func (map optConses is) (optConses r) $ TypeContext (map optConsesInTR tc)
	optConses (Var v)                        = Var v
	optConses (Cons c [])                    = Var c
	optConses (Cons c ts)                    = Cons c $ map optConses ts
	optConses (Uniq t)                       = Uniq $ optConses t
	optConses (Forall vs t (TypeContext tc)) = Forall (map optConses vs) (optConses t) $ TypeContext (map optConsesInTR tc)
	optConses (Arrow t)                      = Arrow $ optConses <$> t
	optConses (Strict t)                     = Strict $ optConses t

	optConsesInTR :: !TypeRestriction -> TypeRestriction
	optConsesInTR (Instance c ts)  = Instance c $ map optConses ts
	optConsesInTR (Derivation g t) = Derivation g $ optConses t
