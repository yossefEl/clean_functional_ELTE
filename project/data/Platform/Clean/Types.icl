implementation module Clean.Types

import StdBool
from StdClass import class Eq
from StdFunc import o, id
import StdList
import StdMisc
from StdOverloaded import class ==(..), class length(..)
from StdString import instance == {#Char}
import StdTuple

from Data.Func import $
import Data.Functor
import Data.List
import Data.Maybe

instance == Type
where
	(==) a b = case a of
		Type t args -> case b of
			Type t` args` -> t==t` && args==args`
			_             -> False
		Func is r tc -> case b of
			Func is` r` tc` -> is==is` && r==r` && tc==tc`
			_               -> False
		Var tv -> case b of
			Var tv` -> tv==tv`
			_       -> False
		Cons tv args -> case b of
			Cons tv` args` -> tv==tv` && args==args`
			_              -> False
		Uniq t -> case b of
			Uniq t` -> t==t`
			_       -> False
		Forall vs t tc -> case b of
			Forall vs` t` tc` -> vs==vs` && t==t` && tc==tc`
			_                 -> False
		Arrow mt -> case b of
			Arrow mt` -> mt==mt`
			_         -> False
		Strict t -> case b of
			Strict t` -> t==t`
			_         -> False

instance == TypeRestriction
where
	(==) a b = case a of
		Instance cls ts -> case b of
			Instance cls` ts` -> cls==cls` && ts==ts`
			_                 -> False
		Derivation gen t -> case b of
			Derivation gen` t` -> gen==gen` && t==t`
			_                  -> False

instance == Kind
where
	(==) a b = case a of
		KStar -> b=:KStar
		KArrow a b -> case b of
			KArrow a` b` -> a==a` && b==b`
			_            -> False

instance == TypeContext
where
	(==) (TypeContext a) (TypeContext b) = a == b

childTypes :: !Type -> [Type]
childTypes t = case t of
	Type _ ts -> ts
	Func is r _ -> [r:is]
	Cons _ ts -> ts
	Uniq t -> [t]
	Forall vs t _ -> [t:vs]
	Var _ -> []
	Arrow mt -> case mt of ?Just t -> [t]; _ -> []
	Strict t -> [t]

subtypes :: !Type -> [Type]
subtypes t = subtypes [] [t]
where
	subtypes :: ![Type] ![Type] -> [Type]
	subtypes subs [] = subs
	subtypes subs [type:types]
		# subs = if (isMember type subs) subs [type:subs]
		= subtypes subs (childTypes type ++ types)

allRestrictions :: !Type -> [TypeRestriction]
allRestrictions (Type _ ts) = concatMap allRestrictions ts
allRestrictions (Func is r (TypeContext tc)) = tc ++ concatMap allRestrictions [r:is]
allRestrictions (Cons _ ts) = concatMap allRestrictions ts
allRestrictions (Uniq t) = allRestrictions t
allRestrictions (Forall _ t (TypeContext tc)) = tc ++ allRestrictions t
allRestrictions (Var _) = []
allRestrictions (Arrow t) = fromMaybe [] (allRestrictions <$> t)
allRestrictions (Strict t) = allRestrictions t

allVars :: !Type -> [TypeVar]
allVars t = vars [] [t]
where
	vars :: ![TypeVar] ![Type] -> [TypeVar]
	vars vs [] = vs
	vars vs [type:types]
		# vs = case type of
			Cons c _ -> if (isMember c vs) vs [c:vs]
			Var v    -> if (isMember v vs) vs [v:vs]
			_        -> vs
		= vars vs (childTypes type ++ types)

allUniversalVars :: !Type -> [TypeVar]
allUniversalVars t = vars [] [t]
where
	vars :: ![TypeVar] ![Type] -> [TypeVar]
	vars vs [] = vs
	vars vs [type:types]
		# vs = case type of
			Forall newvs _ _
				-> foldl (\vs v -> if (isMember v vs) vs [v:vs]) vs (concatMap allVars newvs)
				-> vs
		= vars vs (childTypes type ++ types)

isVar :: !Type -> Bool
isVar t = t=:(Var _)

fromVar :: !Type -> TypeVar
fromVar t = case t of
	Var v -> v
	_     -> abort "error in fromVar\n"

fromVarLenient :: !Type -> TypeVar
fromVarLenient t = case t of
	Var v    -> v
	Cons v _ -> v
	Uniq t   -> fromVarLenient t
	Strict t -> fromVarLenient t
	_        -> abort "missing case in fromVarLenient\n"


isCons :: !Type -> Bool
isCons t = t=:(Cons _ _)

isCons` :: TypeVar !Type -> Bool
isCons` v t = case t of
	Cons v` _ -> v == v`
	_         -> False

isVarOrCons` :: TypeVar !Type -> Bool
isVarOrCons` v t = case t of
	Var v`    -> v == v`
	Cons v` _ -> v == v`
	_         -> False

isType :: !Type -> Bool
isType t = t=:(Type _ _)

isFunc :: !Type -> Bool
isFunc t = t=:(Func _ _ _)

isUniq :: !Type -> Bool
isUniq t = t=:(Uniq _)

isForall :: !Type -> Bool
isForall t = t=:(Forall _ _ _)

fromForall :: !Type -> Type
fromForall t = case t of
	Forall _ t _ -> t
	_            -> abort "fromForall called on non-Forall\n"

isArrow :: !Type -> Bool
isArrow t = t=:(Arrow _)

fromArrow :: !Type -> ?Type
fromArrow t = case t of
	Arrow t -> t
	_       -> abort "fromArrow called on non-Arrow\n"

fromStrict :: !Type -> Type
fromStrict t = case t of
	Strict t -> t
	_        -> abort "fromStrict called on non-Arrow\n"

fromUnifyingAssignment :: !UnifyingAssignment -> TVAssignment
fromUnifyingAssignment (LeftToRight x) = x
fromUnifyingAssignment (RightToLeft x) = x

arity :: !Type -> Int
arity (Type _ ts) = length ts
arity (Func is _ _) = length is
arity (Var _) = 0
arity (Cons _ ts) = length ts
arity (Strict t) = arity t
arity (Uniq _) = abort "what is the arity of Uniq?\n" // TODO
arity (Forall _ _ _) = abort "what is the arity of Forall?\n" // TODO
arity (Arrow _) = abort "what is the arity of Arrow?\n" // TODO

removeTypeContexts :: !Type -> Type
removeTypeContexts (Type s ts) = Type s $ map removeTypeContexts ts
removeTypeContexts (Func is r _) = Func (map removeTypeContexts is) (removeTypeContexts r) (TypeContext [])
removeTypeContexts (Var v) = Var v
removeTypeContexts (Cons v ts) = Cons v $ map removeTypeContexts ts
removeTypeContexts (Uniq t) = Uniq $ removeTypeContexts t
removeTypeContexts (Forall ts t _) = Forall (map removeTypeContexts ts) (removeTypeContexts t) (TypeContext [])
removeTypeContexts (Arrow t) = Arrow (removeTypeContexts <$> t)
removeTypeContexts (Strict t) = Strict (removeTypeContexts t)

constructorsToFunctions :: !TypeDef -> [(String,Type,?Priority)]
constructorsToFunctions {td_name,td_uniq,td_args,td_rhs} = case td_rhs of
	TDRCons _ conses        -> map consfun conses
	TDRMoreConses conses    -> map consfun conses
	TDRNewType cons         -> [consfun cons]
	TDRAbstractNewType cons -> [consfun cons]
	_                       -> []
where
	consfun :: !Constructor -> (String, Type, ?Priority)
	consfun c = (c.cons_name, Func c.cons_args ret c.cons_context, c.cons_priority)
	where ret = if td_uniq Uniq id $ Type td_name td_args

selectorsToFunctions :: !TypeDef -> [(String,Type)]
selectorsToFunctions {td_name,td_uniq,td_args,td_rhs=TDRRecord _ _ fields}
	= [(f.rf_name, Func [strict arg] (unStrict f.rf_type) (TypeContext [])) \\ f <- fields]
where
	arg = if td_uniq Uniq id $ Type td_name td_args
	unStrict t = case t of
		Strict t -> t
		t        -> t
selectorsToFunctions _ = []

td_name :: !TypeDef -> String
td_name {td_name} = td_name

td_uniq :: !TypeDef -> Bool
td_uniq {td_uniq} = td_uniq

td_rhs :: !TypeDef -> TypeDefRhs
td_rhs {td_rhs} = td_rhs

strict :: !Type -> Type
strict t = case t of
	Strict _ -> t
	_ -> Strict t

typedef :: !String !Bool ![Type] !TypeDefRhs -> TypeDef
typedef name uniq args rhs
	= {td_name=name, td_uniq=uniq, td_args=args, td_rhs=rhs}

constructor :: !String ![Type] ![TypeVar] !TypeContext !(?Priority) -> Constructor
constructor name args exi_vars tc pri
	= {cons_name=name, cons_args=args, cons_exi_vars=exi_vars, cons_context=tc, cons_priority=pri}

recordfield :: !String !Type -> RecordField
recordfield selector type = {rf_name=selector, rf_type=type}

removeDupTypedefs :: ![TypeDef] -> [TypeDef]
removeDupTypedefs [] = []
removeDupTypedefs [td:tds]
	= [td:removeDupTypedefs $ filter (\d -> d.td_name <> td.td_name) tds]

typeRhsRestrictions :: !TypeDefRhs -> [TypeRestriction]
typeRhsRestrictions (TDRCons _ cs) = flatten [c \\ {cons_context=TypeContext c} <- cs]
typeRhsRestrictions (TDRNewType {cons_context=TypeContext c}) = c
typeRhsRestrictions (TDRMoreConses cs) = flatten [c \\ {cons_context=TypeContext c} <- cs]
typeRhsRestrictions (TDRRecord _ _ _) = []
typeRhsRestrictions (TDRSynonym _) = []
typeRhsRestrictions (TDRAbstract _) = []
typeRhsRestrictions (TDRAbstractNewType {cons_context=TypeContext c}) = c
typeRhsRestrictions (TDRAbstractSynonym _) = []
