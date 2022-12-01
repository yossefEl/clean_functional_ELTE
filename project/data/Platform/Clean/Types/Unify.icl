implementation module Clean.Types.Unify

import StdArray
import StdBool
from StdFunctions import const, id, o
import StdList
import StdOrdList
import StdTuple
import StdString

import Clean.Types
import Clean.Types.Util
import Control.Applicative
import Control.Monad
import Control.Monad.State
from Data.Func import $
import Data.Functor
import Data.Functor.Identity
import Data.GenEq
import Data.List
from Data.Map import :: Map, newMap
import Data.Maybe
from Text import concat4

derive gEq Type, TypeRestriction, Kind, TypeContext

isGeneralisingUnifier :: ![TVAssignment] -> Bool
isGeneralisingUnifier tvas = isGeneralisingOrSpecialisingUnifier True tvas

isSpecialisingUnifier :: ![TVAssignment] -> Bool
isSpecialisingUnifier tvas = isGeneralisingOrSpecialisingUnifier False tvas

isGeneralisingOrSpecialisingUnifier :: !Bool ![TVAssignment] -> Bool
isGeneralisingOrSpecialisingUnifier generalising tvas = all isOk $ groupVars tvas []
where
	isOk :: ![Type] -> Bool
	isOk ts =
		not variablesFromSameTypeUnifiedWithEachOther &&
		not variableUnifiedWithNonVariable &&
		not universallyQuantifiedVariableUnified
	where
		/* Variables in the left type start with `l`, variables in the right
		 * type start with `r`. We can implement the check for a generalising
		 * unifier identical to the check for a specialising unifier, as long
		 * as we choose the correct variable prefix. */
		r_or_l = if generalising 'r' 'l'
		l_or_r = if generalising 'l' 'r'

		/* If t generalises u, we don't want to see two variables from u to be
		 * unified with each other. For example, `a -> a` does not generalise
		 * `b -> c`, because this would require `b ~= c`. */
		variablesFromSameTypeUnifiedWithEachOther =
			length [v \\ Var v <- ts | v.[0] == r_or_l] >= 2

		/* If t generalises u, we don't want to see a variable from u to be
		 * unified with a non-variable. For example, `Int` does not generalise
		 * `a` because this would require `a ~= Int`. */
		variableUnifiedWithNonVariable =
			any (not o isVar) ts &&
			any (\t -> isVar t && (fromVar t).[0] == r_or_l) ts

		/* If t generalises u, we don't want to see a universally quantified
		 * variable from t to be unified. For example, `A.a: a` does not
		 * generalise `b`, because this would require `a ~= b`. */
		universallyQuantifiedVariableUnified =
			any isUniversallyQuantifiedVariableFromGeneralType ts
		where
			isUniversallyQuantifiedVariableFromGeneralType (Var v) =
				size v > 2 && v.[0] == '_' && isDigit v.[1] && v.[2] == l_or_r
			isUniversallyQuantifiedVariableFromGeneralType _ =
				False

isIsomorphicUnifier :: ![TVAssignment] -> Bool
isIsomorphicUnifier tvas = all isOk $ groupVars tvas []
where
	isOk :: ![Type] -> Bool
	isOk [Var v, Var w] = v.[0] <> '_' && w.[0] <> '_'
	isOk _ = False

groupVars :: ![TVAssignment] ![[Type]] -> [[Type]]
groupVars []           groups = map removeDup groups
groupVars [(v,t):rest] groups = groupVars rest [[Var v,t:flatten yes]:no]
where
	(yes,no) = partition (\g -> isMember (Var v) g || isMember t g) groups

(generalises) infix 4 :: !Type !Type -> Bool
(generalises) a b = a` generalises` b`
where
	(_, a`) = prepare_unification True  (const False) newMap a
	(_, b`) = prepare_unification False (const False) newMap b

(generalises`) infix 4 :: !Type !Type -> Bool
(generalises`) a b = maybe False isGeneralisingUnifier $ unify a b

(specialises) infix 4 :: !Type !Type -> Bool
(specialises) a b = a` specialises` b`
where
	(_, a`) = prepare_unification True  (const False) newMap a
	(_, b`) = prepare_unification False (const False) newMap b

(specialises`) infix 4 :: !Type !Type -> Bool
(specialises`) a b = maybe False isSpecialisingUnifier $ unify a b

(isomorphic_to) infix 4 :: !Type !Type -> Bool
(isomorphic_to) a b = a` isomorphic_to` b`
where
	(_, a`) = prepare_unification True  (const False) newMap a
	(_, b`) = prepare_unification False (const False) newMap b

(isomorphic_to`) infix 4 :: !Type !Type -> Bool
(isomorphic_to`) a b = maybe False isIsomorphicUnifier $ unify a b

prepare_unification :: !Bool (String -> Bool) (Map String [TypeDef]) !Type -> ([TypeDef], Type)
prepare_unification isleft alwaysUnique db t
# (syns, t) = resolve_synonyms db t
# t = unpropagate_uniqueness alwaysUnique t
# t = reduceArities t
# t = evalState (renameAndRemoveStrictness t) 1
= (syns, t)
where
	prep = if isleft "l" "r"

	/* Strictness is removed because it is irrelevant for unification.
	 * All variables receive a `l` or `r` as a prefix (depending on whether it
	 * is the 'left' or 'right' type, to ensure that all variable names are
	 * unique.
	 * We remove `Forall` and treat universally quantified variables as normal
	 * variables. However, we prefix them with an underscore to recognize them
	 * in `checkUniversalisedVariables`. These variables also get an index
	 * representing the scope, so that `T (A.a: a) (A.a: a)` maps to
	 * `T _1la _2la` and can be distinguished from `A.a: T a a` which maps to
	 * `T _1la _1la`. */
	renameAndRemoveStrictness :: !Type -> State Int Type
	renameAndRemoveStrictness (Var v) =
		pure $ Var (prep +++ v)
	renameAndRemoveStrictness (Cons c ts) =
		Cons (prep +++ c) <$> mapM renameAndRemoveStrictness ts
	renameAndRemoveStrictness (Type t ts) =
		Type t <$> mapM renameAndRemoveStrictness ts
	renameAndRemoveStrictness (Func is r (TypeContext tc)) =
		liftM3 Func
			(mapM renameAndRemoveStrictness is)
			(renameAndRemoveStrictness r)
			((\tc -> TypeContext tc) <$> mapM (inTC renameAndRemoveStrictness) tc)
	renameAndRemoveStrictness (Uniq t) =
		Uniq <$> renameAndRemoveStrictness t
	renameAndRemoveStrictness (Arrow t) =
		Arrow <$> (maybe (pure ?None) (fmap ?Just o renameAndRemoveStrictness) t)
	renameAndRemoveStrictness (Forall vs t tc) =
		gets id >>= \forallCounter ->
		modify inc >>|
		fromJust o
		assignAll
			[ (prep+++v, Var (concat4 "_" (toString forallCounter) prep v))
			\\ v <- map fromVarLenient vs
			] <$>
		renameAndRemoveStrictness t
	renameAndRemoveStrictness (Strict t) =
		renameAndRemoveStrictness t

	inTC f (Derivation g t) = Derivation g <$> f t
	inTC f (Instance c ts)  = Instance c <$> mapM f ts

finish_unification :: ![TypeDef] ![TVAssignment] -> Unifier
finish_unification syns tvs
# (tvs1, tvs2) = (filter (startsWith 'l') tvs, filter (startsWith 'r') tvs)
# (tvs1, tvs2) = (map removePrefixes tvs1, map removePrefixes tvs2)
= {assignments=sortBy order (map LeftToRight tvs1 ++ map RightToLeft tvs2), used_synonyms=removeDupTypedefs syns}
where
	startsWith :: !Char !TVAssignment -> Bool
	startsWith c (h,_) =
		h.[0] == c || // normal variable
		h.[0] == '_' && h.[1] == c || // normal variable unified with universally quantified variable
		h.[0] == '_' && isDigit h.[1] && h.[2] == c // universally quantified variable

	removePrefixes :: !TVAssignment -> TVAssignment
	removePrefixes (v,t) = (rm v, fromJust $ assignAll (map (\v->(v,Var (rm v))) $ allVars t) t)
	where
		rm s = s % (start, size s - 1)
		where
			start = if (s.[0] == '_') (if (isDigit s.[1]) 3 2) 1

	order :: !UnifyingAssignment !UnifyingAssignment -> Bool
	order ua1 ua2
	| isMember v1 (allVars t2) = False
	| isMember v2 (allVars t1) = True
	| otherwise                = True // don't care
	where
		(v1,t1) = fromUnifyingAssignment ua1
		(v2,t2) = fromUnifyingAssignment ua2

:: UnificationState =
	{ assignments    :: ![TVAssignment]
	, goals          :: ![(Type, Type)]
	, used_variables :: ![TypeVar] // universally quantified variables can only be used once
	}
assignments    s :== s.UnificationState.assignments
goals          s :== s.goals
used_variables s :== s.used_variables

:: UnifyM t :== StateT UnificationState ? t

fail :: UnifyM a
fail = StateT \_ -> ?None

succeed :: UnifyM ()
succeed = pure ()

applyAssignment :: !TypeVar !Type -> UnifyM ()
// The second alternative assumes a universal variable is always the first
// argument, so if not we swap the two.
applyAssignment v (Var w) | v.[0] <> '_' && w.[0] == '_' =
	applyAssignment w (Var v)
applyAssignment v t =
	checkUniversalisedVariables v t >>= \t ->
	checkCircularAssignment v t >>|
	gets goals >>= mapM (assign` (v,t)) >>= \goals ->
	modify \s ->
	{ s
	& assignments = [(v,t):s.UnificationState.assignments]
	, goals = goals
	, used_variables = if (t=:(Var _)) [v,fromVar t:s.used_variables] [v:s.used_variables]
	}
where
	checkUniversalisedVariables :: !TypeVar !Type -> UnifyM Type
	checkUniversalisedVariables v t
	| v.[0] <> '_' = case t of
		Var v -> if (v.[0] == '_') fail (pure t)
		_     -> pure t
	| otherwise = case t of
		Var v2 -> gets used_variables >>= \used
			| isMember v used = fail
			| isMember v2 used = fail
			| isMember v2` used = fail
			| otherwise =
				applyInGoals (v,Var v2`) >>|
				pure (Var v2`)
			with
				v2` = if (v2.[0] == '_') v2 ("_" +++ v2)
		_     -> fail
	where
		applyInGoals :: !TVAssignment -> UnifyM ()
		applyInGoals tva =
			gets goals >>=
			mapM (\(t,u) -> case (assign tva t, assign tva u) of
				(?Just t`, ?Just u`) -> pure (t`,u`)
				_                    -> fail) >>= \gs ->
			modify (\s -> {s & goals=gs})

	checkCircularAssignment :: !TypeVar !Type -> UnifyM ()
	checkCircularAssignment v t
	| isMember v (allVars t) = fail
	| otherwise              = succeed

	assign` :: !TVAssignment !(!Type,!Type) -> UnifyM (!Type,!Type)
	assign` a=:(v,_) (t,u) = case (assign a t, assign a u) of
		(?Just t, ?Just u) -> pure (t,u)
		_                  -> fail

unify :: !Type !Type -> ?[TVAssignment]
unify t u = evalStateT loopUntilDone {assignments=[], goals=[(t,u)], used_variables=[]}
where
	loopUntilDone :: UnifyM [TVAssignment]
	loopUntilDone = gets goals >>= \goals -> case goals of
		[(t1,t2):_] -> modify (\s -> {s & goals=tl s.goals}) >>| uni t1 t2 >>| loopUntilDone
		[]          -> gets assignments

uni :: !Type !Type -> UnifyM ()
uni (Var v) t = if (t == Var v) succeed (applyAssignment v t)
uni t (Var v) = applyAssignment v t
uni (Type t tas) (Type u uas) = if (t==u) (addGoals tas uas) fail
uni (Cons c cas) (Type t tas)
| lc <= lt = addGoals cas end >>| applyAssignment c (Type t begin)
where
	(lc,lt) = (length cas, length tas)
	(begin,end) = splitAt (lt - lc) tas
uni t=:(Type _ _) c=:(Cons _ _) = uni c t
uni (Cons c1 as1) (Cons c2 as2)
| l1 == l2  = addGoals as1 as2 >>| if (c1 == c2) succeed (applyAssignment c1 (Var c2))
| l1 <  l2  = addGoals as1 end >>| applyAssignment c1 (Cons c2 begin) with (begin,end) = splitAt (l2-l1) as2
| otherwise = addGoals end as2 >>| applyAssignment c2 (Cons c1 begin) with (begin,end) = splitAt (l1-l2) as1
where (l1,l2) = (length as1, length as2)
uni (Func [] r1 _) (Func [] r2 _) = addGoal r1 r2
uni (Func [] r _) t = addGoal r t
uni t (Func [] r _) = addGoal t r
uni (Func [i1] r1 _) (Func [i2] r2 _) = addGoal i1 i2 >>| addGoal r1 r2
uni (Uniq a) (Uniq b) = addGoal a b
uni (Arrow ?None) (Arrow ?None) = succeed
uni (Arrow (?Just t)) (Arrow (?Just u)) = addGoal t u
uni (Cons c cas=:[_:_]) (Func [fa] r _) =
	addGoal (last cas) r >>|
	addGoal (case init cas of [] -> Var c; cas -> Cons c cas) (Arrow (?Just fa))
uni f=:(Func _ _ _) c=:(Cons _ _) = uni c f
uni (Cons c [a]) (Arrow (?Just t)) = addGoal a t >>| applyAssignment c (Arrow ?None)
uni a=:(Arrow _) c=:(Cons _ _) = uni c a
uni _ _ = fail

addGoal :: !Type !Type -> UnifyM ()
addGoal t u = modify (\s -> {s & goals=[(t,u):s.goals]})

addGoals :: ![Type] ![Type] -> UnifyM ()
addGoals [t:ts] [u:us] = addGoal t u >>| addGoals ts us
addGoals []     []     = succeed
addGoals _      _      = fail
