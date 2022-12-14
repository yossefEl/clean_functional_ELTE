implementation module Clean.Types.Parse

import StdEnv

import Clean.Types
import Clean.Types.Util
import Data.Either
import Data.GenEq
import Data.Maybe
from Data.Func import $
from Data.List import instance Functor [], instance pure [], instance <*> [],
	instance Alternative []
from Text import class Text(concat), instance Text String
import Data.Functor
import Control.Applicative
import Control.Monad
from Text.Parsers.Simple.Core import :: Parser,
	instance Functor (Parser t), instance pure (Parser t),
	instance <*> (Parser t), instance Alternative (Parser t),
	instance Monad (Parser t), pList, pMany, pPeek, pSatisfy, pSepBy, pSepBy1,
	pToken, runParser

(|<<) infixl 1 :: !(m a) !(m b) -> m a | Monad m
(|<<) ma mb = ma >>= \a -> mb >>= \_ -> pure a

:: Token
	= TIdent String            // UpperCaseId or FunnyId
	| TVar String              // LowerCaseId
	| TInt Int

	| TArrow                   // ->
	| TComma                   // ,
	| TStar                    // *
	| TAnonymous               // .
	| TUnboxed                 // #
	| TStrict                  // !
	| TTailStrict              // ! in `[ !`, i.e. preceded by whitespace
	| TColon                   // :
	| TUniversalQuantifier     // A.
	| TPipe                    // |
	| TAmpersand               // &
	| TLtEq                    // <=
	| TMaybe !Int              // 0: ?; 1: ?^; 2: ?#

	| TParenOpen | TParenClose // ( )
	| TBrackOpen | TBrackClose // [ ]
	| TBraceOpen | TBraceClose // { }

instance == Token
where
	(==) (TIdent a) b = case b of
		TIdent b -> a == b
		_        -> False
	(==) (TVar a) b = case b of
		TVar b   -> a == b
		_        -> False
	(==) (TInt a) b = case b of
		TInt b   -> a == b
		_        -> False
	(==) TArrow      b          = b=:TArrow
	(==) TComma      b          = b=:TComma
	(==) TStar       b          = b=:TStar
	(==) TAnonymous  b          = b=:TAnonymous
	(==) TUnboxed    b          = b=:TUnboxed
	(==) TStrict     b          = b=:TStrict
	(==) TTailStrict b          = b=:TTailStrict
	(==) TColon      b          = b=:TColon
	(==) TUniversalQuantifier b = b=:TUniversalQuantifier
	(==) TPipe       b          = b=:TPipe
	(==) TAmpersand  b          = b=:TAmpersand
	(==) TLtEq       b          = b=:TLtEq
	(==) (TMaybe a) b = case b of
		TMaybe b -> a == b
		_        -> False
	(==) TParenOpen  b          = b=:TParenOpen
	(==) TParenClose b          = b=:TParenClose
	(==) TBrackOpen  b          = b=:TBrackOpen
	(==) TBrackClose b          = b=:TBrackClose
	(==) TBraceOpen  b          = b=:TBraceOpen
	(==) TBraceClose b          = b=:TBraceClose

isTIdent t = t=:(TIdent _)
isTVar   t = t=:(TVar _)
isTMaybe t = t=:(TMaybe _)

tokenize :: ([Char] -> ?[Token])
tokenize = fmap reverse o tkz []
where
	tkz :: ![Token] ![Char] -> ?[Token]
	tkz tks [] = ?Just tks
	tkz tks ['->':cs] = tkz [TArrow:tks]               cs
	tkz tks [',':cs]  = tkz [TComma:tks]               cs
	tkz tks ['*':cs]  = tkz [TStar:tks]                cs
	tkz tks ['.':cs]  = tkz [TAnonymous:tks]           cs
	tkz tks ['#':cs]  = tkz [TUnboxed:tks]             cs
	tkz tks ['!':cs]  = tkz [TStrict:tks]              cs
	tkz tks ['(':cs]  = tkz [TParenOpen:tks]           cs
	tkz tks [')':cs]  = tkz [TParenClose:tks]          cs
	tkz tks ['[':cs]  = case span isSpace cs of
		([_:_], ['!':cs]) -> tkz [TTailStrict,TBrackOpen:tks] cs
		(_, cs)           -> tkz [TBrackOpen:tks] cs
	tkz tks [']':cs]  = tkz [TBrackClose:tks]          cs
	tkz tks ['{':cs]  = tkz [TBraceOpen:tks]           cs
	tkz tks ['}':cs]  = tkz [TBraceClose:tks]          cs
	tkz tks ['A.':cs] = tkz [TUniversalQuantifier:tks] cs
	tkz tks [':':cs]  = tkz [TColon:tks]               cs
	tkz tks ['|':cs]  = tkz [TPipe:tks]                cs
	tkz tks ['&':cs]  = tkz [TAmpersand:tks]           cs
	tkz tks ['<=':cs] = tkz [TLtEq:tks]                cs
	tkz tks ['?^':cs] = tkz [TMaybe 1:tks]             cs
	tkz tks ['?#':cs] = tkz [TMaybe 2:tks]             cs
	tkz tks ['?':cs]  = tkz [TMaybe 0:tks]             cs
	tkz tks [c:cs]
	| isSpace c = tkz tks cs
	| isUpper c = tkz [TIdent $ toString [c:id]:tks] cs`
		with (id, cs`) = span isIdentChar cs
	| isFunny c = tkz [TIdent $ toString [c:id]:tks] cs`
		with (id, cs`) = span isFunny cs
	| isLower c = tkz [TVar $ toString [c:var]:tks] cs`
		with (var, cs`) = span isIdentChar cs
	| isDigit c = tkz [TInt (toInt (toString [c:int])):tks] cs`
		with (int, cs`) = span isDigit cs
	tkz _ _ = ?None

	isIdentChar :: !Char -> Bool
	isIdentChar c = any (\f->f c) [isLower, isUpper, isDigit, (==)'_', (==) '`']

	isFunny :: !Char -> Bool
	isFunny c = isMember c ['~@#$%^?!+-*<>\\/|&=:']

type :: Parser Token Type
type =
	liftM3 Func (some argtype) (pToken TArrow >>| type) optContext
	<|> addContextAsConstFunction (liftM2 Cons cons $ some argtype)
	<|> addContextAsConstFunction (liftM2 Type (ident <|> maybe) $ many argtype)
	<|> arrayOrList True
	<|> liftM2 (\cs -> Type ("_Tuple" +++ toString (length cs+1)))
		(parenthised (some (pToken TComma))) (some argtype)
	<|> liftM3 Forall
		(pToken TUniversalQuantifier >>| some (Var <$> var <|> Uniq <$> uniq (Var <$> var)) |<< pToken TColon)
		type
		optContext
	<|> addContextAsConstFunction argtype
where
	argtype :: Parser Token Type
	argtype = (pList [pToken TParenOpen, pToken TParenClose] $> Type "_Unit" [])
		<|> parenthised type
		<|> liftM (\t -> Type t []) (ident <|> maybe)
		<|> liftM Uniq (uniq argtype)
		<|> liftM Uniq (uniq (liftM2 Cons cons  $ some argtype))
		<|> liftM Uniq (uniq (liftM2 Type ident $ some argtype))
		<|> arrayOrList False
		<|> liftM (\ts -> Type ("_Tuple" +++ toString (length ts)) ts)
			(parenthised (pSepBy1 type (pToken TComma)))
		<|> liftM2 (\a r -> Func [a] r (TypeContext [])) (parenthised (pToken TArrow) >>| argtype) argtype
		<|> liftM (Arrow o ?Just) (parenthised (pToken TArrow) >>| argtype)
		<|> liftM (const (Arrow ?None)) (parenthised (pToken TArrow))
		<|> (pToken TStrict >>| argtype)           // ! ignored for now
		<|> (pToken TAnonymous >>| argtype)        // . ignored for now
		<|> (unqvar >>| pToken TColon >>| argtype) // u: & friends ignored for now
		<|> liftM Var var

	/* When prefix is True, accept e.g. `[] a`. Otherwise, accept `[a]`. In
	 * both cases `[]` is accepted. */
	arrayOrList prefix =
		    circumfixOrPrefix "_#Array"   braced  (pToken TUnboxed) (pure ())
		<|> circumfixOrPrefix "_32#Array" braced  (pToken (TInt 32) >>| pToken TUnboxed) (pure ())
		<|> circumfixOrPrefix "_!Array"   braced  (pToken TStrict) (pure ())
		<|> circumfixOrPrefix "_Array"    braced  (pure ()) (pure ())
		<|> circumfixOrPrefix "_#List!"   bracked (pToken TUnboxed) (pToken TStrict)
		<|> circumfixOrPrefix "_!List!"   bracked (pToken TStrict) (pToken TStrict)
		<|> circumfixOrPrefix "_#List"    bracked (pToken TUnboxed) (pure ())
		<|> circumfixOrPrefix "_!List"    bracked (pToken TStrict) (pure ())
		<|> circumfixOrPrefix "_List!"    bracked (pure ()) (pToken TStrict <|> pToken TTailStrict)
		<|> circumfixOrPrefix "_List"     bracked (pure ()) (pure ())
	where
		circumfixOrPrefix :: String (A.a: (Parser Token a) -> Parser Token a) (Parser Token b) (Parser Token c) -> Parser Token Type
		circumfixOrPrefix name parens before after
			| prefix =
				Type name <$> (parens (before >>| after) >>| pMany argtype)
			| otherwise =
				Type name o maybeToList <$> parens (before >>| (?Just <$> type <|> pure ?None) |<< after)

	ident :: Parser Token String
	ident = (\tk -> case tk of TIdent id -> id; _ -> abort "error in type parser\n") <$> pSatisfy isTIdent

	var :: Parser Token TypeVar
	var = (\tk -> case tk of TVar id -> id; _ -> abort "error in type parser\n") <$> pSatisfy isTVar
	cons = var
	unqvar = var

	maybe :: Parser Token String
	maybe = toString <$> pSatisfy isTMaybe
	where
		toString (TMaybe 0) = "_!Maybe"
		toString (TMaybe 1) = "_Maybe"
		toString (TMaybe 2) = "_#Maybe"
		toString _          = abort "error in type parser\n"

	uniq :: (Parser Token Type) -> Parser Token Type
	uniq parser = pToken TStar >>| parser

	optContext :: Parser Token TypeContext
	optContext = liftM2 (\(TypeContext a) (TypeContext b)->TypeContext (a ++ b)) (context <|> pure (TypeContext [])) (uniquenessEqualities <|> pure (TypeContext []))

	addContextAsConstFunction :: (Parser Token Type) -> Parser Token Type
	addContextAsConstFunction parser =
		parser >>= \t -> pPeek >>= \tks -> case tks of
			[TPipe:_] ->  (pure (TypeContext []) <|> optContext) >>= \c -> case c of
				TypeContext [] -> pure t
				c              -> pure $ Func [] t c
			_ -> pure t

	context :: Parser Token TypeContext
	context = pToken TPipe >>| typeContext o flatten <$> pSepBy context` (pToken TAmpersand)
	where
		context` :: Parser Token [TypeRestriction]
		context` = pSepBy classOrGeneric (pToken TComma) >>= \restrictions ->
			some argtype >>= \ts ->
			mapM (flip ($) ts) restrictions

		classOrGeneric :: Parser Token ([Type] -> Parser Token TypeRestriction)
		classOrGeneric = className >>= \name ->
			optional (braced $ piped skipKind) >>= \kind ->
			case kind of
				?None   -> pure $ pure o Instance name
				?Just _ -> pure $ deriv name
		where
			deriv :: !String ![Type] -> Parser Token TypeRestriction
			deriv d [t] = pure $ Derivation d t
			deriv _ _   = empty

		className :: Parser Token String
		className = ident <|> var

		skipKind :: Parser Token [Token]
		skipKind = some $ pSatisfy \t -> case t of
			TStar       -> True
			TArrow      -> True
			TParenOpen  -> True
			TParenClose -> True
			_           -> False

	uniquenessEqualities :: Parser Token TypeContext
	uniquenessEqualities = pToken TComma >>| bracked (pSepBy inequality (pToken TComma)) $> TypeContext []
	where
		inequality = unqvar >>| pToken TLtEq >>| unqvar

	parenthised p = pToken TParenOpen >>| p |<< pToken TParenClose
	braced p = pToken TBraceOpen >>| p |<< pToken TBraceClose
	bracked p = pToken TBrackOpen >>| p |<< pToken TBrackClose
	piped p = pToken TPipe >>| p |<< pToken TPipe

parseType :: ![Char] -> ?Type
parseType cs = case tokenize cs of
	?None -> ?None
	?Just tokens -> case [t \\ (t,[]) <- fst $ runParser type tokens] of
		[t:_] -> ?Just t
		_     -> ?None
