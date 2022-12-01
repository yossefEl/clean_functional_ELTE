implementation module Clean.Parse.Comments

import StdDebug
// Use the following two lines to print line numbers of documentation that could not be linked to syntax elements.
//traceSkippedComments [] x = x
//traceSkippedComments [c:cc] x = trace_n ("Missed documentation on line "+++toString c.line) (traceSkippedComments cc x)
traceSkippedComments _ x :== x

import StdArray
import StdBool
import StdChar
import StdClass
import StdFunctions
import StdInt
import StdList
import StdMisc
import StdString
import StdTuple

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Error
from Data.Func import $
import Data.Functor
from Data.Map import :: Map(..), newMap, put, get
import Data.Maybe
import Data.Tuple
import System.File
import System.FilePath
from Text import class Text(concat,startsWith), instance Text String

from Heap import :: Heap, :: HeapN, :: Ptr{pointer}, :: PtrN(Ptr), readPtr
from syntax import
	:: AttrVarInfo,
	:: AttrVarInfoPtr,
	:: AType,
	:: ATypeVar,
	:: BITVECT,
	:: CheckedTypeDef,
	:: ClassDef{class_ident,class_pos},
	:: ClassInstance,
	:: ClassInstanceR,
	:: CollectedDefinitions,
	:: ComponentNrAndIndex,
	:: ConsDef,
	:: DclInstanceMemberTypeAndFunctions,
	:: Declaration,
	:: FileName,
	:: FunctionOrMacroIndex,
	:: FunctName,
	:: FunKind,
	:: FunSpecials,
	:: GenericDef{gen_ident,gen_pos},
	:: GenericCaseDef{gc_pos},
	:: Global,
	:: Ident{id_info,id_name},
	:: Import{import_file_position},
	:: ImportedObject,
	:: Index, :: LineNr,
	:: Module{mod_defs,mod_ident},
	:: ModuleN,
	:: Optional,
	:: ParsedConstructor{pc_cons_ident,pc_cons_pos},
	:: ParsedDefinition(..),
	:: ParsedExpr,
	:: ParsedImport,
	:: ParsedInstance{pi_ident,pi_pos},
	:: ParsedInstanceAndMembers{pim_pi},
	:: ParsedModule,
	:: ParsedSelector{ps_field_pos,ps_field_ident},
	:: ParsedTypeDef,
	:: Position(..),
	:: Priority,
	:: Rhs,
	:: RhsDefsOfType(..),
	:: SelectorDef,
	:: SortedQualifiedImports,
	:: STE_BoundTypeVariable,
	:: STE_Kind(..),
	:: SymbolPtr,
	:: SymbolTable,
	:: SymbolTableEntry{ste_kind},
	:: SymbolType,
	:: TypeCons,
	:: TypeDef{td_ident,td_pos,td_rhs},
	:: TypeRhs,
	:: TypeVarInfo,
	:: TypeVarInfoPtr,
	:: VarInfo,
	:: VarInfoPtr

scanComments :: !FilePath !*env -> *(!MaybeError FileError [CleanComment], !*env) | FileSystem env
scanComments fp w
# (s,w) = readFile fp w
| isError s = (Error (fromError s), w)
# s = fromOk s
# (cmnts,ss) = scan {defaultScanState & input=s}
= (Ok cmnts, w)

scanCommentsFile :: !*File -> *(!MaybeError FileError [CleanComment], !*File)
scanCommentsFile f
# (s,f) = readAll f
| isError s = (Error (fromError s), f)
# s = fromOk s
# (cmnts,ss) = scan {defaultScanState & input=s}
= (Ok cmnts, f)

:: ScanState =
	{ comment_level :: !Int
	, comment_idxs  :: ![(Int,Int,Int)] // line, col, idx
	, ln            :: !Int
	, col           :: !Int
	, input         :: !String
	, idx           :: !Int
	}

defaultScanState :: ScanState
defaultScanState =
	{ comment_level = 0
	, comment_idxs  = []
	, ln            = 1
	, col           = 0
	, input         = ""
	, idx           = 0
	}

advance :: !ScanState -> ScanState
advance ss = case ss.input.[ss.idx] of
	'\t' -> {ss &             col=ss.col+4, idx=ss.idx+1} // We assume that there are no tabs in a line
	'\n' -> {ss & ln=ss.ln+1, col=0,        idx=ss.idx+1}
	_    -> {ss &             col=ss.col+1, idx=ss.idx+1}

scan :: !ScanState -> (![CleanComment], !ScanState)
scan ss=:{idx}
| idx >= size ss.input = ([], ss)
| otherwise = case [ss.input.[i] \\ i <- [idx..]] of
	[s:_] | s=='\r' || s=='\n' || s=='\t'
		-> scan (advance ss)
	['//':_]
		| ss.comment_level <> 0
			-> scan (scan_to_newline ss)
		# cmnt =
			{ line      = ss.ln
			, column    = ss.col
			, level     = ?None
			, content   = ""
			, multiline = False
			}
		# ss = scan_to_newline ss
		# content = ss.input % (idx+2,ss.idx-1)
		# (extra_content,ss) = collect_single_line_comments cmnt.line cmnt.column ss
		# cmnt & content = concat [content:extra_content]
		# (cmnts,ss) = scan ss
		-> ([cmnt:cmnts],ss)
	['/*':_]
		-> scan
			{ ss & idx=idx+2, col=ss.col+2
			, comment_level = ss.comment_level+1
			, comment_idxs  = [(ss.ln, ss.col, idx+2):ss.comment_idxs]
			}
	['*/':_] | ss.comment_level > 0
		# (c_ln,c_col,c_idx) = hd ss.comment_idxs
		# level = ss.comment_level
		# cmnt =
			{ line      = c_ln
			, column    = c_col
			, level     = ?Just level
			, content   = ss.input % (c_idx, idx-1)
			, multiline = True
			}
		# (cmnts,ss) = scan
			{ ss & idx=idx+2, col=ss.col+2
			, comment_level = ss.comment_level-1
			, comment_idxs  = tl ss.comment_idxs
			}
		# (before,after) = span (\c -> isJust c.level && fromJust c.level < level) cmnts
		-> (before ++ [cmnt:after],ss)
	['[':_] | ss.comment_level == 0
		-> scan (skip_list_literal (advance ss))
	['"':_] | ss.comment_level == 0
		-> scan (skip_string_literal '"' (advance ss))
	_
		-> scan (advance ss)
where
	collect_single_line_comments :: !Int !Int !ScanState -> (![String], !ScanState)
	collect_single_line_comments ln col ss
	# ss=:{idx} = skip_whitespace ss
	| ss.ln==ln+1 && ss.col==col
			&& ss.idx<size ss.input-2
			&& ss.input.[idx]=='/' && ss.input.[idx+1]=='/'
		# ss = scan_to_newline ss
		# content = ss.input % (idx+2,ss.idx-1)
		# (cmnts,ss) = collect_single_line_comments (ln+1) col ss
		= ([content:cmnts],ss)
		= ([],ss)

scan_to_newline :: !ScanState -> ScanState
scan_to_newline ss
| ss.idx >= size ss.input = ss
# c = ss.input.[ss.idx]
# ss = advance ss
= if (c=='\n') ss (scan_to_newline ss)

skip_whitespace :: !ScanState -> ScanState
skip_whitespace ss
| ss.idx >= size ss.input = ss
# c = ss.input.[ss.idx]
| isSpace c = skip_whitespace (advance ss)
| otherwise = ss

skip_list_literal :: !ScanState -> ScanState
skip_list_literal ss
| ss.idx >= size ss.input = ss
# c = ss.input.[ss.idx]
| isSpace c = skip_list_literal (advance ss)
| c == '\'' = skip_string_literal '\'' (advance ss)
| otherwise = ss

skip_string_literal :: !Char !ScanState -> ScanState
skip_string_literal term ss
| ss.idx >= size ss.input = ss
# c = ss.input.[ss.idx]
| c == term = advance ss
| c == '\\' = skip_escape_sequence (advance ss)
| otherwise = skip_string_literal term (advance ss)
where
	skip_escape_sequence :: !ScanState -> ScanState
	skip_escape_sequence ss
	| ss.idx >= size ss.input = ss
	# [c1,c2,c3,c4:_] = [ss.input.[i] \\ i <- [ss.idx..]]
	= case c1 of
		'x'
			| isHexDigit c2
				| isHexDigit c3 -> iter 3 advance ss
				| otherwise -> twice advance ss
			| otherwise -> advance ss
		'0'
			| isOctDigit c2
				| isOctDigit c3
					| isOctDigit c4 -> iter 4 advance ss
					| otherwise -> iter 3 advance ss
				| otherwise -> twice advance ss
			| otherwise -> advance ss
		_ -> twice advance ss

:: CollectedComments :== Map CommentIndex CleanComment

:: CommentIndex = CI String Position String

instance < Position
where
	(<) a b = index a < index b
	where
		index (LinePos f l)  = (f,   l, "")
		index (PreDefPos id) = ("", -1, id.id_name)
		index NoPos          = ("", -2, "")

instance < CommentIndex where (<) (CI a b c) (CI d e f) = (a,b,c) < (d,e,f)

putCC k v coll :== case commentIndex k of
	?None   -> coll
	?Just k -> put k v coll

emptyCollectedComments :: CollectedComments
emptyCollectedComments = newMap

getComment :: !a !CollectedComments -> ?String | commentIndex a
getComment elem coll = (\cc -> cc.content) <$> (flip get coll =<< commentIndex elem)

collectComments :: ![CleanComment] !ParsedModule -> CollectedComments
collectComments comments pm
# comments = filter (\c -> startsWith "*" c.content) comments
# coll = newMap
# (comments,coll) = case comments of
	[] -> ([], coll)
	[c:cs]
		| c.line <= 3 -> (cs, putCC pm c coll)
		| otherwise -> (comments, coll)
# (cc,coll) = collect comments pm.mod_defs ?None coll
= traceSkippedComments cc coll

/**
 * The basic idea: we go through all syntax elements in order, and select the
 * most appropriate comment. All comments before it are discarded.
 *
 * There are two kinds of elements:
 * 1. Those that expect single- or multi-line documentation above it. This goes
 *    for all top-level elements as well as class members.
 * 2. Those that expect single-line documentation below it, and do not accept
 *    multi-line documentation (although multiple single-line comments are
 *    accepted and concatenated). This single-line documentation should not
 *    start in column 0. This goes for constructors and record selectors.
 *
 * For elements of type 1 we find the last comment above the element; for
 * elements of type 2 we find the first single-line comment below it, as long
 * as there are no intervening elements.
 *
 * @param The comments to match unto syntax elements.
 * @param The syntax elements. For types and classes, we recurse into `collect`
 *   with the list of constructors / selectors / members.
 * @param Optionally the next syntax element (after the main list of syntax
 *   elements). On the top level this will be `?None`, but it may not be for
 *   list of constructors / selectors / class members. This is needed for
 *   correctly assigning comments to constructors and selectors, to ensure that
 *   no syntax elements appear in between the last constructor / selector and
 *   the comment.
 * @param The set of already assigned comments.
 * @result The comments that have not been assigned to any element.
 * @result The assigned comments.
 */
collect :: ![CleanComment] ![a] !(?Next) !CollectedComments -> (![CleanComment], !CollectedComments) | canHaveComments a
collect cc [] mbNextX coll
	= (cc, coll)
collect cc [x:xs] mbNextX coll
	# (cc,coll) = case bestComment x (listToNext xs <|> mbNextX) cc of
		?None        -> (cc, coll)
		?Just (c,cc) -> (cc, putCC x c coll)
	# (cc,coll) = recurse cc (children x) (listToNext xs <|> mbNextX) coll
	= collect cc xs mbNextX coll
where
	// Compiler cannot figure out the overloading if we call collect from collect directly
	recurse :: ![CleanComment] !Children !(?Next) !CollectedComments -> (![CleanComment], !CollectedComments)
	recurse cs (Children xs) mbNextX coll = collect cs xs mbNextX coll

	bestComment :: !a (?Next) ![CleanComment] -> ?(CleanComment, [CleanComment]) | canHaveComments a
	bestComment x mbNextX cs
		| singleLineAbove x
			= lastCommentAbove x ?None cs
			= firstSingleLineBetween x mbNextX cs
	where
		// For elements that expect single-line documentation above, we simply
		// find the last comment above the element; all comments before it can
		// be ignored.
		lastCommentAbove x prev [c:cs]
			| c before x == ?Just True
				= traceSkippedComments (maybeToList prev) (lastCommentAbove x (?Just c) cs)
			// fall-through
		lastCommentAbove x prev rest = (\p -> (p, rest)) <$> prev

		// Elements that expect single-line documentation below do not accept
		// multi-line documentation. We skip until the first comment under the
		// element, but return it only if it is a single-line comment and if
		// there is no intervening element (for this `mbNextX` is used).
		firstSingleLineBetween x mbNextX [c:cs]
			| c before x == ?Just False
				| isJust mbNextX && c before (fromJust mbNextX) == ?Just False
					= ?None
				| c.multiline || c.column == 0
					= ?None
					= ?Just (c, cs)
			| otherwise
				= traceSkippedComments [c] (firstSingleLineBetween x mbNextX cs)
		firstSingleLineBetween _ _ [] = ?None

(before) {line,column,multiline} elem = case elem_line of
	?None
		-> ?None
	?Just elem_line
		| multiline
			-> ?Just (line <= elem_line)
			-> ?Just (line <  elem_line)
where
	elem_line = case pos elem of
		?Just (LinePos _ ln) -> ?Just ln
		_ -> ?None

:: Children = E.t: Children ![t] & canHaveComments t

:: Next = E.t: Next !t & canHaveComments t

listToNext :: ![a] -> ?Next | canHaveComments a
listToNext [x:_] = ?Just (Next x)
listToNext [] = ?None

class canHaveComments a | commentIndex a
where
	pos :: !a -> ?Position

	//* For recursion.
	children :: !a -> Children

	/**
	 * If true, single-line documentation should be given above the element.
	 * Multi-line documentation is always above the element.
	 */
	singleLineAbove :: !a -> Bool

instance commentIndex (Module a)
where
	commentIndex {mod_ident} = ?Just (CI "Module" NoPos mod_ident.id_name)

instance commentIndex ParsedDefinition
where
	commentIndex pd = case pd of
		PD_Function pos id is_infix args rhs kind -> ?Just (CI "PD_Function" pos id.id_name)
		PD_TypeSpec pos id prio type specials -> ?Just (CI "PD_TypeSpec" pos id.id_name)
		PD_Instance {pim_pi=pi} -> ?Just (CI "PD_Instance" pi.pi_pos pi.pi_ident.id_name)
		PD_Instances [{pim_pi=pi}:_] -> ?Just (CI "PD_Instances" pi.pi_pos pi.pi_ident.id_name)
		PD_Class cd pds -> ?Just (CI "PD_Class" cd.class_pos cd.class_ident.id_name)
		PD_Type ptd -> ?Just (CI "PD_Type" ptd.td_pos ptd.td_ident.id_name)
		PD_Generic gd -> ?Just (CI "PD_Generic" gd.gen_pos gd.gen_ident.id_name)
		_ -> ?None

instance canHaveComments ParsedDefinition
where
	pos pd = case pd of
		PD_Function pos _ _ _ _ _ -> ?Just pos
		PD_NodeDef pos _ _ -> ?Just pos
		PD_Type ptd -> ?Just ptd.td_pos
		PD_TypeSpec pos _ _ _ _ -> ?Just pos
		PD_Class cd _ -> ?Just cd.class_pos
		PD_Instance piam -> ?Just piam.pim_pi.pi_pos
		PD_Instances [piam:_] -> ?Just piam.pim_pi.pi_pos
		PD_Instances [] -> ?None
		PD_Import [pi:_] -> ?Just pi.import_file_position
		PD_Import [] -> ?None
		PD_ImportedObjects _ -> ?None
		PD_ForeignExport _ _ _ _ -> ?None
		PD_Generic gd -> ?Just gd.gen_pos
		PD_GenericCase gcd _ -> ?Just gcd.gc_pos
		PD_Derive [gcd:_] -> ?Just gcd.gc_pos
		PD_Derive [] -> ?None
		PD_Erroneous -> ?None

	children pd = case pd of
		PD_Type ptd -> case ptd.td_rhs of
			ConsList cs -> Children cs
			ExtensibleConses cs -> Children cs
			MoreConses _ cs -> Children cs
			SelectorList _ _ _ ss -> Children ss
			_ -> Children (tl [pd]) // to fix the type
		PD_Class _ pds -> Children pds
		_ -> Children (tl [pd])

	singleLineAbove _ = True

instance commentIndex ParsedSelector
where
	commentIndex ps = ?Just (CI "ParsedSelector" ps.ps_field_pos ps.ps_field_ident.id_name)

instance canHaveComments ParsedSelector
where
	pos ps = ?Just ps.ps_field_pos
	children ps = Children (tl [ps])
	singleLineAbove _ = False

instance commentIndex ParsedConstructor
where
	commentIndex pc = ?Just (CI "ParsedConstructor" pc.pc_cons_pos pc.pc_cons_ident.id_name)

instance canHaveComments ParsedConstructor
where
	pos pc = ?Just pc.pc_cons_pos
	children pc = Children (tl [pc])
	singleLineAbove _ = False

instance commentIndex Next
where
	commentIndex (Next x) = commentIndex x

instance canHaveComments Next
where
	pos (Next x) = pos x
	children (Next x) = children x
	singleLineAbove (Next x) = singleLineAbove x
