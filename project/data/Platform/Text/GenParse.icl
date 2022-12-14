implementation module Text.GenParse

import StdGeneric, StdEnv, StdOverloadedList, Data.GenEq, Data.Maybe
import Text

//---------------------------------------------------------------------------


:: StringInput = { si_str :: !String, si_pos :: !Int} 

mkStringInput :: !String -> StringInput
mkStringInput str = {si_str = str, si_pos = 0}

instance ParseInput StringInput where
	parseInput s=:{si_pos, si_str}
		#! size_str = size si_str
		| size_str == si_pos 
			= (?None, {s & si_str = si_str})
		| otherwise
			#! ch = si_str.[si_pos]
			= (?Just ch, {s & si_str = si_str, si_pos = inc si_pos})

instance ParseInput File where	
	parseInput file 
		# (ok, c, file) = sfreadc file
		| ok
			= (?Just c, file)
			= (?None, file)
						
//---------------------------------------------------------------------------

// lex tokens
:: Token 
	= TokenInt Int
	| TokenChar Char
	| TokenReal Real 
	| TokenBool Bool
	| TokenString String
	| TokenIdent String
	| TokenOpenPar
	| TokenClosePar
	| TokenOpenCurly
	| TokenCloseCurly
	| TokenOpenList
	| TokenCloseList
	| TokenComma
	| TokenEnd
	| TokenError String

instance toString Token where
	toString (TokenInt x) = toString x
	toString (TokenChar x) = toString x
	toString (TokenReal x) = toString x
	toString (TokenBool x) = toString x
	toString (TokenString x) = x
	toString (TokenIdent x) = x
	toString TokenOpenPar = "("
	toString TokenClosePar = ")"
	toString TokenOpenCurly = "{"
	toString TokenCloseCurly = "}"
	toString TokenOpenList = "["
	toString TokenCloseList = "]"	
	toString TokenComma = ","
	toString TokenEnd = "<end>"
	toString (TokenError err) = "<error: " +++ err +++ ">"

instance toString Expr where
	toString (ExprInt x) = toString x
	toString (ExprChar x) = toString x
	toString (ExprReal x) = toString x
	toString (ExprBool x) = toString x
	toString (ExprString x) = x
	toString (ExprIdent x) = x
	toString (ExprApp xs) = "(" +++ join " " [toString x\\x<-:xs] +++ ")"
	toString (ExprTuple xs) = "(" +++ join ", " [toString x\\x<-:xs] +++ ")"
	toString (ExprField name expr) = name +++ "=" +++ toString expr
	toString (ExprRecord name xs) = "{" +++ maybe "" (\n -> n +++ "|") name +++ join ", " [toString x\\x<-:xs] +++ "}"
	toString (ExprList xs) = "[" +++ join ", " (map toString xs) +++ "]"
	toString (ExprArray xs) = "{" +++ join ", " (map toString xs) +++ "}"
	toString ExprUnitBuiltin = "()"
	toString (ExprError err) = "<error: " +++ err +++ ">"
	toString ExprUnit = abort "toString on auxiliary ExprUnit\n"
	toString (ExprAppInInfix _ _ _ _) = abort "toString on auxiliary ExprAppInInfix\n"
	toString (ExprPair _ _) = abort "toString on auxiliary ExprPair\n"

instance == Expr where
    (==) x y = x === y
derive gEq Expr, GenConsAssoc

:: ParseState s =
	{ ps_input  :: !s       // lex input
	, ps_char   :: !?Char   // unget char
	, ps_tokens :: ![Token] // unget tokens
	}

lexGetChar ps=:{ps_char= ?None, ps_input}
	# (mc, ps_input) = parseInput ps_input
	= (mc, {ps & ps_input = ps_input})
lexGetChar ps=:{ps_char} = (ps_char, {ps & ps_char = ?None})

lexUngetChar c ps=:{ps_char= ?None} = {ps & ps_char = ?Just c}
lexUngetChar c ps = abort "cannot unget\n"	

isSpecialChar	:: !Char -> Bool
isSpecialChar '~'	= True
isSpecialChar '@'	= True
isSpecialChar '#'	= True
isSpecialChar '$'	= True
isSpecialChar '%'	= True
isSpecialChar '^'	= True
isSpecialChar '?'	= True
isSpecialChar '!'	= True
isSpecialChar '+'	= True
isSpecialChar '-'	= True
isSpecialChar '*'	= True
isSpecialChar '<'	= True
isSpecialChar '>'	= True
isSpecialChar '\\'	= True
isSpecialChar '/'	= True
isSpecialChar '|'	= True
isSpecialChar '&'	= True
isSpecialChar '='	= True
isSpecialChar ':'	= True
isSpecialChar '.'	= True
isSpecialChar c		= False

//----------------------------------------------------------------------------------		
// lex input

lexUngetToken token ps=:{ps_tokens} = {ps & ps_tokens = [token:ps_tokens]}

lexGetToken ps=:{ps_tokens=[token:tokens]} = (token, {ps & ps_tokens = tokens})
lexGetToken ps=:{ps_tokens=[]}
	= lex ps
where
	lex s	
		# (mc, s) = lexGetChar s
		= case mc of
			?None      -> (TokenEnd, s)
			?Just '\0' -> (TokenEnd, s)
			?Just '('  -> (TokenOpenPar, s)
			?Just ')'  -> (TokenClosePar, s)
			?Just '{'  -> (TokenOpenCurly, s)
			?Just '}'  -> (TokenCloseCurly, s)
			?Just '['  -> (TokenOpenList, s)
			?Just ']'  -> (TokenCloseList, s)
			?Just ','  -> (TokenComma, s)
			?Just '\'' -> lex_char 0 [] s
			?Just '"'  -> lex_string 0 [] s
			?Just '_'  -> lex_ident 1 ['_'] s
			?Just '`'  -> lex_ident 1 ['`'] s
			?Just '?'  -> lex_maybe 1 ['?'] s
			?Just '+'
				# (mc, s) = lexGetChar s
				-> case mc of
					?None -> (TokenIdent "+", s)
					?Just c
						| isDigit c
							-> lex_number +1 (lexUngetChar c s)
						| otherwise
							-> lex_ident 1 ['+'] (lexUngetChar c s)
			?Just '-'
				# (mc, s) = lexGetChar s
				-> case mc of
					?None -> (TokenIdent "-", s)
					?Just c
						| isDigit c
							-> lex_number -1 (lexUngetChar c s)
						| otherwise
							-> lex_funny_ident 1 ['-'] (lexUngetChar c s) // PK
	//						-> lex_ident 1 ['-'] (lexUngetChar c s)
			?Just c
				| isSpace c
					-> lex s 
				| isDigit c
					-> lex_number +1 (lexUngetChar c s)
				| isAlpha c
					-> lex_ident 1 [c] s
				| isSpecialChar c
					-> lex_funny_ident 1 [c] s
				| otherwise
					-> (TokenError ("Unknown character " +++ toString c), s)

	lex_maybe i cs s
		# (mc, s) = lexGetChar s
		= case mc of
			?Just c -> case c of
				'^' -> lex_maybe (inc i) ['^':cs] s
				'#' -> lex_maybe (inc i) ['#':cs] s
				c  -> case lex_ident 0 [] (lexUngetChar c s) of
					(TokenIdent "Just", s) = (TokenIdent (mk_str i cs +++ "Just"), s)
					(TokenIdent "None", s) = (TokenIdent (mk_str i cs +++ "None"), s)
					(o, s) = (TokenIdent (mk_str i cs), lexUngetToken o s)
			?None -> (TokenIdent "?", s)

	lex_digits s 
		= lex_digits_acc 0 [] s	
	lex_digits_acc num acc s
		# (mc, s) = lexGetChar s
		= case mc of
			?None
				-> (num, acc, s)
			?Just c
				| isDigit c
					-> lex_digits_acc (inc num) [digitToInt c:acc] s
				| otherwise 
					-> (num, acc, lexUngetChar c s)									

	digits_to_int :: [Int] -> Int
	digits_to_int [] = 0
	digits_to_int [digit:digits] = digit + 10 * digits_to_int digits 

	digits_to_real :: [Int] -> Real 
	digits_to_real [] = 0.0
	digits_to_real [digit:digits] = toReal digit + 10.0 * digits_to_real digits

	lex_number sign s
		#! (num_digits, digits, s) = lex_digits s
		#! (mc, s) = lexGetChar s
		= case mc of
			?None -> (TokenInt (sign * digits_to_int digits), s)
			?Just '.'
				-> lex_real_with_fraction (toReal sign) (digits_to_real digits) s
			?Just 'E'
				#! real = toReal sign * digits_to_real digits
				-> lex_real_with_exp real s
			?Just 'e'
				#! real = toReal sign * digits_to_real digits
				-> lex_real_with_exp real s
			?Just c
				-> (TokenInt (sign * digits_to_int digits), lexUngetChar c s)

	lex_real_with_fraction sign real s
		#! (num_digits, digits, s) = lex_digits s
		#! fraction = digits_to_real digits  / 10.0^ toReal num_digits	
		#! real = sign * (real + fraction)	
		#! (mc, s) = lexGetChar s
		= case mc of
			?None -> (TokenReal real, s)
			?Just 'E'
				-> lex_real_with_exp real s
			?Just 'e'
				-> lex_real_with_exp real s
			?Just c
				-> (TokenReal real, lexUngetChar c s)

	lex_real_with_exp real s
		# (mc, s) = lexGetChar s
		= case mc of
			?None -> (TokenReal real, s)
			?Just '+'
				#! (num_digits, digits, s) = lex_digits s
				-> (TokenReal (real * 10.0 ^ digits_to_real digits), s)  
			?Just '-'
				#! (num_digits, digits, s) = lex_digits s
				-> (TokenReal (real * 10.0 ^ (-1.0 * digits_to_real digits)), s)  
			?Just c
				| isDigit c
					#! (num_digits, digits, s) = lex_digits (lexUngetChar c s)
					-> (TokenReal (real * 10.0 ^ digits_to_real digits), s)  
				| otherwise	
					-> (TokenError "error in real constant", s)
						
	lex_ident num_chars acc_chars s
		# (mc, s) = lexGetChar s
		= case mc of
			?None -> (mktoken num_chars acc_chars, s)
			?Just '_' -> lex_ident (inc num_chars) ['_':acc_chars] s
			?Just '`' -> lex_ident (inc num_chars) ['`':acc_chars] s
			?Just c
				| isAlphanum c
					-> lex_ident (inc num_chars) [c:acc_chars] s
				| otherwise  
					-> (mktoken num_chars acc_chars, lexUngetChar c s)									
	where
		mktoken num_chars acc_chars
			= case mk_str num_chars acc_chars of
				"True"  -> TokenBool True
				"False" -> TokenBool False
				str		-> TokenIdent str			

	lex_funny_ident num_chars acc_chars s
		# (mc, s) = lexGetChar s
		= case mc of
			?None -> (TokenIdent (mk_str num_chars acc_chars), s)
			?Just c
				| isSpecialChar c
					-> lex_funny_ident (inc num_chars) [c:acc_chars] s
				| otherwise		
					-> (TokenIdent (mk_str num_chars acc_chars), lexUngetChar c s)									

	lex_string num_chars acc_chars s
		# (mc, s) = lexGetChar s
		= case mc of
			?None -> (TokenError "error in string constant", s)
			?Just '"' -> (TokenString (mk_str num_chars acc_chars), s)
			?Just '\\'
				#! (mc, s) = lex_special_char s
				-> case mc of
					?None -> (TokenError "error in string constant", s)
					?Just c -> lex_string (inc num_chars) [c:acc_chars] s
			?Just c -> lex_string (inc num_chars) [c:acc_chars] s


	lex_char num_chars acc_chars s
		# (mc, s) = lexGetChar s
		= case mc of
			?None -> (TokenError "error in char constant", s)
			?Just '\''
				| num_chars == 1
					-> (TokenChar (hd acc_chars), s)
				| num_chars == 0
					-> (TokenError "char constant contains no characters ", s)
				| otherwise 	
					-> (TokenError "char constant contains more than one character", s)
			?Just '\\'
				#! (mc, s) = lex_special_char s
				-> case mc of
					?None -> (TokenError "error in char constant", s)
					?Just c -> lex_char (inc num_chars) [c:acc_chars] s
			?Just c -> lex_char (inc num_chars) [c:acc_chars] s

	lex_special_char s 
		#! (mc, s) = lexGetChar s
		= case mc of
			?Just 'n' -> (?Just '\n', s)
			?Just 'r' -> (?Just '\r', s)
			?Just 'f' -> (?Just '\f', s)
			?Just 'b' -> (?Just '\b', s)
			?Just 't' -> (?Just '\t', s)
			?Just '\\' -> (?Just '\\', s)
			?Just '\'' -> (?Just '\'', s)
			?Just '\"' -> (?Just '\"', s)
			?Just '\0' -> (?Just '\0', s)
			//?Just '\x' -> abort "lex: hex char not implemented\n"
			//?Just '\0' -> abort "lex: oct char not implemented\n"
			_ -> (mc, s)

	mk_str num_chars acc_chars
		# str = createArray num_chars ' '
		= fill (dec num_chars) acc_chars str
	where	
		fill i [] str 		= str
		fill i [x:xs] str 	= fill (dec i) xs {str & [i] = x}
	

//----------------------------------------------------------------------------------		
// preparse input


:: ParseEnv = PETop | PETuple | PEPar | PERecord | PEList

preParse :: (ParseState s) -> (Expr, ParseState s) | ParseInput s
preParse s 
	= parse_expr PETop s
where
	parse_expr env s
		= parse_app env [] s
	
	parse_app env exprs s
		#! (token, s) = lexGetToken s
		= parse token env exprs s
	where
		parse TokenComma PETuple exprs	s 	= (mkexpr exprs, lexUngetToken TokenComma s)
		parse TokenComma PERecord exprs	s 	= (mkexpr exprs, lexUngetToken TokenComma s)
		parse TokenComma PEList exprs	s 	= (mkexpr exprs, lexUngetToken TokenComma s)
		parse TokenComma PETop exprs s 		= (ExprError "end of input expected instead of ,", s)
		parse TokenComma PEPar exprs s 		= (ExprError ") expected instead of ,", s)
		parse TokenComma env exprs s 		= abort "unknown env\n"

		parse TokenClosePar PETuple	exprs s	= (mkexpr exprs, lexUngetToken TokenClosePar s)
		parse TokenClosePar PERecord exprs s = (ExprError "} expected instead of )", s)
		parse TokenClosePar PEList exprs s  = (ExprError "] expected instead of )", s)
		parse TokenClosePar PETop	exprs s	= (ExprError "end of input expected instead of )", s)
		parse TokenClosePar PEPar	exprs s = (mkexpr exprs, lexUngetToken TokenClosePar s)
		parse TokenClosePar env exprs s 	= abort "unknown env\n"

		parse TokenCloseCurly PETuple	exprs s	= (ExprError ") expected instead of }", s)
		parse TokenCloseCurly PEList	exprs s	= (ExprError "] expected instead of }", s)
		parse TokenCloseCurly PERecord exprs s = (mkexpr exprs, lexUngetToken TokenCloseCurly s)
		parse TokenCloseCurly PETop	exprs s	= (ExprError "end of input expected instead of )", s)
		parse TokenCloseCurly PEPar	exprs s = (mkexpr exprs, lexUngetToken TokenCloseCurly s)
		parse TokenCloseCurly env exprs s 	= abort "unknown env\n"

		parse TokenCloseList PETuple exprs s	= (ExprError ") expected instead of ]", s)
		parse TokenCloseList PERecord exprs s = (ExprError "} expected instead of ]", s)
		parse TokenCloseList PEList exprs s = (mkexpr exprs, lexUngetToken TokenCloseList s)
		parse TokenCloseList PETop	exprs s	= (ExprError "end of input expected instead of )", s)
		parse TokenCloseList PEPar	exprs s = (mkexpr exprs, lexUngetToken TokenCloseList s)
		parse TokenCloseList env exprs s 	= abort "unknown env\n"

		parse TokenEnd PETuple exprs s		= (ExprError ") or, expected instead of end of input", s)
		parse TokenEnd PERecord exprs s		= (ExprError "} or, expected instead of end of input", s)
		parse TokenEnd PEList exprs s		= (ExprError "] or, expected instead of end of input", s)
		parse TokenEnd PETop exprs s		= (mkexpr exprs, lexUngetToken TokenEnd s)
		parse TokenEnd PEPar exprs s		= (ExprError ") expected instead of end of input",s)
		parse TokenEnd env exprs s 			= abort "unknown env\n"
	
		parse (TokenInt x) env exprs s 		= parse_app env [ExprInt x:exprs] s
		parse (TokenBool x) env exprs s 	= parse_app env [ExprBool x:exprs] s
		parse (TokenReal x) env exprs s 	= parse_app env [ExprReal x:exprs] s
		parse (TokenChar x) env exprs s 	= parse_app env [ExprChar x:exprs] s
		parse (TokenString x) env exprs s 	= parse_app env [ExprString x:exprs] s
		parse (TokenIdent x) env exprs s 	= parse_app env [ExprIdent x:exprs] s
		parse TokenOpenPar env exprs s 	
			# (token, s) = lexGetToken s
			= case token of
				TokenClosePar = (ExprUnitBuiltin, s)
				t
					# (expr, s)	= parse_par_expr (lexUngetToken t s)
					= case expr of
						ExprError err 	-> (ExprError err, s)
						_				->  parse_app env [expr:exprs] s
		parse TokenOpenCurly env exprs s
			# (expr, s) = parse_record_or_array s
			= case expr of
				ExprError err 	-> (ExprError err, s)
				_				->  parse_app env [expr:exprs] s
		parse TokenOpenList env exprs s	
			# (expr, s) = parse_list s
			= case expr of
				ExprError err 	-> (ExprError err, s)
				_				->  parse_app env [expr:exprs] s
		parse (TokenError err) env exprs s 
			= (ExprError ("lex error in parse_app: "  +++ err), s) 		
				
		parse token env exprs s 
			= abort ("parse app - unknown token " +++ toString token)
		
		
		mkexpr []		= ExprError "expression expected"
		mkexpr [expr] 	= expr
		mkexpr exprs 	= ExprApp {e\\e <- reverse exprs}

	parse_par_expr s
		#! (expr, s) = parse_expr PETuple s
		= case expr of
			ExprError err -> (ExprError err, s)
			_
				#! (token, s) = lexGetToken s
				-> case token of
					TokenClosePar -> (expr, s)
					TokenComma -> parse_tuple [expr] (lexUngetToken token s)
					_	-> (ExprError (", or ) expected, found " +++ toString token), s)
							 				
	parse_tuple exprs s 
		#! (token, s) = lexGetToken s
		= case token of
			TokenComma 
				#! (expr, s) = parse_expr PETuple s
				-> case expr of
					ExprError err -> (ExprError err, s)
					_	-> parse_tuple [expr:exprs] s
			TokenClosePar 
				-> (ExprTuple {e\\e<-reverse exprs}, s)
			_ 	
				-> (ExprError "parse tuple: , or ) expected", s) 		

	parse_list s
		#! (token, s) = lexGetToken s
		= case token of
			TokenCloseList 
				-> (ExprList [], s)
			_
				#! (expr, s) = parse_expr PEList (lexUngetToken token s)
				-> case expr of
					ExprError err -> (ExprError (err +++ " ; parse list"), s)
					_ -> parse_rest [expr] s
	where
		parse_rest exprs s		
			#! (token, s) = lexGetToken s
			= case token of
				TokenComma 
					#! (expr, s) = parse_expr PEList s
					-> case expr of
						ExprError err -> (ExprError err, s)
						_	-> parse_rest [expr:exprs] s
				TokenCloseList 
					-> (ExprList (reverse exprs), s)
				_ 	
					-> (ExprError "parse list: , or ] expected", s) 		

		
	parse_record_or_array s 
		#! (token, s) = lexGetToken s
		= case token of
			TokenCloseCurly 
				-> (ExprArray [], s)
			TokenIdent name
				#! (token, s) = lexGetToken s
				-> case token of
					TokenIdent "="
						#! (expr, s) = parse_expr PERecord s
						-> parse_record ?None [ExprField name expr] s
					TokenIdent "|"
						-> parse_record (?Just name) [] (lexUngetToken TokenComma s)
					_
						#! (expr, s) = parse_expr PERecord 
							(lexUngetToken (TokenIdent name) (lexUngetToken token s))
						-> parse_array [expr] s
			_	
				#! (expr, s) = parse_expr PERecord (lexUngetToken token s)
				-> parse_array [expr] s
	where
		parse_record rec_name fields s
			#! (token, s) = lexGetToken s
			= case token of
				TokenCloseCurly 
					-> (ExprRecord rec_name {e\\e<- reverse fields}, s)
				TokenComma
					#! (token, s) = lexGetToken	s
					-> case token of
						TokenIdent field_name
							#! (token, s) = lexGetToken	s
							-> case token of
								TokenIdent "=" 
									#! (expr, s) = parse_expr PERecord s
									-> parse_record rec_name [ExprField field_name expr:fields] s
								_ -> (ExprError ("parse record failed on token " +++ toString token), s)
						_ -> (ExprError ("parse record failed on token " +++ toString token), s)
				_ -> (ExprError ("parse record failed on token " +++ toString token), s)

		parse_array exprs s
			#! (token, s) = lexGetToken s
			= case token of
				TokenCloseCurly 
					-> (ExprArray (reverse exprs), s)
				TokenComma
					#! (expr, s) = parse_expr PERecord s
					-> parse_array [expr:exprs] s
				_ -> (ExprError ("parse array failed on token " +++ toString token), s)


//----------------------------------------------------------------------------------

generic gParse a :: !Expr -> ?a

gParse{|Int|} (ExprInt x) = ?Just x
gParse{|Int|} _           = ?None

gParse{|Char|} (ExprChar x) = ?Just x
gParse{|Char|} _            = ?None

gParse{|Bool|} (ExprBool x) = ?Just x
gParse{|Bool|} _            = ?None

gParse{|Real|} (ExprReal x) = ?Just x
gParse{|Real|} _            = ?None

gParse{|String|} (ExprString x) = ?Just x
gParse{|String|} _              = ?None

gParse{|UNIT|} ExprUnit = ?Just UNIT
gParse{|UNIT|} _        = ?None

gParse{|PAIR|} fx fy (ExprPair ex ey)
	= case fx ex of
		?Just x -> case fy ey of
			?Just y -> ?Just (PAIR x y)
			?None   -> ?None
		?None -> ?None
gParse{|PAIR|} fx fy _ = ?None

gParse{|EITHER|} fl fr expr
	= case fl expr of
		?None -> case fr expr of
			?None   -> ?None
			?Just x -> ?Just (RIGHT x)
		?Just x -> ?Just (LEFT x)
		
gParse{|CONS of d|} parse_arg expr
	| d.gcd_arity == 0 	
		= parse_nullary expr
	| is_tuple d.gcd_name
		= parse_tuple expr	
	| otherwise
		= case d.gcd_prio of
			GenConsNoPrio			
				-> parse_nonfix expr
			GenConsPrio assoc prio 	
				-> parse_infix assoc prio expr 						
where
	parse_nullary (ExprIdent name)
		# name = case name of
			"?None" = "_!None"
			"?#None" = "_#None"
			"?^None" = "_None"
			x = x
		| name == d.gcd_name
			= mapMaybe CONS (parse_arg ExprUnit)
		| otherwise
			= ?None
	parse_nullary _
		= ?None

	parse_nonfix (ExprApp exprs)
		= parse_nonfix1 exprs
	parse_nonfix (ExprAppInInfix exprs _ _ _)
		= parse_nonfix1 exprs
	parse_nonfix _
		= ?None

	parse_nonfix1 exprs
		#! size_exprs = size exprs
		| size_exprs == d.gcd_arity + 1 && is_ident d.gcd_name exprs.[0]
			#! arg_exprs = [exprs.[i] \\ i <- [1 .. size_exprs - 1]]
			= mapMaybe CONS (parse_arg (mkprod arg_exprs))
		| otherwise
			= ?None
	
	parse_tuple (ExprTuple exprs) 
		= mapMaybe CONS (parse_arg (mkprod [e\\e<-:exprs]))
	parse_tuple expr = ?None
	
	parse_infix this_assoc this_prio (ExprApp exprs)
		= parse_infix1 this_assoc this_prio exprs
	parse_infix this_assoc this_prio (ExprAppInInfix exprs outer_assoc outer_prio branch)
		| this_prio > outer_prio
			= parse_infix1 this_assoc this_prio exprs
		| this_prio < outer_prio
			= ?None
		| otherwise
			= case (this_assoc, outer_assoc, branch) of
				(GenConsAssocLeft, GenConsAssocLeft, GenConsAssocLeft)
					-> parse_infix1 this_assoc this_prio exprs
				(GenConsAssocRight, GenConsAssocRight, GenConsAssocRight)
					-> parse_infix1 this_assoc this_prio exprs
				_ -> ?None
	parse_infix this_assoc this_prio expr
		= ?None
		
	parse_infix1 this_assoc this_prio exprs
		#! size_exprs = size exprs
		| size_exprs < 3 = ?None
		= case (case this_assoc of GenConsAssocLeft -> find_last; _ -> find_first) exprs of
			?None -> ?None
			?Just op_index
				#! left_arg  = mkarg GenConsAssocLeft {exprs.[i] \\ i <- [0 .. op_index - 1]}
				#! right_arg = mkarg GenConsAssocRight {exprs.[i] \\ i <- [op_index + 1 .. size_exprs - 1]}
				-> mapMaybe CONS (parse_arg (ExprPair left_arg right_arg))
	where
		mkarg branch exprs
			= case size exprs of
				0 -> abort "mkarg\n"
				1 -> exprs.[0]
				_ -> ExprAppInInfix exprs this_assoc this_prio branch
	
	find_last exprs 
		= find (size exprs - 2) exprs
	where
		find i exprs
			| i < 1
				= ?None
			| otherwise	
				= case exprs.[i] of
					ExprIdent s | s == d.gcd_name 	-> ?Just i
				 	_ 								-> find (dec i) exprs	
	find_first exprs
		= find 1 exprs
	where
		find i exprs
			| i >= size exprs - 1
				= ?None
			| otherwise	
				= case exprs.[i] of
					ExprIdent s | s == d.gcd_name 	-> ?Just i
				 	_ 								-> find (inc i) exprs	

	is_tuple name 
		#! size_name = size name
		= (size_name == 7 || size_name == 8)
		&& name.[0] == '_'
		&& name.[1] == 'T'
		&& name.[2] == 'u'
		&& name.[3] == 'p'
		&& name.[4] == 'l'
		&& name.[5] == 'e'
		&& isDigit name.[6]
		&& (size_name == 7 || isDigit name.[7])

is_ident wanted_name (ExprIdent name)
	# wanted_name = case wanted_name of
		"_!Just" = "?Just"
		"_#Just" = "?#Just"
		"_Just" = "?^Just"
		x = x
	= name == wanted_name
is_ident _ _ = False		

gParse{|RECORD of {grd_name}|} parse_arg (ExprRecord rec_name exprs)
	| check_name rec_name grd_name
		= mapMaybe RECORD (parse_arg (mkprod [e\\e<-:exprs]))
		= ?None
	where
		check_name ?None cons_name = True
		check_name (?Just rec_name) cons_name = rec_name == cons_name
gParse{|RECORD of {grd_name}|} parse_arg expr
	= ?None

mkprod [] 		= abort "mkprod\n"
mkprod [expr] 	= expr
mkprod exprs 	
	# (xs, ys) = splitAt (length exprs / 2) exprs
	= ExprPair (mkprod xs) (mkprod ys)
	
gParse{|FIELD of {gfd_name}|} parse_arg (ExprField name value) 
	| gfd_name == name
		= mapMaybe (\x -> FIELD x) (parse_arg value)
		= ?None
gParse{|FIELD of {gfd_name}|} _ _ = ?None
gParse{|OBJECT of {gtd_num_conses,gtd_conses}|} parse_arg expr
	| gtd_num_conses == 0 = case expr of
		ExprApp ap
			| size ap == 2 && is_ident (hd gtd_conses).gcd_name ap.[0]
				= mapMaybe (\x -> OBJECT x) (parse_arg ap.[1])
			= ?None
		_ = ?None
	= mapMaybe (\x -> OBJECT x) (parse_arg expr)

gParse{|[]|} parse_arg (ExprList exprs) 
	= maybeAll [parse_arg e \\e<-exprs]
gParse{|[]|} parse_arg _ = ?None

gParse{|{}|} parse_arg (ExprArray exprs)
	= mapMaybe (\xs -> {x\\x<-xs}) (maybeAll (map parse_arg exprs)) 
gParse{|{}|} parse_arg _ = ?None
		
gParse{|{!}|} parse_arg (ExprArray exprs)
	= mapMaybe (\xs -> {x\\x<-xs}) (maybeAll (map parse_arg exprs))
gParse{|{!}|} parse_arg _ = ?None

gParse{|()|} ExprUnitBuiltin = ?Just ()
gParse{|()|} _ = ?None

maybeAll [|] = ?Just [|]
maybeAll [| ?None:_] = ?None
maybeAll [| ?Just x:mxs]
	= case maybeAll mxs of
		?None    -> ?None
		?Just xs -> ?Just [|x:xs]

//----------------------------------------------------------------------------------		

preParseInput :: !s -> Expr | ParseInput s
preParseInput input
	# (expr, s) = preParse {ps_input=input, ps_char = ?None, ps_tokens = [] }
	= expr

preParseString :: !String -> Expr
preParseString str = preParseInput {si_pos = 0, si_str = str}

preParseFile :: !File -> Expr
preParseFile file = preParseInput file

parseString :: !String -> ?a | gParse{|*|} a
parseString str = gParse{|*|} (preParseString str)

parseFile :: !File -> ?a | gParse{|*|} a
parseFile file = gParse{|*|} (preParseFile file)
