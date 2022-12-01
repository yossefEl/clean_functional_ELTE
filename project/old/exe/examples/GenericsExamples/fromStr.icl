module fromStr

import StdGeneric, StdEnv, StdMaybe

generic gFromString a :: String Int -> (Maybe a, Int)
gFromString{|Int|} str pos = lexInt str pos
gFromString{|Bool|} str pos = lexBool str pos
gFromString{|UNIT|} str pos = (Just UNIT, pos)
gFromString{|PAIR|} fx fy str pos 
	= case fx str pos of 
		(Just x, posx) -> case fy str posx of
			(Just y, posy) 		-> (Just (PAIR x y), posy)
			(Nothing, _) 		-> (Nothing, pos)  
		(Nothing, _) 			-> (Nothing, pos)
			
gFromString{|EITHER|} fl fr str pos
	# (l, posl) = fl str pos
	= case l of
		Just x 					-> (Just (LEFT x), posl)
		Nothing 
			# (r, posr) = fr str pos
			-> case r of
				Just x 			-> (Just (RIGHT x), posr)
				Nothing 		-> (Nothing, pos)

gFromString{|CONS of {gcd_name, gcd_arity}|} fx str pos
	| gcd_arity == 0
		# (ok, next_pos) = lex_ident gcd_name str pos
		| not ok
			= (Nothing, pos)
		# (x, next_pos) = fx str next_pos 
		= case x of
			Just args -> (Just (CONS args), next_pos)
			Nothing -> (Nothing, pos)
	| otherwise // gcd_arity <> 0
		# (ok, next_pos) = lexWantChar '(' str pos
		| not ok
			= (Nothing, pos)
		# (ok, next_pos) = lex_ident gcd_name str next_pos
		| not ok
			= (Nothing, pos)
		# (x, next_pos) = fx str next_pos 
		= case x of
			Just args
				# (ok, next_pos) = lexWantChar ')' str next_pos
				| not ok
					-> (Nothing, pos)
					-> (Just (CONS args), next_pos)	
			Nothing -> (Nothing, pos)
where
	lex_ident name str pos	
		= case lexIdent str pos of
			(Nothing, _) -> (False, pos)
			(Just ident, next_pos)
				| ident == name
					-> (True, next_pos)
					-> (False, pos)
gFromString{|OBJECT|} fx str pos
	# (mx, st) = fx str pos
	= (case mx of Just x -> Just (OBJECT x); Nothing -> Nothing, st)
	
gFromString{|RECORD|} fx str pos
	= abort "gFromString for records is not implemented" 

gFromString{|FIELD of {gfd_name}|} fx str pos
	= abort "gFromString for fields is not implemented" 
					
gFromString{|[]|} f str start_pos
	# (ok, pos) = lexWantChar '[' str start_pos
	| ok 
		# (ok, pos) = lexWantChar ']' str pos
		| ok 
			= (Just [], pos)
			= want_list f str pos	  
		= (Nothing, start_pos)
where
	want_list f str start_pos	
		# (x, pos) = f str start_pos
		= case x of
			Nothing -> (Nothing, start_pos)
			Just x
				# (ok, pos) = lexWantChar ']' str pos
				| ok 
					-> (Just [x], pos)
				# (ok, pos) = lexWantChar ',' str pos
				| ok 
					# (xs, pos) = want_list f str pos
					->  case xs of
						Nothing -> (Nothing, start_pos)
						Just xs -> (Just [x:xs], pos)
					-> (Nothing, start_pos)
gFromString{|(,)|} f1 f2 str start_pos	
	# (ok1, pos) = lexWantChar '(' str start_pos	
	# (x1, pos) = f1 str pos	
	# (ok2, pos) = lexWantChar ',' str pos
	# (x2, pos) = f2 str pos
	# (ok3, pos) = lexWantChar ')' str pos
	| ok1 && ok2 && ok3 
		= case (x1, x2) of
			(Just x1, Just x2) -> (Just (x1, x2), pos)
			_ -> (Nothing, start_pos)
		= (Nothing, start_pos)

fromStr :: String -> (Maybe a, String) | gFromString{|*|} a
fromStr str 
	# (x, pos) = gFromString{|*|} str 0
	= (x, {str.[i] \\ i <- [pos .. size str - 1]} )

//---------------------------------------------------------------------------------------

lexInt :: String Int -> (Maybe Int, Int)
lexInt str pos
	# pos = lexSkipSpaces str pos
	= lex_sign_and_digits pos
where
	size_str = size str
	lex_sign ::	Int -> (Int, Int)
	lex_sign pos 
		| pos < size_str 		
			= case str.[pos] of
				'+'	-> (+1, pos + 1)
				'-'	-> (-1, pos + 1)
				_	-> (+1, pos)
			= (+1, pos)			

	lex_base x pos 
		| x <> 0 
			= (10, pos)			
		| pos < size_str 		
			= case str.[pos] of
				'x'	-> (16, pos + 1)
				_	-> (8, pos)
			= (10, pos)			

	lex_digit :: Int -> (Bool, Int, Int)
	lex_digit pos
		| pos < size_str && isDigit str.[pos]
			= (True, digitToInt str.[pos], pos + 1)		
			= (False, 0, pos)
	lex_digits :: Int Int Int -> (Int, Int)
	lex_digits base num pos 
		# (ok, x, pos) = lex_digit pos
		| ok
			= lex_digits base (base * num + x) pos
			= (num, pos)

	lex_sign_and_digits :: Int -> (Maybe Int, Int)
	lex_sign_and_digits pos
		| pos >= size_str
			= (Nothing, pos)
		# (sign, pos) = lex_sign pos
		# (ok, x, pos) = lex_digit pos
		| ok
			# (base, pos)= lex_base x pos
			# (n, pos) = lex_digits	base (sign * x) pos
			= (Just n, pos)
			= (Nothing, pos) 

lexIdent :: String Int -> (Maybe String, Int)
lexIdent str start_pos
	# pos = lexSkipSpaces str start_pos
	| pos >= size_str
		= (Nothing, start_pos)
	# ch = str.[pos]	
	| not (isAlpha ch)	
		= (Nothing, start_pos)
	# (xs, pos) = lex_alphas (pos + 1)
	= (Just {x\\x<-[ch:xs]}, pos)
where	
	size_str = size str
	skip_spaces pos
		| pos < size_str && isSpace str.[pos] 	
			= skip_spaces (pos+1)
			= pos 
	lex_alphas pos
		| pos < size_str && isAlphanum str.[pos]
			# (xs, next_pos) = lex_alphas (pos + 1)
			= ([str.[pos] : xs], next_pos)
			= ([], pos)

lexSkipSpaces :: String Int -> Int
lexSkipSpaces str pos  
	| pos < size str && isSpace str.[pos] 	
		= lexSkipSpaces str (pos+1)
		= pos 
	
lexChar :: String Int -> (Maybe Char, Int)
lexChar str pos = undef

lexString :: String Int -> (Maybe String, Int)
lexString str pos = undef

lexReal :: String Int -> (Maybe Real, Int)
lexReal str pos = undef

lexBool :: String Int -> (Maybe Bool, Int)
lexBool str pos
	= case lexIdent str pos of
		(Nothing, _) -> (Nothing, pos)
		(Just x, next_pos) -> case x of
			"True" -> (Just True, next_pos)
			"False" -> (Just False, next_pos)
			_		-> (Nothing, pos)

lexWantChar :: Char String Int -> (Bool, Int)
lexWantChar ch str pos
	# pos = lexSkipSpaces str pos
	| pos < size str && str.[pos] == ch
		= (True, pos + 1)
		= (False, pos)
				

//------------------------------------------------------------------------

:: Color = Red | Green | Blue
:: Tree a b = Tip a | Bin b (Tree a b) (Tree a b)
:: Record = { rec_x :: Int, rec_y :: Int }

derive bimap (,), Maybe
derive gFromString Color, Tree, Record

Start :: (Color, Color, Color, Tree Color Color)
Start = 
	( froms "Red"
	, froms "Green"	
	, froms "Blue"
	, froms "(Bin Red (Tip Green) (Tip Blue))"
	)
where
	froms :: String -> a | gFromString{|*|} a
	froms s = case fromStr s of
		(Just x, str) -> x
		_ -> abort "could not convert\n"		
		