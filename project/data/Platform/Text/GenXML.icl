implementation module Text.GenXML

import StdEnv
import Data.Error, Data.Either, Data.Maybe, Text, Data.GenEq, Data.Func, Data._Array, Data.Functor
from Control.Applicative import class <*> (..)
import Text.GenParse
from Text.Parsers.CParsers.ParserCombinators import :: Parser, :: ParsResult, :: CParser, &>, +&+, +&-, -&+, <!>, <&, <&>, <*?>, <@, >?<, @>, begin1, satisfy, symbol, yield, <|>, <+?>, fail

instance == XMLDoc where
	(==) x y = x === y

uname :: !String -> XMLQName
uname name = XMLQName ?None name

qname :: !XMLNamespacePrefix !String -> XMLQName
qname namespace name = XMLQName (?Just namespace) name

instance toString XMLDoc
where
	toString (XMLDoc defaultNamespace namespaces documentElement)
		# documentElement = addNamespaces defaultNamespace namespaces documentElement
		# doc             = XMLDoc defaultNamespace namespaces documentElement
		# docsize = docSize doc
		# docstring = unsafeCreateArray docsize
		# (docstring,_) = serializeDoc doc docstring 0
		= docstring
	where
		addNamespaces :: !(?XMLURI) [(XMLNamespacePrefix,String)] !XMLNode -> XMLNode
		addNamespaces mbDefaultNamespace namespaces (XMLElem qname attrs children)
			# ns = map (\(prefix,uri) -> XMLAttr (XMLQName (?Just "xmlns") prefix) uri) namespaces
			# ns = case mbDefaultNamespace of
				?None					= ns
				?Just defaultNamespace	= [XMLAttr (XMLQName ?None "xmlns") defaultNamespace:ns]
			= (XMLElem qname (ns ++ attrs) children)
		addNamespaces _ _ _ = abort "addNamespaces called on non-XMLElem\n"

		docSize :: !XMLDoc -> Int
		docSize (XMLDoc defaultNamespace namespaces documentElement)
			= 37 + nodeSize documentElement

		nodeSize :: !XMLNode -> Int
		nodeSize (XMLText text) = escapedSize text
		nodeSize (XMLElem qname attrs children)
			# attrsSize = sum (map attrSize attrs) + length attrs
			= if (isEmpty children)
				(3 + qnameSize qname + attrsSize)
				(5 + 2 * qnameSize qname + attrsSize + sum (map nodeSize children))
		nodeSize (XMLCData data) = 12 + size data

		attrSize :: !XMLAttr -> Int
		attrSize (XMLAttr qname value) = 3 + qnameSize qname + escapedSize value

		qnameSize :: !XMLQName -> Int
		qnameSize (XMLQName ?None      name) = size name
		qnameSize (XMLQName (?Just ns) name) = 1 + size ns + size name

		//Calculates the number of chars in a string when xml special characters are escaped
		escapedSize :: !{#Char} -> Int
		escapedSize s = escapedSize` s (size s) 0
		where
			escapedSize` s n i
				| i == n = 0
				| s.[i] == '<'  = 4 + escapedSize` s n (i + 1)
				| s.[i] == '>'  = 4 + escapedSize` s n (i + 1)
				| s.[i] == '&'  = 5 + escapedSize` s n (i + 1)
				| s.[i] == '\'' = 6 + escapedSize` s n (i + 1)
				| s.[i] == '"'  = 6 + escapedSize` s n (i + 1)
				| otherwise = 1 + escapedSize` s n (i + 1)

		serializeDoc :: !XMLDoc !*{#Char} !Int -> (!*{#Char}, !Int)
		serializeDoc (XMLDoc defaultNamespace namespaces documentElement) dest dest_i
			# (dest,dest_i) = copyChars "<?xml version=\"1.0\" standalone=\"no\"?>" 0 False dest dest_i
			= serializeNode documentElement dest dest_i

		serializeNode :: !XMLNode !*{#Char} !Int -> (!*{#Char}, !Int)
		serializeNode (XMLText text) dest dest_i = copyChars text 0 True dest dest_i
		serializeNode (XMLElem qname attrs []) dest dest_i
			# dest = {dest & [dest_i] = '<'}
			# dest_i = dest_i + 1
			# (dest,dest_i) = serializeQName qname dest dest_i
			# (dest,dest_i) = serializeMap serializeAttr attrs dest dest_i
			# dest = {dest & [dest_i] = '/'}
			# dest_i = dest_i + 1
			# dest = {dest & [dest_i] = '>'}
			= (dest,dest_i + 1)
		serializeNode (XMLElem qname attrs children) dest dest_i
			# dest = {dest & [dest_i] = '<'}
			# dest_i = dest_i + 1
			# (dest,dest_i) = serializeQName qname dest dest_i
			# (dest,dest_i) = serializeMap serializeAttr attrs dest dest_i
			# dest = {dest & [dest_i] = '>'}
			# dest_i = dest_i + 1
			# (dest,dest_i) = serializeMap serializeNode children dest dest_i
			# dest = {dest & [dest_i] = '<'}
			# dest_i = dest_i + 1
			# dest = {dest & [dest_i] = '/'}
			# dest_i = dest_i + 1
			# (dest,dest_i) = serializeQName qname dest dest_i
			# dest = {dest & [dest_i] = '>'}
			= (dest,dest_i + 1)
		serializeNode (XMLCData data) dest dest_i
			# (dest, dest_i) = copyChars "<![CDATA[" 0 False dest dest_i
			# (dest, dest_i) = copyChars data        0 False dest dest_i
			# (dest, dest_i) = copyChars "]]>"       0 False dest dest_i
			= (dest, dest_i)

		serializeMap f [] dest dest_i = (dest, dest_i)
		serializeMap f [x:xs] dest dest_i
			# (dest, dest_i) = f x dest dest_i
			= serializeMap f xs dest dest_i

		serializeAttr :: !XMLAttr !*{#Char} !Int -> (!*{#Char}, !Int)
		serializeAttr (XMLAttr qname value) dest dest_i
			# dest = {dest & [dest_i] = ' '}
			# dest_i = dest_i + 1
			# (dest,dest_i) = serializeQName qname dest dest_i
			# dest = {dest & [dest_i] = '='}
			# dest_i = dest_i + 1
			# dest = {dest & [dest_i] = '"'}
			# dest_i = dest_i + 1
			# (dest,dest_i) = copyChars value 0 True dest dest_i
			# dest = {dest & [dest_i] = '"'}
			# dest_i = dest_i + 1
			= (dest,dest_i)

		serializeQName :: !XMLQName !*{#Char} !Int -> (!*{#Char}, !Int)
		serializeQName (XMLQName ?None      name) dest dest_i = copyChars name 0 False dest dest_i
		serializeQName (XMLQName (?Just ns) name) dest dest_i
			# (dest, dest_i) = copyChars ns 0 False dest dest_i
			# dest = {dest & [dest_i] = ':'}
			# dest_i = dest_i + 1
			= copyChars name 0 False dest dest_i

		copyChars :: !{#Char} !Int !Bool !*{#Char} !Int -> (!*{#Char},!Int)
		copyChars src src_i escape dest dest_i
			| src_i == (size src) = (dest, dest_i)
			| otherwise
				| escape && (src.[src_i] == '<')
					# dest = {dest & [dest_i] = '&', [dest_i + 1] = 'l', [dest_i + 2] = 't', [dest_i + 3] = ';'}
					= copyChars src (src_i + 1) escape dest (dest_i + 4)
				| escape && (src.[src_i] == '>')
					# dest = {dest & [dest_i] = '&', [dest_i + 1] = 'g', [dest_i + 2] = 't', [dest_i + 3] = ';'}
					= copyChars src (src_i + 1) escape dest (dest_i + 4)
				| escape && (src.[src_i] == '&')
					# dest =
						{ dest
						& [dest_i] = '&', [dest_i + 1] = 'a', [dest_i + 2] = 'm', [dest_i + 3] = 'p', [dest_i + 4] = ';'
						}
					= copyChars src (src_i + 1) escape dest (dest_i + 5)
				| escape && (src.[src_i] == '"')
					# dest =
						{ dest
						& [dest_i] = '&', [dest_i + 1] = 'q', [dest_i + 2] = 'u', [dest_i + 3] = 'o'
						, [dest_i + 4] = 't', [dest_i + 5] = ';'
						}
					= copyChars src (src_i + 1) escape dest (dest_i + 6)
				| escape && (src.[src_i] == '\'')
					# dest =
						{ dest
						& [dest_i] = '&', [dest_i + 1] = 'a', [dest_i + 2] = 'p', [dest_i + 3] = 'o'
						, [dest_i + 4] = 's', [dest_i + 5] = ';'
						}
					= copyChars src (src_i + 1) escape dest (dest_i + 6)
				| otherwise
					# dest = {dest & [dest_i] = src.[src_i]}
					= copyChars src (src_i + 1) escape dest (dest_i + 1)

instance fromString (MaybeErrorString XMLDoc)
where
	fromString xmlStr
		# tokens = lex xmlStr
		| isError tokens = liftError tokens
		# xmlDoc = pXMLDoc (fromOk tokens)
		| isEmpty xmlDoc = Error "parse error"
		= Ok (snd (hd xmlDoc))

//Token type which is the intermediary representation during XML parsing
:: Token	= TokenCharData !String
			| TokenName !String
			| TokenStartTagOpen
			| TokenTagClose
			| TokenEmptyTagClose
			| TokenEndTagOpen
			| TokenDeclarationStart
			| TokenDeclarationEnd
			| TokenEqual
			| TokenCData !String

instance == Token
where
	(==) a b = a === b

isName (TokenName _)			= True
isName _						= False

isCharData (TokenCharData _)	= True
isCharData _					= False


:: LexFunctionResult = Token !Int !Token | NoToken !Int | Fail !String
:: LexFunction :== Int -> ?LexFunctionResult

lex :: !String -> MaybeErrorString [Token]
lex input = lex` 0 []
where
	lex` :: !Int ![Token] -> MaybeErrorString [Token]
	lex` offset tokens
		| offset >= inputSize                      = Ok (reverse tokens) //Done
		| dataMode tokens && isJust charDataResult = processResult (fromJust charDataResult)
		| otherwise                                = processResult (lexOneToken offset)
	where
		processResult :: !LexFunctionResult -> MaybeErrorString [Token]
		processResult r = case r of
			Token offset token		= lex` (inc offset) [token:tokens] //Lex another token and do recursive call
			NoToken offset			= lex` (inc offset) tokens
			Fail err				= Error err

		charDataResult = lexCharData '<' False input offset

	lexOneToken :: !Int -> LexFunctionResult
	lexOneToken idx
		// This must only be called when we are sure that `idx < inputSize`
		# c = input.[idx]
		| c == '<' && idx+1 < inputSize
			# c2 = input.[idx+1]
			| isAlpha c2 // this case is not necessary, but it's a quick check for a common case
				= Token idx TokenStartTagOpen
			| c2 == '/'
				= Token (idx+1) TokenEndTagOpen
			| c2 == '!'
				= try $ lexCData idx
			| c2 == '?' && idx+4 < inputSize && input.[idx+2] == 'x' && input.[idx+3] == 'm' && input.[idx+4] == 'l'
				= lexDeclarationStart idx
				= Token idx TokenStartTagOpen
		| c == '>'
			= Token idx TokenTagClose
		| isWhitespace c
			= lexWhitespace idx
		| isNameStartChar c
			= lexName idx
		| c == '='
			= Token idx TokenEqual
		| c == '"'
			= try $ lexAttrValue idx
		| c == '/' && idx+1 < inputSize && input.[idx+1] == '>'
			= Token (idx+1) TokenEmptyTagClose
		| c == '?' && idx+1 < inputSize && input.[idx+1] == '>'
			= Token (idx+1) TokenDeclarationEnd
			= try ?None
	where
		try (?Just result) = result
		try ?None = Fail (concat5 "lexing failed at index " (toString idx) " (\"" (input % (idx,idx+4)) "\")")

	dataMode :: ![Token] -> Bool
	dataMode [TokenTagClose :_]      = True
	dataMode [TokenEmptyTagClose :_] = True
	dataMode [TokenCData _: _]       = True
	dataMode _                       = False

	//Try any of the lexers in the list until one succeeds
	lexAny :: !Int ![!LexFunction!] -> LexFunctionResult
	lexAny offset [!!] = Fail ("invalid input character: '" +++ toString input.[offset] +++ "'")
	lexAny offset [!f:fs!] = case f offset of
		?Just result = result
		?None        = lexAny offset fs

	lexDeclarationStart :: !Int -> LexFunctionResult
	lexDeclarationStart 0 = Token 4 TokenDeclarationStart
	lexDeclarationStart _ = Fail "XML declaration not at start of entity"

	lexCData :: !Int -> ?LexFunctionResult
	lexCData offset
		| input.[offset] == '<' && input.[offset+1] == '!' && input.[offset+2] == '[' &&
				input.[offset+3] == 'C' && input.[offset+4] == 'D' && input.[offset+5] == 'A' &&
				input.[offset+6] == 'T' && input.[offset+7] == 'A' && input.[offset+8] == '['
			= ?Just $
				maybe
					(Fail "CDATA start without end")
					(\endIdx -> Token (endIdx + 3) $ TokenCData $ input % (offset + 9, endIdx))
					(dataEndIndex $ inc offset)
		| otherwise
			= ?None
	where
		dataEndIndex :: !Int -> ?Int
		dataEndIndex curIndex
			| curIndex >= size input - 3                    = ?None
			| input.[curIndex+1] == ']' && input.[curIndex+2] == ']' && input.[curIndex+3] == '>'
			                                                = ?Just curIndex
			| otherwise                                     = dataEndIndex $ inc curIndex

	//Char data
	lexCharData :: !Char !Bool !String !Int -> ?LexFunctionResult
	lexCharData endChar endCharBelongsToToken input offset =
		case lexCharData` offset [] of
			Error e               = ?Just $ Fail e
			Ok ([], _)            = ?None
			Ok (dataStrings, end) = ?Just $ Token end (TokenCharData $ trim $ concat dataStrings)
	where
		lexCharData` :: !Int ![String] -> MaybeErrorString (![String], !Int)
		lexCharData` offset accum
			| offset >= size input = Ok (reverse accum, offset)
			| input.[offset] == '&'
				# end = idxOfNextNonMatchingChar input ((<>) ';') (offset + 1)
				| input.[end] <> ';' = Error $ "Missing ';' at end of character entity"
				# name = input % (offset + 1, end - 1)
				= maybe
					(Error $ concat ["Unknown character entity reference '", name, "'"])
					(\charString -> lexCharData` (end + 1) [charString: accum])
					(entityCharacter name)
			| input.[offset] <> endChar
				# end = idxOfNextNonMatchingChar input isTextChar $ offset + 1
				= lexCharData` end [input % (offset, end - 1): accum]
			| otherwise = Ok (reverse accum, if endCharBelongsToToken offset (offset - 1))
		where
			isTextChar :: !Char -> Bool
			isTextChar c = c <> endChar && c <> '&'

		entityCharacter :: !String -> ?String
		entityCharacter "quot" = ?Just "\""
		entityCharacter "amp"  = ?Just "&"
		entityCharacter "apos" = ?Just "'"
		entityCharacter "lt"   = ?Just "<"
		entityCharacter "gt"   = ?Just ">"
		entityCharacter ref
			| ref.[0] == '#' && ref.[1] == 'x'
				| refSize == 4 = (\d1 d2 -> {shifted d1 + d2})                        <$> d1 <*> d2
				| refSize == 6 = (\d1 d2 d3 d4 -> {shifted d1 + d2, shifted d3 + d4}) <$> d1 <*> d2 <*> d3 <*> d4
				| otherwise    = ?None
		where
			d1      = valueOf ref.[2]
			d2      = valueOf ref.[3]
			d3      = valueOf ref.[4]
			d4      = valueOf ref.[5]
			refSize = size ref

			shifted :: !Char -> Char
			shifted c = toChar (toInt c << 4)

			valueOf :: !Char -> ?Char
			valueOf c
				| not $ isHexDigit c = ?None
				| isDigit c          = ?Just $ c - '0'
				| isLower c          = ?Just $ c - 'a' + '\d10'
				| isUpper c          = ?Just $ c - 'A' + '\d10'
				| otherwise          = ?None
		entityCharacter ref | ref.[0] == '#' = (\i -> {fromInt i}) <$> parseString (ref % (1, size ref - 1))
		entityCharacter _  = ?None

	//Names
	lexName :: !Int -> LexFunctionResult
	lexName offset = Token (end-1) (TokenName (input % (offset, end-1)))
	where
		end = idxOfNextNonMatchingChar input isNameChar $ inc offset

	isNameStartChar :: !Char -> Bool
	isNameStartChar c
		| isAlpha c = True
		| otherwise = c == ':' || c == '_'

	isNameChar :: !Char -> Bool
	isNameChar c
		| isAlphanum c = True
		| otherwise    = c == ':' || c == '_' || c == '-' || c == '.'

	//AttrValue
	lexAttrValue :: !Int -> ?LexFunctionResult
	lexAttrValue offset = lexCharData '"' True input (inc offset)

	lexWhitespace :: !Int -> LexFunctionResult
	lexWhitespace offset = NoToken $ dec $ idxOfNextNonMatchingChar input isWhitespace (offset+1)

	isWhitespace :: !Char -> Bool
	isWhitespace '\x20'	= True
	isWhitespace '\x9'	= True
	isWhitespace '\xD'	= True
	isWhitespace '\xA'	= True
	isWhitespace _		= False

	inputSize = size input

idxOfNextNonMatchingChar input pred start :== run start
where
	run :: !Int -> Int
	run idx
		| idx >= inputSize = idx
		| pred input.[idx] = run (idx+1)
		| otherwise        = idx

	inputSize = size input

pXMLDoc :: Parser Token XMLDoc
pXMLDoc = begin1 pXMLDoc`
where
	pXMLDoc` = mkXMLDoc @> (pDocDeclaration <|> yield [] )-&+ pElem
	
	mkXMLDoc (XMLElem name attributes elements) = XMLDoc mbURI namespaces (XMLElem name attrs elements)
	where
		(mbURI,namespaces,attrs) = filterNamespaces attributes (?None,[],[])
		
		filterNamespaces [] (mbUri, namespaces, attrs) = (mbUri, reverse namespaces, reverse attrs)
		filterNamespaces [attr=:(XMLAttr name val):rest] (mbURI,namespaces,attrs)
			# acc = case name of
				XMLQName ?None "xmlns"		= (?Just val,namespaces,attrs)
				XMLQName (?Just "xmlns") ns	= (mbURI,[(ns,val):namespaces],attrs)
				_							= (mbURI,namespaces,[attr:attrs])
			= filterNamespaces rest acc
	mkXMLDoc _ = abort "mkXMLDoc called on non-XMLElem\n"

pDocDeclaration	= symbol TokenDeclarationStart &> (<+?> pAttr) <& symbol TokenDeclarationEnd
pNode			= pCharData <@ (\d -> XMLText d) <!> pElem
pElem			= pElemCont <!> pElemEmpty <!> pCData
pElemCont		= pElemStart <&> (\(name,attributes) -> symbol TokenTagClose &> (<*?> pNode) <& pElemContEnd >?< ((==) name) <@ (\nodes -> XMLElem (toQName name) attributes nodes))
pElemEmpty		= pElemStart <& symbol TokenEmptyTagClose <@ (\(name,attributes) -> XMLElem (toQName name) attributes [])
pElemStart		= (\name attributes -> (name,attributes)) @> symbol TokenStartTagOpen -&+ pName +&+ (<*?> pAttr)
pElemContEnd	= symbol TokenEndTagOpen &> pName <& symbol TokenTagClose
pAttr			= (\name v -> XMLAttr (toQName name) v) @> pName +&- symbol TokenEqual +&+ pAttrValue
pName			= satisfy isName		<@ \n -> case n of TokenName n -> n; _ -> abort "error in pName\n"
pAttrValue		= satisfy isCharData	<@ \n -> case n of TokenCharData v -> v; _ -> abort "error in pAttrValue\n"
pCharData		= satisfy isCharData	<@ \n -> case n of TokenCharData d -> d; _ -> abort "error in pCharData\n"
pCData          = satisfy (\t -> t =: TokenCData _) <@ \t -> case t of TokenCData data = XMLCData data; _ = undef

toQName :: !String -> XMLQName
toQName name
	| colonIdx > 0	= qname (subString 0 colonIdx name) (subString (colonIdx + 1) (textSize name - colonIdx) name)
	| otherwise		= uname name
where
	colonIdx = indexOf ":" name 
	
// generic printer

toXML :: !a -> XMLDoc | XMLEncode{|*|} a
toXML a = XMLDoc ?None [] (wrapToElem (XMLEncode{|*|} a))

toXMLString :: !a -> String | XMLEncode{|*|} a
toXMLString a = toString (toXML a)

:: XMLEncodeResult = XMLEncElem !(!XMLQName,![XMLAttr],![XMLNode]) | XMLEncText !(!String,!XMLQName) | XMLEncNodes ![XMLNode] !XMLQName | XMLEncNothing

generic XMLEncode a :: !a -> XMLEncodeResult

XMLEncode{|OBJECT|} fx (OBJECT o) = fx o
XMLEncode{|CONS of d|} fx (CONS c)
	# nodes	= getNodes (fx c)
	# name	= uname (formatConsName d.gcd_name)
	| d.gcd_type_def.gtd_num_conses > 1 = XMLEncElem (name,[],nodes)
	| otherwise							= XMLEncNodes nodes name
where
	nonEmpty (XMLElem _ _ [])	= False
	nonEmpty _					= True
	
	formatConsName name
		| startsWith "_" name	= subString 1 (textSize name - 1) name
		| otherwise				= name
XMLEncode{|RECORD of d|} fx (RECORD c)
	# nodes	= getNodes (fx c)
	# name	= uname (formatConsName d.grd_name)
	| not (isEmpty d.grd_fields)		= XMLEncNodes (filter nonEmpty nodes) name
	| otherwise							= XMLEncNodes nodes name
where
	nonEmpty (XMLElem _ _ [])	= False
	nonEmpty _					= True
	
	formatConsName name
		| startsWith "_" name	= subString 1 (textSize name - 1) name
		| otherwise				= name
XMLEncode{|FIELD of d|} fx (FIELD f) = XMLEncElem (uname d.gfd_name,[],getNodes (fx f))
XMLEncode{|EITHER|} fx fy either = case either of
	LEFT x	= fx x
	RIGHT y	= fy y
XMLEncode{|PAIR|} fx fy (PAIR x y) = XMLEncNodes (getNodes` (fx x) ++ getNodes` (fy y)) (uname "PAIR")
where
	getNodes` (XMLEncNodes nodes _)	= nodes
	getNodes` res					= [wrapToElem res]
XMLEncode{|UNIT|} _ = XMLEncNodes [] (uname "UNIT")

XMLEncode{|Int|} i		= basicXML "integer" i
XMLEncode{|Char|} c		= basicXML "character" c
XMLEncode{|Real|} r		= basicXML "float" r
XMLEncode{|String|} s	= basicXML "string" s
XMLEncode{|Bool|} b		= basicXML "boolean" b

basicXML name v = XMLEncText (toString v,uname name)

XMLEncode{|[]|} fx list = XMLEncNodes (map (wrapToElem o fx) list) (uname "list")
XMLEncode{|(?)|} fx (?Just x) = fx x
XMLEncode{|(?)|} _  ?None     = XMLEncNothing

XMLEncode{|XMLIntAttribute|}	fx (XMLIntAttribute name v x)		= encodeAttr name v (fx x)
XMLEncode{|XMLCharAttribute|}	fx (XMLCharAttribute name v x)		= encodeAttr name v (fx x)
XMLEncode{|XMLRealAttribute|}	fx (XMLRealAttribute name v x)		= encodeAttr name v (fx x)
XMLEncode{|XMLStringAttribute|}	fx (XMLStringAttribute name v x)	= encodeAttr name v (fx x)
XMLEncode{|XMLBoolAttribute|}	fx (XMLBoolAttribute name v x)		= encodeAttr name v (fx x)

encodeAttr name a x = XMLEncElem (fromElem (wrapToElemAttr x [XMLAttr name (toString a)]))

derive XMLEncode Either, (,), (,,), (,,,)

// auxiliary functions
wrapToElem :: !XMLEncodeResult -> XMLNode
wrapToElem x = wrapToElemAttr x []

wrapToElemAttr :: !XMLEncodeResult ![XMLAttr] -> XMLNode
wrapToElemAttr (XMLEncElem (name,attr,nodes))	attr` = XMLElem name (attr ++ attr`) nodes
wrapToElemAttr (XMLEncText t=:(txt,name))		attr` = XMLElem name attr` [toText t]
wrapToElemAttr (XMLEncNodes nodes wname)		attr` = XMLElem wname attr` nodes
wrapToElemAttr XMLEncNothing					attr` = XMLElem (uname "nothing") attr` []

toElem :: !(!XMLQName,![XMLAttr],![XMLNode]) -> XMLNode
toElem (name,attr,nodes) = XMLElem name attr nodes

fromElem :: !XMLNode -> (!XMLQName,![XMLAttr],![XMLNode])
fromElem n = case n of
	XMLElem name attr nodes -> (name,attr,nodes)
	_                       -> abort "fromElem called on non-XMLElem\n"

toText :: !(!String,!XMLQName) -> XMLNode
toText (txt,_) = XMLText txt

getNodes :: !XMLEncodeResult -> [XMLNode]
getNodes (XMLEncElem elem)		= [toElem elem]
getNodes (XMLEncText txt)		= [toText txt]
getNodes (XMLEncNodes nodes _)	= nodes
getNodes XMLEncNothing			= []

derive gEq Token, XMLDoc, XMLQName, XMLNode, XMLAttr
