definition module Text.GenJSON

/**
 * This module provides functions to encode and decode any Clean data type
 * to JSON format. It provides two generic functions JSONEncode and JSONDecode
 * which must be derived for concrete types. Then toJSON and fromJSON may be
 * used to convert any value to and from JSON.
 *
 * For more info about JSON see: http://www.json.org/
 *
 * @property-bootstrap
 *     import StdEnv
 *     import Data.GenEq, Data.Int, Data.List, Data.Maybe, Text
 *
 *     :: TestRecord = {a :: Int, b` :: [(Int,[Char])], c :: ()}
 *
 *     derive ggen    TestRecord, JSONNode
 *     derive genShow TestRecord, JSONNode
 *     derive gPrint  TestRecord, JSONNode
 *
 *     derive gEq TestRecord
 *     instance == TestRecord where (==) a b = a === b
 *
 *     derive JSONEncode TestRecord
 *     derive JSONDecode TestRecord
 *
 * @property-test-with type=Int
 * @property-test-with type=Bool
 * @property-test-with type=Char
 * @property-test-with type=String
 * @property-test-with type=()
 * @property-test-with type=[TestRecord]
 */

import StdGeneric
from _SystemStrictLists import class UList, class UTSList
from StdFile import class <<<
from StdOverloaded import class fromString, class toString, class ==(..)
from StdString import instance == {#Char}
from Data.List import !?
from Data.GenEq import generic gEq

/**
 * @invariant noErrorOrRaw: A.json :: JSONNode:
 *     case json of
 *         JSONNull     = prop True
 *         JSONBool _   = prop True
 *         JSONInt _    = prop True
 *         JSONReal _   = prop True
 *         JSONString _ = prop True
 *         JSONArray a  = foldl (\p j -> p /\ noErrorOrRaw j) (prop True) a
 *         JSONObject o = foldl (\p (_,j) -> p /\ noErrorOrRaw j) (prop True) o
 *         JSONRaw _    = prop False
 *         JSONError    = prop False
 */
:: JSONNode	= JSONNull
			| JSONBool !Bool
			| JSONInt !Int
			| JSONReal !Real
			| JSONString !String //* Only control characters and '"' will be escaped
			| JSONArray ![JSONNode]
			| JSONObject ![(String,JSONNode)]
			| JSONRaw !String
			| JSONError
/**
* Serializing JSON structures is done with a toString instance
*/
instance toString JSONNode

/**
 * Deserializing JSON structures is done with a fromString instance
 *
 * @property no error/raw constructors for generated values: A.x :: type:
 *     noErrorOrRaw (fromString (toString (toJSON x)))
 * @property error for incomplete parsings: A.x :: type; s :: String:
 *     let json = toJSON x in
 *         toInt s == 0 ==> // we may append more digits to an int, in which case it is not invalid
 *         trim s <> "" ==> // if we only add whitespace it is not invalid
 *         fromString (toString json +++ s) =.= JSONError
 */
instance fromString JSONNode

/**
* Serialize a JSON structure and write to a File
*/
instance <<< JSONNode

derive gEq JSONNode

/**
 * Encodes any value to JSON format.
 *
 * @property correctness: A.a :: type:
 *     maybe (prop False) ((=.=) a) (fromJSON (fromString (toString (toJSON a))))
 *
 * @property correctness Real: A.a :: Real:
 *     // Since integer-valued Reals are printed as integers (i.e., without
 *     // trailing `.0`), they are parsed as `JSONInt` and only later converted
 *     // to Reals. Because of this we can only parse Reals within the integer
 *     // boundaries.
 *     let i = toInt a in i <> SmallestInt && i <> LargestInt ==>
 *         case fromJSON (fromString (toString (toJSON a))) of
 *             ?None   -> prop False
 *             ?Just b -> if (isNaN a) (prop (isNaN b)) (toString a =.= fromReal b)
 *
 * @param The value to encode
 * @return The JSON encoded value
 */
toJSON        :: !a -> JSONNode | JSONEncode{|*|} a

toJSONInField :: !a -> JSONNode | JSONEncode{|*|} a

/**
* Tries to parse a JSON encoded string.
* When parsing fails, the result is `?None`.
*
* @param The JSON encoded input
* @return `?Just` the result, when parsing succeeds
*/
fromJSON :: !JSONNode -> ?a | JSONDecode{|*|} a

/**
* Escapes a string for manual JSON construction
*
* @param The unescaped string
* @return A properly escaped string
*/
jsonEscape	:: !String	-> String

/**
* Unescapes a string that is escaped for use in a serialized JSON string
*
* @param The escaped string
* @return An unescaped string
*/
jsonUnescape :: !String -> String

/**
* Simple query-by-path function that enables searching of JSON structures
*
* @param The query path separated by '/'. Objects are indexed by fieldname
*        and arrays by their array index.
*        Example paths: 'node1/node3' 'node1/node2/23'
*
* @return The value if a value of the right type is at that path.
*/
jsonQuery :: !String !JSONNode -> ?a | JSONDecode{|*|} a

/**
* Generic encoding function. This function should not be used
* directly but always through the toJSON function. It must be derived
* for each type you want to encode in JSON format.
*/
generic JSONEncode t :: !Bool !t -> [JSONNode]
derive JSONEncode Int, Real, Char, Bool, String
derive JSONEncode [], [!], [ !], [!!], [#], [#!]
derive JSONEncode (), (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
derive JSONEncode {}, {!}, ?, JSONNode

JSONEncode{|UNIT|} _ (UNIT) = []
JSONEncode{|PAIR|} fx fy _ (PAIR x y) = fx False x ++ fy False y
where
	(++) infixr 5::![.a] !u:[.a] -> u:[.a]
	(++) [hd:tl]	list	= [hd:tl ++ list]
	(++) nil 		list	= list
JSONEncode{|EITHER|} fx fy _ (LEFT x) = fx False x
JSONEncode{|EITHER|} fx fy _ (RIGHT y) = fy False y
JSONEncode{|OBJECT|} fx _ (OBJECT x) = fx False x
JSONEncode{|CONS of {gcd_name}|} fx _ (CONS x)
  = [JSONArray [JSONString gcd_name : fx False x]]
JSONEncode{|RECORD of {grd_fields}|} fx _ (RECORD x)
	= [JSONObject [(name, o) \\ o <- fx False x & name <- grd_fields | isNotNull o]]
where
	isNotNull :: !JSONNode -> Bool
	isNotNull JSONNull = False
	isNotNull _ = True
JSONEncode{|FIELD|} fx _ (FIELD x) = case fx True x of [jn] = [jn]

/**
* Generic decoding function. This function should not be used
* directly, but always through the fromJSON function. It must be derived
* for each type you want to parse from JSON format.
*/
generic JSONDecode t :: !Bool ![JSONNode] -> (!?t,![JSONNode])
derive JSONDecode Int, Real, Char, Bool, String
derive JSONDecode [], [!], [ !], [!!], [#], [#!]
derive JSONDecode (), (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
derive JSONDecode {}, {!}, ?, JSONNode

JSONDecode{|UNIT|} _ l = (?Just UNIT, l)
JSONDecode{|EITHER|} fx fy _ l = case fx False l of
	(?Just x, xs) -> (?Just (LEFT x),xs)
	(?None,   xs) -> case fy False l of
		(?Just y, ys) -> (?Just (RIGHT y),ys)
		(?None,   ys) -> (?None, l)
JSONDecode{|OBJECT|} fx _ l = case fx False l of
	(?Just x, xs) -> (?Just (OBJECT x),xs)
	_             -> (?None, l)
JSONDecode{|CONS of {gcd_name}|} fx _ l=:[JSONArray [JSONString name:fields] :xs]
	| name == gcd_name = case fx False fields of
		(?Just x, _) -> (?Just (CONS x), xs)
		_            -> (?None, l)
	| otherwise = (?None, l)
JSONDecode{|CONS|} fx _ l = (?None, l)
JSONDecode{|PAIR|} fx fy _ l = d1 fy (fx False l) l
where
	d1 :: !(Bool [JSONNode] -> (?b, [JSONNode])) !(!?a, ![JSONNode]) ![JSONNode] -> (!?(PAIR a b), ![JSONNode])
	d1 fy (?Just x,xs) l = d2 x (fy False xs) l
	d1 _  (?None, _)   l = (?None, l)

	d2 :: !a !(!?b, ![JSONNode]) ![JSONNode] -> (!?(PAIR a b), ![JSONNode])
	d2 x (?Just y, ys) l = (?Just (PAIR x y), ys)
	d2 x (?None,   _)  l = (?None, l)
JSONDecode{|RECORD|} fx _ l=:[obj=:JSONObject fields : xs] = d (fx False [obj]) xs l
where
	d :: !(?a, b) ![JSONNode] ![JSONNode] -> (!?(RECORD a), ![JSONNode])
	d (?Just x, _) xs l = (?Just (RECORD x),xs)
	d (?None, _)   xs l = (?None, l)
JSONDecode{|RECORD|} fx _ l=:[obj=:JSONArray fields : xs] = d (fx False [obj]) xs l
where
	d :: !(?a, b) ![JSONNode] ![JSONNode] -> (!?(RECORD a), ![JSONNode])
	d (?Just x, _) xs l = (?Just (RECORD x),xs)
	d (?None, _)   xs l = (?None, l)
JSONDecode{|RECORD|} fx _ l = (?None,l)
JSONDecode{|FIELD of {gfd_name}|} fx _ l =:[JSONObject fields]
	#! field = findField gfd_name fields
	= case fx True field of
		(?Just x, _) -> (?Just (FIELD x), l)
		(_, _)       -> (?None, l)
where
	findField :: !String ![(String, JSONNode)] -> [JSONNode]
	findField match [(l,x):xs]
		| l == match = [x]
		| otherwise  = findField match xs
	findField match [] = []
JSONDecode{|FIELD of {gfd_index}|} fx _ l=:[JSONArray fields] = case fields !? gfd_index of
	?None       -> (?None, l)
	?Just field -> case fx True [field] of
		(?Just x, _) -> (?Just (FIELD x), l)
		(_, _)       -> (?None, l)
JSONDecode{|FIELD|} fx _ l = (?None, l)

/**
* Equality of JSON nodes.
* JSON Reals are considered equal if their string representation is equal.
* JSON Objects are considered equal if they contain the same non-null fields.
*/
instance == JSONNode

/**
* Pretty printed string encoding of JSON nodes.
* This function uses indenting and newlines to make the serialized JSON representation
* more readable than the standard toString instance, which uses minimal whitespace.
*/
jsonPrettyPrint :: !JSONNode -> String


