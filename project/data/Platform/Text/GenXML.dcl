definition module Text.GenXML

/**
 * This module provides data types for easy construction of XML documents and
 * a generic conversion of Clean values to XML.
 *
 * @property-bootstrap
 *     import StdEnv, Data.Error, Data.Func, Text.GenXML.Gast, Text.GenXML.GenPrint, Data.Maybe.Gast
 *
 *     derive genShow MaybeError
 *     derive gPrint  MaybeError
 */

import StdOverloaded, StdGeneric, Data.Either
from Data.Error import :: MaybeErrorString, :: MaybeError
from Data.GenEq import generic gEq

:: XMLDoc = XMLDoc !(?XMLURI) ![(XMLNamespacePrefix,XMLURI)] !XMLNode

instance == XMLDoc
derive gEq XMLDoc

/**
 * A node in an XML document.
 */
:: XMLNode
	= XMLElem  !XMLQName ![XMLAttr] ![XMLNode] //* An element
	| XMLText  !String                         //* Character data
	| XMLCData !String                         //* A CDATA section

derive gEq XMLNode

:: XMLAttr = XMLAttr !XMLQName !String

derive gEq XMLAttr

:: XMLQName = XMLQName !(?XMLNamespacePrefix) !XMLName

derive gEq XMLQName

:: XMLNamespacePrefix :== String
:: XMLURI :== String
:: XMLName :== String

/**
* Create an XMLQName containing an unqualified name from a String.
*
* @param Unqualified name
* @result XMLQName containing the unqualified name
*/
uname :: !String -> XMLQName

/**
* Create an XMLQName containing a qualified name from a String.
*
* @param Qualified name
* @result XMLQName containing the qualified name
*/
qname :: !XMLNamespacePrefix !String -> XMLQName

instance toString XMLDoc

/**
 * The XML document corresponding to a string.
 *
 * @property correctness: A.doc :: XMLDocWithString:
 *     fromString doc.stringRepresentation =.= typedOk doc.document
 *     where
 *         typedOk :: !XMLDoc -> MaybeErrorString XMLDoc
 *         typedOk doc = Ok doc
 */
instance fromString (MaybeErrorString XMLDoc)

// generic printer

toXML			:: !a -> XMLDoc	| XMLEncode{|*|} a
toXMLString		:: !a -> String	| XMLEncode{|*|} a

:: XMLEncodeResult

generic XMLEncode a :: !a -> XMLEncodeResult

// special types for adding attributes to XML data
:: XMLIntAttribute		a = XMLIntAttribute		!XMLQName !Int		!a
:: XMLCharAttribute		a = XMLCharAttribute	!XMLQName !Char		!a
:: XMLRealAttribute		a = XMLRealAttribute	!XMLQName !Real		!a
:: XMLStringAttribute	a = XMLStringAttribute	!XMLQName !String	!a
:: XMLBoolAttribute		a = XMLBoolAttribute	!XMLQName !Bool		!a

derive XMLEncode OBJECT, CONS of d, FIELD of d, PAIR, EITHER, UNIT, Int, Char, Real, String, Bool
derive XMLEncode ?, Either, (,), (,,), (,,,), []
derive XMLEncode XMLIntAttribute, XMLCharAttribute, XMLRealAttribute, XMLStringAttribute, XMLBoolAttribute

