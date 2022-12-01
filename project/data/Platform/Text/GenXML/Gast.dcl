definition module Text.GenXML.Gast

from Text.GenPrint import generic gPrint, class PrintOutput, :: PrintState
from Gast          import generic ggen, generic genShow, :: GenState
from Text.GenXML   import :: XMLDoc, :: XMLNode, :: XMLAttr, :: XMLQName

//* An XML doc with the corresponding string representation.
:: XMLDocWithString = {document :: !XMLDoc, stringRepresentation :: !String}

derive ggen    XMLDocWithString, XMLDoc, XMLQName, XMLNode, XMLAttr
derive genShow XMLDocWithString, XMLDoc, XMLQName, XMLNode, XMLAttr
derive gPrint  XMLDocWithString
