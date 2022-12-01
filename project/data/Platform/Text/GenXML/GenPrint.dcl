definition module Text.GenXML.GenPrint

from Text.GenXML   import :: XMLDoc, :: XMLNode, :: XMLAttr, :: XMLQName
from Text.GenPrint import generic gPrint, class PrintOutput, :: PrintState

derive gPrint XMLDoc, XMLQName, XMLNode, XMLAttr
