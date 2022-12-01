implementation module Text.GenXML.GenPrint

import Data.Maybe.GenPrint
import Text.GenXML, Text.GenPrint

derive gPrint XMLDoc, XMLQName, XMLNode, XMLAttr
