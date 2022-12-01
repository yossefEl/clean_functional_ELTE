module char

import StdEnv
import Gast

pChar c = isAlpha c || toUpper c == c

Start = test pChar
