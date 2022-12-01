module bool

import StdEnv
import Gast

Start = test \x y.(x && y) == not (not x || not y)
