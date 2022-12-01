definition module System.Time.Gast

import System.Time
from Gast import generic ggen, generic genShow, generic gPrint, :: GenState, :: PrintState, class PrintOutput
import Data.Int
import Gast.Gen
import StdEnum
import StdOverloadedList

derive ggen Timespec

derive genShow Timespec
derive gPrint Timespec

