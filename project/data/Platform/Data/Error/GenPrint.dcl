definition module Data.Error.GenPrint

from Text.GenPrint import generic gPrint, :: PrintState, class PrintOutput
from Data.Error import :: MaybeError

derive gPrint MaybeError