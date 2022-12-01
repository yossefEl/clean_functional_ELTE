definition module Data.Maybe.GenParse

import _SystemStrictMaybes
from Text.GenParse import generic gParse, :: Expr

derive gParse ?, ?^, ?#
