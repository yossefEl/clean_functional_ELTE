implementation module Data.Maybe.Ord

import StdEnv

instance < (?a) | < a
where
	(<) ?None      ?None      = False
	(<) ?None      _          = True
	(<) (?Just a1) (?Just a2) = a1 < a2
	(<) _          _          = False
