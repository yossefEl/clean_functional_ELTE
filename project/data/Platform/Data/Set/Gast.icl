implementation module Data.Set.Gast

import Gast, Data.Set, Data.Func

genShow{|Set|} fx sep p x rest = genShow{|* -> *|} fx sep p (lazyList $ toList x) rest

lazyList :: ![a] -> [a]
lazyList x = x
