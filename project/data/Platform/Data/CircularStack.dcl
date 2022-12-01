definition module Data.CircularStack

from Data.IntMap.Strict import :: IntMap

:: CircularStack a =
  { maxSize    :: !Int
  , actualSize :: !Int
  , nextIdx    :: !Int
  , stackData  :: !IntMap a
  }

newStack   :: !Int -> CircularStack a

push       :: !a !(CircularStack a) -> CircularStack a

pop        :: !(CircularStack a) -> (!?a, !CircularStack a)

peek       :: !(CircularStack a) -> ?a

emptyStack :: !(CircularStack a) -> Bool

fromList   :: ![a] -> CircularStack a

toList     :: !(CircularStack a) -> [a]
