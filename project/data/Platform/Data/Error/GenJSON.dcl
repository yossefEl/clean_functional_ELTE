definition module Data.Error.GenJSON

from Data.Error   import :: MaybeError
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

derive JSONEncode MaybeError
derive JSONDecode MaybeError
