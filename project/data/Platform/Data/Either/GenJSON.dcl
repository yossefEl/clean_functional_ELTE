definition module Data.Either.GenJSON

from Data.Either  import :: Either
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

derive JSONEncode Either
derive JSONDecode Either
