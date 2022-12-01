definition module Data.Set.GenJSON

from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.Set     import :: Set

derive JSONEncode Set
derive JSONDecode Set
