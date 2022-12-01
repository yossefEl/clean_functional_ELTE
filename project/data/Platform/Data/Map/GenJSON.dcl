definition module Data.Map.GenJSON

from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.Map     import :: Map

derive JSONEncode Map
derive JSONDecode Map
