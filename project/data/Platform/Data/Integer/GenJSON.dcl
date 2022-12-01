definition module Data.Integer.GenJSON

from Data.Integer import :: Integer
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

derive JSONEncode Integer
derive JSONDecode Integer
