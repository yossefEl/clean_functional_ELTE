definition module System.File.GenJSON

from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from System.File import :: FileError, :: FileInfo

derive JSONEncode FileError, FileInfo
derive JSONDecode FileError, FileInfo