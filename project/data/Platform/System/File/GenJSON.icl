implementation module System.File.GenJSON

import Text.GenJSON
import System.File
import System.Time.GenJSON

derive JSONEncode FileError, FileInfo
derive JSONDecode FileError, FileInfo