implementation module System.FilePath

import StdArray
import StdList
import StdTuple
import StdString

import Data.Error, Data.Func
import Text
import System.OS
import System.OSError
import qualified System._FilePath

pathSeparator :: Char
pathSeparator = OS_PATH_SEPARATOR

pathSeparatorString :: String
pathSeparatorString =: {#pathSeparator}

pathSeparators :: [Char]
pathSeparators = ['\\', '/']

extSeparator :: Char
extSeparator = '.'

extSeparatorString :: String
extSeparatorString =: {#extSeparator}

(</>) infixr 5 :: !FilePath !FilePath -> FilePath
(</>) x y
	| hasTrailingPathSeparator x
		= x +++ y
		= concat [x,pathSeparatorString,y]

concatPaths :: ![FilePath] -> FilePath
concatPaths paths = concat (addSeparators paths)
where
	addSeparators [] = []
	addSeparators [p] = [p]
	addSeparators [p:ps]
		| hasTrailingPathSeparator p
			= [p:addSeparators ps]
			= [p,pathSeparatorString: addSeparators ps]

splitExtension :: !FilePath -> (String, String)
splitExtension path = split sz
where
	sz = size path - 1

	split :: !Int -> (String, String)
	split i
	| i <= 0             = (path, "")
	| c == pathSeparator = (path, "")
	| c == extSeparator
		| i == sz        = (path, "")
		| otherwise      = (path % (0,i-1), path % (i+1, sz))
	| otherwise          = split (i-1)
	where
		c = path.[i]

takeExtension :: !FilePath -> String
takeExtension path = snd (splitExtension path)

dropExtension :: !FilePath -> String
dropExtension path = fst (splitExtension path)

addExtension :: !FilePath !String -> FilePath
addExtension path ext
	| size ext == 0
		= path
	# sz = size path
	| sz == 0
		= ext
	| path.[sz-1] == extSeparator
		= path +++ ext
		= concat [path,extSeparatorString,ext]

replaceExtension :: !FilePath !String -> FilePath
replaceExtension path ext = addExtension (dropExtension path) ext

hasTrailingPathSeparator :: !FilePath -> Bool
hasTrailingPathSeparator path
	# sz = size path
	| sz == 0
		= False
		= path.[sz-1] == pathSeparator

splitFileName  :: !FilePath -> (String, String)
splitFileName path = 
	case lastIndexOf pathSeparatorString path of
		-1 -> ("", path)
		i  -> (subString 0 i path, subString (i+1) (size path - i - 1) path)

takeDirectory :: !FilePath -> FilePath
takeDirectory path = fst (splitFileName path) 

dropDirectory :: !FilePath -> String
dropDirectory path = case lastIndexOf pathSeparatorString path of
	-1                    = path
	i | i == sizePath - 1 = dropDirectory $ subString 0 (sizePath - 1) path // drop file separator at end of path
	  | otherwise         = subString (i+1) (sizePath - i - 1) path
where
    sizePath = size path

takeFileName :: !FilePath -> FilePath
takeFileName path = snd (splitFileName path) 

replaceFileName :: !FilePath !String -> FilePath
replaceFileName path fn = takeDirectory path </> fn

dropFileName :: !FilePath -> FilePath
dropFileName path = takeDirectory path

getFullPathName :: !FilePath !*World -> (!MaybeOSError FilePath, !*World)
getFullPathName p w = 'System._FilePath'.getFullPathName p w
