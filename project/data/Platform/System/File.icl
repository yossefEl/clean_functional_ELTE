implementation module System.File

import StdEnv

import System.Time
import Data.Error
import System.OS
import System.OSError
import qualified System._CopyFile
import qualified System._File
import System._Pointer
import qualified Text
from Text import class Text, instance Text String

instance toString FileError
where
	toString CannotOpen = "Cannot open"
	toString CannotClose = "Cannot close"
	toString IOError = "I/O error"

readFile :: !String !*env -> (!MaybeError FileError String, !*env) | FileSystem env
readFile filename env = withFile filename FReadData readAll env

readAll :: !*File -> (!MaybeError FileError String, !*File)
readAll file
	# (ok,file)  = fseek file 0 FSeekEnd
	| not ok     = (Error IOError,file)
	# (pos,file) = fposition file
	# (err,file) = ferror file
	| err        = (Error IOError,file)
	# (ok,file)  = fseek file 0 FSeekSet
	| not ok     = (Error IOError,file)
	# (str,file) = freads file pos
	# (err,file) = ferror file
	| err        = (Error IOError,file)
	| otherwise  = (Ok str,file)

readFileLines :: !String !*env -> (!MaybeError FileError [String], !*env) | FileSystem env
readFileLines filename env = withFile filename FReadData readAllLines env

readAllLines :: !*File -> (!MaybeError FileError [String], !*File)
readAllLines file
# (result, file) = rec file []
= case result of
	Error e	 = (Error e, file)
	Ok lines = (Ok (reverse lines), file)
where
	rec :: *File [String] -> (!MaybeError FileError [String], *File)
	rec file acc
		# (string, file) = freadline file
		# (err,file)	 = ferror file
		| err			 = (Error IOError,file)
		| string == ""   = (Ok acc, file)
		| otherwise      = rec file [string:acc]

writeFile :: !String !String !*env -> (!MaybeError FileError (), !*env) | FileSystem env
writeFile filename contents env =
	withFile filename FWriteData (\file -> (Ok (), fwrites contents file)) env

withFile :: !String !Int (*File -> (MaybeError FileError a,*File)) !*env
			-> (!MaybeError FileError a, !*env) | FileSystem env
withFile filename filemode operation env
# (ok,file,env)	= fopen filename filemode env
| not ok			= (Error CannotOpen, env)
# (result,file)		= operation file
| isError result 	= (result, env)
# (ok,env)	 		= fclose file env
| not ok			= (Error CannotClose, env)
= (Ok (fromOk result), env)

fileExists :: !String !*World -> (!Bool, !*World)
fileExists path world = 'System._File'._fileExists path world

deleteFile :: !String !*World -> (!MaybeOSError (), !*World)
deleteFile path world
	# (ok,world) = 'System._File'._deleteFile path world
	| ok        = (Ok (), world)
	| otherwise = getLastOSError world

getFileInfo :: !String !*World -> (!MaybeOSError FileInfo, !*World)
getFileInfo path world = 'System._File'._getFileInfo path world

moveFile :: !String !String !*World -> (!MaybeOSError (), !*World)
moveFile oldpath newpath world = 'System._File'._moveFile (packString oldpath) (packString newpath) world

copyFile :: !String !String !*World -> (!MaybeOSError (), !*World)
copyFile srcPath destPath world = 'System._CopyFile'._copyFile (packString srcPath) (packString destPath) world
