implementation module System._File

import StdEnv

import Data.Error
import System.File
import System.OSError
import System._Pointer
import System._Posix
import qualified System._CopyFile

_fileExists :: !String !*World -> (!Bool, !*World)
_fileExists path world
	# buf = createArray sizeOfStat '\0'
	# (ret,world) = stat (packString path) buf world
	= (ret == 0, world)

_deleteFile :: !String !*World -> (!Bool, !*World)
_deleteFile path world
	# (ret,world) = unlink (packString path) world
	= (ret == 0, world)

_getFileInfo :: !String !*World -> (!MaybeOSError FileInfo, !*World)
_getFileInfo path world
	# buf           = createArray sizeOfStat '\0'
	# (ok,world)    = stat (packString path) buf world
	| ok <> 0		= getLastOSError world
	# stat			= unpackStat buf
	= (Ok { directory = (stat.st_mode bitand S_IFMT) == S_IFDIR
		  , creationTime = stat.st_ctimespec
		  , lastModifiedTime = stat.st_mtimespec
		  , lastAccessedTime = stat.st_atimespec
		  , sizeHigh = stat.st_blocks * stat.st_blksize
		  , sizeLow = stat.st_size
		  }, world)

_moveFile :: !String !String !*World -> (!MaybeOSError (), !*World)
_moveFile oldpath newpath w
	# (ret, w) = rename (packString oldpath) (packString newpath) w
	| ret == 0 = (Ok (), w)
	= case getLastOSErrorCode w of
		// Different filesystem so we have to copy by hand and remove the old
		// 18 = EXDEV
		// This is portable between mac and linux because mac won't return this
		// error code if it supports cross device renaming
		(Error 18, w) = case 'System._CopyFile'._copyFile oldpath newpath w of
			(Ok (), w)
				# (ok, w) = _deleteFile oldpath w
				| not ok = getLastOSError w
				= (Ok (), w)
			e = e
		(Error _, w) = getLastOSError w
