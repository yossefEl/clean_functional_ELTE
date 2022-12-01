definition module System._File

/**
 * This is a platform-specific module. Use the general interface in System.File
 * instead.
 */

from System.File import :: FileInfo
from System.OSError import :: MaybeOSError, :: MaybeError, :: OSError,
	:: OSErrorCode, :: OSErrorMessage

_fileExists :: !String !*World -> (!Bool, !*World)
_deleteFile :: !String !*World -> (!Bool, !*World)
_getFileInfo :: !String !*World -> (!MaybeOSError FileInfo, !*World)

/*
 * Move a file from the src to dst.
 *
 * @param src
 * @param dst
 */
_moveFile :: !String !String !*World -> (!MaybeOSError (), !*World)
