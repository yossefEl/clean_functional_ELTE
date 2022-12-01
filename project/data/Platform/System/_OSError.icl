implementation module System._OSError

import System.OSError
import System._Pointer
import System._Posix

_getLastOSErrorCode :: !*w -> (!OSErrorCode, !*w)
_getLastOSErrorCode world = errno world

_osErrorCodeToMessage :: !OSErrorCode -> OSErrorMessage
_osErrorCodeToMessage errno = derefString (strerr errno)
