implementation module System.OSError

import Data.Error
import qualified System._OSError

getLastOSError :: !*w -> (!MaybeOSError .a, !*w)
getLastOSError world 
	# (err, world) = 'System._OSError'._getLastOSErrorCode world
	= (Error (osErrorCodeToOSError err), world)

getLastOSErrorCode :: !*w -> (!MaybeOSErrorCode .a, !*w)
getLastOSErrorCode world
	# (err, world) = 'System._OSError'._getLastOSErrorCode world
	= (Error err, world)

osErrorCodeToOSError :: !OSErrorCode -> OSError
osErrorCodeToOSError errno = (errno, 'System._OSError'._osErrorCodeToMessage errno)
