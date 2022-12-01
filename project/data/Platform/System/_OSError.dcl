definition module System._OSError

/**
 * This is a platform-specific module. Use the general interface in
 * System.OSError instead.
 */

from System.OSError import :: OSErrorCode, :: OSErrorMessage

_getLastOSErrorCode :: !*w -> (!OSErrorCode, !*w)

_osErrorCodeToMessage :: !OSErrorCode -> OSErrorMessage
