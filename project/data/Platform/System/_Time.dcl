definition module System._Time

/**
 * This is a platform-specific module. Use the general interface in System.Time
 * instead.
 */

from System.Time import :: Timespec
from Data.Error import :: MaybeError
from System.OSError import :: OSError, :: MaybeOSError, :: OSErrorCode, :: OSErrorMessage

//* The resolution of the system clock.
CLK_PER_SEC	:== 1000000

_timegm :: !{#Int} -> Int
_nsTime :: !*World -> (!Timespec, !*World)
_tsSleep :: !Timespec !*World -> (!MaybeOSError (), !*World)
