definition module System._CopyFile

from System.OSError import :: MaybeOSError, :: MaybeError, :: OSError, :: OSErrorCode, :: OSErrorMessage

_copyFile :: !String !String !*World -> (!MaybeOSError (), !*World)
