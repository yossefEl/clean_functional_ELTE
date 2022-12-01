definition module System._Process

/**
 * This is a platform-specific module. Use the general interface in
 * System.Process instead.
 */

from System.FilePath import :: FilePath
from System.OSError import :: MaybeOSError, :: MaybeError, :: OSError,
	:: OSErrorCode, :: OSErrorMessage
import System.Process
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

:: ProcessHandle =
	{ pid :: !Int
	}

:: WritePipe (=: WritePipe Int)
:: ReadPipe (=: ReadPipe Int)

derive JSONEncode WritePipe, ReadPipe
derive JSONDecode WritePipe, ReadPipe

_openPipePair :: !Bool !*World -> (!MaybeOSError (Int, Int), !*World)

instance closePipe WritePipe
instance closePipe ReadPipe

_blockPipe :: !ReadPipe !*World -> (!MaybeOSError (), !*World)
_blockAnyPipe :: ![ReadPipe] !*World -> (!MaybeOSError (), !*World)
_peekPipe :: !ReadPipe !*World -> (!MaybeOSError Int, !*World)
_readPipeNonBlocking :: !ReadPipe !Int !*World -> (!MaybeOSError String, !*World)
_writePipe :: !String !WritePipe !*World -> (!MaybeOSError (), !*World)

_equalPipe :: !WritePipe !ReadPipe -> Bool

_startProcess ::
	!FilePath ![String] !(?String)
	!(?((Int,Int), (Int,Int), (Int,Int)))
	!*World -> (!MaybeOSError (ProcessHandle, ?ProcessIO), !*World)

_startProcessPty ::
	!FilePath ![String] !(?String) !ProcessPtyOptions
	!*World -> (!MaybeOSError (ProcessHandle, ProcessIO), !*World)

_checkProcess :: !ProcessHandle !*World -> (!MaybeOSError (?Int), !*World)
_waitForProcess :: !ProcessHandle !*World -> (!MaybeOSError Int, !*World)
_terminateProcess :: !ProcessHandle !Int !*World -> (!MaybeOSError (), !*World)
