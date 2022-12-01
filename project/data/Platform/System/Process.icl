implementation module System.Process

import StdEnv

import Data.Func
import Data.Functor
import Data.Maybe
import System.FilePath
import System.OS
import System.OSError
import System._Process
import qualified System._Process

runProcess :: !FilePath ![String] !(?String) !*World -> (!MaybeOSError ProcessHandle, !*World)
runProcess exe args dir w
	# (mbHandle,w) = 'System._Process'._startProcess exe args dir ?None w
	| isError mbHandle = (liftError mbHandle, w)
	| otherwise = (Ok (fst (fromOk mbHandle)), w)

runProcessIO :: !FilePath ![String] !(?String) !*World -> (!MaybeOSError (ProcessHandle, ProcessIO), !*World)
runProcessIO exe args dir w
	# (pipeStdIn, w) = 'System._Process'._openPipePair True w
	| isError pipeStdIn = (liftError pipeStdIn, w)
	# (pipeStdOut, w) = 'System._Process'._openPipePair False w
	| isError pipeStdOut = (liftError pipeStdOut, w)
	# (pipeStdErr, w) = 'System._Process'._openPipePair False w
	| isError pipeStdErr = (liftError pipeStdErr, w)
	# (mbHandleAndIO, w) = 'System._Process'._startProcess
		exe args dir
		(?Just ((fromOk pipeStdIn), (fromOk pipeStdOut), (fromOk pipeStdErr)))
		w
	| isError mbHandleAndIO = (liftError mbHandleAndIO, w)
	# (handle, mbIO) = fromOk mbHandleAndIO
	| isNone mbIO = abort "runProcessIO: _startProcess returned ?None for IO\n"
	| otherwise   = (Ok (handle, fromJust mbIO), w)

runProcessPty :: !FilePath ![String] !(?String) !ProcessPtyOptions !*World -> (!MaybeOSError (ProcessHandle, ProcessIO), !*World)
runProcessPty exe args dir ptyOpts w = IF_WINDOWS
	(runProcessIO exe args dir w)
	('System._Process'._startProcessPty exe args dir ptyOpts w)

defaultPtyOptions :: ProcessPtyOptions
defaultPtyOptions =
	{ childInNewSession = True
	, childControlsTty  = True
	, useRawIO          = False
	}

checkProcess :: !ProcessHandle !*World -> (!MaybeOSError (?Int), !*World)
checkProcess ph w = 'System._Process'._checkProcess ph w

waitForProcess :: !ProcessHandle !*World -> (!MaybeOSError Int, !*World)
waitForProcess ph w = 'System._Process'._waitForProcess ph w

callProcess :: !FilePath ![String] !(?String) !*World -> (!MaybeOSError Int, !*World)
callProcess exe args dir world
	# (res, world) = runProcess exe args dir world
	= case res of
		Ok ph   = waitForProcess ph world
		Error e = (Error e, world)

callProcessWithOutput :: !FilePath ![String] !(?String) !*World -> (!MaybeOSError ProcessResult, !*World)
callProcessWithOutput exe args dir world
	# (res, world) = runProcessIO exe args dir world
	= case res of
		Ok (ph, pio=:{ProcessIO|stdOut, stdErr})
			# (mbExitCode, world) = waitForProcess ph world
			| isError mbExitCode = (liftError mbExitCode, world)
			# (mbStdOut, world) = readPipeNonBlocking stdOut world
			| isError mbStdOut = (liftError mbStdOut, world)
			# (mbStdErr, world) = readPipeNonBlocking stdErr world
			| isError mbStdErr = (liftError mbStdErr, world)
			# (mbErr, world) = closeProcessIO pio world
			| isError mbErr = (liftError mbErr, world)
			= (Ok {exitCode=fromOk mbExitCode, stdout=fromOk mbStdOut, stderr=fromOk mbStdErr}, world)
		Error e = (Error e, world)

callProcessAndPassIO :: !FilePath ![String] !(?String) !*World -> (!MaybeOSError Int, !*World)
callProcessAndPassIO exe args dir w
	| IF_POSIX True False
		= callProcess exe args dir w // On POSIX, this is the default behaviour
	# (mbHandleAndIO,w) = runProcessIO exe args dir w
	| isError mbHandleAndIO
		= (liftError mbHandleAndIO, w)
	# (ph,pio) = fromOk mbHandleAndIO
	# (io,w) = stdio w
	# (mbExitCode,w) = passIOUntilDone ph pio io stderr w
	= (mbExitCode, w)
where
	passIOUntilDone :: !ProcessHandle !ProcessIO !*File !*File !*World -> *(!MaybeOSError Int, !*World)
	passIOUntilDone ph pio io err w
		# (mbExitCode,w) = checkProcess ph w
		| isError mbExitCode || isJust (fromOk mbExitCode)
			# (mbStrings,w) = readPipeBlockingMulti [pio.stdOut, pio.stdErr] w
			# (io,err) = case mbStrings of
				Ok [stdout,stderr:_] = (io <<< stdout, err <<< stderr)
				_ = (io, err)
			# (_,w) = fclose io w
			  (_,w) = fclose err w
			= (fromJust <$> mbExitCode, w)
		# (mbStrings,w) = readPipeBlockingMulti [pio.stdOut, pio.stdErr] w
		| isError mbStrings
			# (_,w) = fclose io w
			  (_,w) = fclose err w
			= (liftError mbStrings, w)
		# [stdout,stderr:_] = fromOk mbStrings
		= passIOUntilDone ph pio (io <<< stdout) (err <<< stderr) w

readPipeNonBlocking :: !ReadPipe !*World -> (!MaybeOSError String, !*World)
readPipeNonBlocking pipe w
	# (mbN,w) = 'System._Process'._peekPipe pipe w
	| isError mbN = (liftError mbN, w)
	# n = fromOk mbN
	| n == 0      = (Ok "", w)
	| otherwise   = readPipeNonBlockingN pipe (fromOk mbN) w

readPipeNonBlockingN :: !ReadPipe !Int !*World -> (!MaybeOSError String, !*World)
readPipeNonBlockingN pipe n w = 'System._Process'._readPipeNonBlocking pipe n w

readPipeBlocking :: !ReadPipe !*World -> (!MaybeOSError String, !*World)
readPipeBlocking pipe w
	# (mbErr,w) = 'System._Process'._blockPipe pipe w
	| isError mbErr = (liftError mbErr, w)
	| otherwise     = readPipeNonBlocking pipe w

readPipeBlockingN :: !ReadPipe !Int !*World -> (!MaybeOSError String, !*World)
readPipeBlockingN pipe n w
	# (mbErr,w) = 'System._Process'._blockPipe pipe w
	| isError mbErr = (liftError mbErr, w)
	| otherwise     = readPipeNonBlockingN pipe n w

readPipeBlockingMulti :: ![ReadPipe] !*World -> (!MaybeOSError [String], !*World)
readPipeBlockingMulti pipes w
	# (mbErr,w) = 'System._Process'._blockAnyPipe pipes w
	| isError mbErr = (liftError mbErr, w)
	// NB: At least one pipe is ready, but we read them all. This is
	// documented in the dcl.
	# (mbStrings, w) = mapSt readPipeNonBlocking pipes w
	= case filter isError mbStrings of
		[e:_] -> (liftError e, w)
		_     -> (Ok (map fromOk mbStrings), w)

writePipe :: !String !WritePipe !*World -> (!MaybeOSError (), !*World)
writePipe data pipe w = 'System._Process'._writePipe data pipe w

terminateProcess :: !ProcessHandle !*World -> (!MaybeOSError (), !*World)
terminateProcess ph w = terminateProcessCode ph (IF_WINDOWS 0 15) w

terminateProcessCode :: !ProcessHandle !Int !*World -> (!MaybeOSError (), !*World)
terminateProcessCode ph signal_or_code w = 'System._Process'._terminateProcess ph signal_or_code w

closeProcessIO :: !ProcessIO !*World -> (!MaybeOSError (), !*World)
closeProcessIO pio w
	# (mbErr1,w) = closePipe pio.stdIn w
	| 'System._Process'._equalPipe pio.stdIn pio.stdOut // with pseudoterminals, the same file descriptor is used
		= (mbErr1, w)
	# (mbErr2,w) = closePipe pio.stdOut w
	# (mbErr3,w) = closePipe pio.stdErr w
	| isError mbErr1 = (mbErr1, w)
	| isError mbErr2 = (mbErr2, w)
	| otherwise      = (mbErr3, w)
