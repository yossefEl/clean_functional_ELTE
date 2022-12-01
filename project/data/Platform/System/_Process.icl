implementation module System._Process

import StdEnv
import StdFunc

from Data.Foldable import maximum
import Data.Functor
from Data.List import instance Foldable []
import Data.Maybe
import Data.Tuple
import System.FilePath
import System.OSError
import System.Process
import System._Pointer
import System._Posix
import Text.GenJSON

:: WritePipe =: WritePipe Int
:: ReadPipe =: ReadPipe Int

derive JSONEncode WritePipe, ReadPipe
derive JSONDecode WritePipe, ReadPipe

_openPipePair :: !Bool !*World -> (!MaybeOSError (Int, Int), !*World)
_openPipePair _ w
	# ptr = malloc 8
	| ptr == 0 = abort "malloc failed\n"
	# (res, w) = pipe ptr w
	| res == -1
		# w = freeSt ptr w
		= getLastOSError w
	# (rEnd, ptr)  = readP (\ptr -> readInt4S ptr 0) ptr
	# (wEnd, ptr)  = readP (\ptr -> readInt4S ptr 4) ptr
	# w = freeSt ptr w
	= (Ok (rEnd, wEnd), w)

instance closePipe WritePipe
where
	closePipe :: !WritePipe !*World -> (!MaybeOSError (), !*World)
	closePipe (WritePipe pipe) w = closePipe` pipe w

instance closePipe ReadPipe
where
	closePipe :: !ReadPipe !*World -> (!MaybeOSError (), !*World)
	closePipe (ReadPipe pipe) w = closePipe` pipe w

closePipe` :: !Int !*World -> (!MaybeOSError (), !*World)
closePipe` pipe w
	# (res, w) = close pipe w
	| res <> 0  = getLastOSError w
	| otherwise = (Ok (), w)

_blockPipe :: !ReadPipe !*World -> (!MaybeOSError (), !*World)
_blockPipe (ReadPipe pipe) w = appFst (fmap (const ())) (readSelect [pipe] 0 w)

_blockAnyPipe :: ![ReadPipe] !*World -> (!MaybeOSError (), !*World)
_blockAnyPipe pipes w
	# (res, w) = readSelect [pipe \\ ReadPipe pipe <- pipes] 0 w
	| isError res = (liftError res, w)
	| otherwise   = (Ok (), w)

_peekPipe :: !ReadPipe !*World -> (!MaybeOSError Int, !*World)
_peekPipe (ReadPipe pipe) w = pipeBufferSize pipe w

_readPipeNonBlocking :: !ReadPipe !Int !*World -> (!MaybeOSError String, !*World)
_readPipeNonBlocking (ReadPipe fd) n w
	# (timeout, w) = mallocSt 16 w
	| timeout == 0 = getLastOSError w
	# timeout = IF_INT_64_OR_32
		(writeInt (writeInt timeout 0 0) 8 0)
		(writeInt (writeInt (writeInt (writeInt timeout 0 0) 4 0) 8 0) 12 0)
	# ((res, w), timeout) = readP (\ptr -> readSelect [fd] ptr w) timeout
	# w = freeSt timeout w
	| isError res = (liftError res, w)
	| and (fromOk res)
		# (buf, w) = mallocSt n w
		| buf == 0 = getLastOSError w
		# (res, w) = read fd buf n w
		| res == -1 = getLastOSError (freeSt buf w)
		# (str, buf) = readP (\ptr -> derefCharArray ptr res) buf
		= (Ok str, freeSt buf w)
	= (Ok "", w)

_writePipe :: !String !WritePipe !*World -> (!MaybeOSError (), !*World)
_writePipe data (WritePipe pipe) w
	# (res, w) = write pipe data (size data) w
	| res == -1
		= getLastOSError w
		= (Ok (), w)

_equalPipe :: !WritePipe !ReadPipe -> Bool
_equalPipe (WritePipe x) (ReadPipe y) = x == y

readSelect :: ![Int] !Pointer !*World -> (!MaybeOSError [Bool], !*World)
readSelect fds timeout world
	# readfds = malloc 128
	| readfds == 0 = getLastOSError world
	# readfds = seq [\ptr -> writeIntElemOffset ptr i 0 \\ i <- [0..IF_INT_64_OR_32 15 31]] readfds
	# readfds = seq [setFdBit fd\\fd<-fds] readfds
	# (res, world) = select_ (maximum fds + 1) readfds 0 0 timeout world
	| res == -1 = getLastOSError (freeSt readfds world)
	# (res, readfds) = seqList [readP (getFdBit fd)\\fd<-fds] readfds
	# world = freeSt readfds world
	= (Ok res, world)
where
	setFdBit :: !Int !Pointer -> Pointer
	setFdBit fd ptr
		# offset  = fromInt fd / IF_INT_64_OR_32 64 32
		# val = (readIntElemOffset ptr offset) bitor (1 << (fd rem IF_INT_64_OR_32 64 32))
		= writeIntElemOffset ptr offset val

	getFdBit :: !Int !Pointer -> Bool
	getFdBit fd ptr
		# offset  = fromInt fd / IF_INT_64_OR_32 64 32
		# val = (readIntElemOffset ptr offset) bitand (1 << (fd rem IF_INT_64_OR_32 64 32))
		= val <> 0

_startProcess ::
	!FilePath ![String] !(?String)
	!(?((Int,Int), (Int,Int), (Int,Int)))
	!*World -> (!MaybeOSError (ProcessHandle, ?ProcessIO), !*World)
_startProcess exe args dir mbPipes w = runProcessFork (childProcess mbPipes) (parentProcess mbPipes) w
where
	childProcess ::
		!(?((Int,Int), (Int,Int), (Int,Int))) !Int !Int
		!*World -> (!MaybeOSError (ProcessHandle, ?ProcessIO), !*World)
	childProcess ?None pipeExecErrorOut pipeExecErrorIn w
		# (_, w) = runProcessChildProcessExec exe args dir pipeExecErrorOut pipeExecErrorIn w
		= (undef, w) // this is never executed as 'childProcessExec' never returns
	childProcess
			(?Just ((pipeStdInOut, pipeStdInIn), (pipeStdOutOut, pipeStdOutIn), (pipeStdErrOut, pipeStdErrIn)))
			pipeExecErrorOut pipeExecErrorIn w
		//redirect stdin/out/err to pipes
		# (mbErr,w) = redirectPipe pipeStdInOut pipeStdInIn STDIN_FILENO w
		| isError mbErr = (liftError mbErr, w)
		# (mbErr,w) = redirectPipe pipeStdOutIn pipeStdOutOut STDOUT_FILENO w
		| isError mbErr = (liftError mbErr, w)
		# (mbErr,w) = redirectPipe pipeStdErrIn pipeStdErrOut STDERR_FILENO w
		| isError mbErr = (liftError mbErr, w)
		# (_, w) = runProcessChildProcessExec exe args dir pipeExecErrorOut pipeExecErrorIn w
		= (undef, w) // this is never executed as 'childProcessExec' never returns
	where
		redirectPipe pipe1 pipe2 fileno w
			# (res, w) = dup2 pipe1 fileno w
			| res == -1 = getLastOSError w
			# (res, w) = close pipe2 w
			| res == -1 = getLastOSError w
			| otherwise = (Ok (), w)

	parentProcess ::
		!(?((Int,Int), (Int,Int), (Int,Int))) !Int !Int !Int
		!*World -> (!MaybeOSError (ProcessHandle, ?ProcessIO), !*World)
	parentProcess ?None pid pipeExecErrorOut pipeExecErrorIn w
		# (mbHandle, w) = runProcessParentProcessCheckError pid pipeExecErrorOut pipeExecErrorIn w
		| isError mbHandle = (liftError mbHandle, w)
		| otherwise = (Ok (fromOk mbHandle, ?None), w)
	parentProcess
			(?Just ((pipeStdInOut, pipeStdInIn), (pipeStdOutOut, pipeStdOutIn), (pipeStdErrOut, pipeStdErrIn)))
			pid pipeExecErrorOut pipeExecErrorIn w
		# (res, w) = close pipeStdInOut w
		| res == -1 = getLastOSError w
		# (res, w) = close pipeStdOutIn w
		| res == -1 = getLastOSError w
		# (res, w) = close pipeStdErrIn w
		| res == -1 = getLastOSError w
		# (mbHandle, w) = runProcessParentProcessCheckError pid pipeExecErrorOut pipeExecErrorIn w
		| isError mbHandle = (liftError mbHandle, w)
		| otherwise =
			( Ok
				( fromOk mbHandle
				, ?Just
					{ stdIn  = WritePipe pipeStdInIn
					, stdOut = ReadPipe  pipeStdOutOut
					, stdErr = ReadPipe  pipeStdErrOut
					}
				)
			, w
			)

_startProcessPty ::
	!FilePath ![String] !(?String) !ProcessPtyOptions
	!*World -> (!MaybeOSError (ProcessHandle, ProcessIO), !*World)
_startProcessPty path args mCurrentDirectory opts world
	# (masterPty, world) = posix_openpt (O_RDWR bitor O_NOCTTY) world
	| masterPty == -1    = getLastOSError world
	# (slavePty, world)  = grantpt masterPty world
	| slavePty == -1     = getLastOSError world
	# (slavePty, world)  = unlockpt masterPty world
	| slavePty == -1     = getLastOSError world
	# (slavePty, world)  = ptsname masterPty world
	| slavePty == 0      = getLastOSError world
	# (slavePty, world)  = open slavePty (O_RDWR bitor O_NOCTTY) world
	| slavePty == -1     = getLastOSError world
	= runProcessFork
		(childProcess  slavePty masterPty)
		(parentProcess slavePty masterPty)
		world
where
	childProcess :: !Int !Int !Int !Int !*World -> (!MaybeOSError (!ProcessHandle, !ProcessIO), !*World)
	childProcess slavePty masterPty pipeExecErrorOut pipeExecErrorIn world
		//Close the master side
		# (res, world) = close masterPty world
		| res == -1    = getLastOSError world

		//Disable echo
		//sizeof(struct termios) on linux gives 60, on mac 72, lets play safe
		# termios      = malloc 128
		| termios == 0 = getLastOSError world
		# (res, world) = tcgetattr slavePty termios world
		| res == -1    = getLastOSError world

		//Apply the termios transformation
		# world        = (if opts.useRawIO cfmakeraw (flip const)) termios world
		# (res, world) = tcsetattr slavePty TCSANOW termios world
		| res == -1    = getLastOSError world
		# world        = freeSt termios world

		//Close our stdio
		# (res, world) = close STDIN_FILENO world
		| res == -1    = getLastOSError world
		# (res, world) = close STDOUT_FILENO world
		| res == -1    = getLastOSError world
		# (res, world) = close STDERR_FILENO world
		| res == -1    = getLastOSError world

		//Connect the pty to the stdio
		# (res, world) = dup2 slavePty STDIN_FILENO world
		| res == -1    = getLastOSError world
		# (res, world) = dup2 slavePty STDOUT_FILENO world
		| res == -1    = getLastOSError world
		# (res, world) = dup2 slavePty STDERR_FILENO world
		| res == -1    = getLastOSError world

		// Close slavePty since it is duped anyway
		# (res, world) = close slavePty world
		| res == -1    = getLastOSError world

		//Set the correct ioctl settings
		# world        = (if opts.childInNewSession setsid id) world
		# (res, world) = if opts.childControlsTty (0, world)
			(ioctl TCSANOW TIOCSCTTY 1 world)
		| res == -1    = getLastOSError world
		//Start
		# (_, world)   = runProcessChildProcessExec path args mCurrentDirectory pipeExecErrorOut pipeExecErrorIn world
		// this is never executed as 'childProcessExec' never returns
		= (undef, world)

	parentProcess :: !Int !Int !Int !Int !Int !*World -> (!MaybeOSError (!ProcessHandle, !ProcessIO), !*World)
	parentProcess slavePty masterPty pid pipeExecErrorOut pipeExecErrorIn world
		//sizeof(struct termios) on linux gives 60, lets play safe
		# termios          = malloc 128
		| termios == 0     = getLastOSError world
		# (res, world)     = tcgetattr masterPty termios world
		| res == -1        = getLastOSError world

		//Apply the termios transformation
		# world            = (if opts.useRawIO cfmakeraw (flip const)) termios world
		# (res, world)     = tcsetattr slavePty TCSANOW termios world
		| res == -1        = getLastOSError world

		//Close the slave side
		# (res, world)     = close slavePty world
		| res == -1        = getLastOSError world
		# world            = freeSt termios world
		//Start
		# (mbPHandle, world) = runProcessParentProcessCheckError pid pipeExecErrorOut pipeExecErrorIn world
		| isError mbPHandle  = (liftError mbPHandle, world)
		= ( Ok ( fromOk mbPHandle
		       , { stdIn  = WritePipe masterPty
		         , stdOut = ReadPipe masterPty
		         , stdErr = ReadPipe masterPty
		         }
		       )
		  , world)

runProcessFork ::
	!(    Int Int *World -> (MaybeOSError a, *World))
	!(Int Int Int *World -> (MaybeOSError a, *World))
	!*World -> (!MaybeOSError a, !*World)
runProcessFork childProcess parentProcess w
	// create pipe to pass errors of 'execvp' from child to parent
	# (pipeExecError, w) = _openPipePair False w
	| isError pipeExecError = (liftError pipeExecError, w)
	# (pipeExecErrorOut, pipeExecErrorIn) = fromOk pipeExecError
	# (pid, w) = fork w
	| pid == 0  = childProcess      pipeExecErrorOut pipeExecErrorIn w
	| pid > 0   = parentProcess pid pipeExecErrorOut pipeExecErrorIn w
	| otherwise = getLastOSError w

// This function never returns, as the process is replaced by `execvp`.
// All errors before `execvp` succeeds are passed on to the parent process.
runProcessChildProcessExec :: !FilePath ![String] !(?String) !Int !Int !*World -> (!MaybeOSError ProcessHandle, !*World)
runProcessChildProcessExec path args mCurrentDirectory pipeExecErrorOut pipeExecErrorIn world
	# (res, world) = close pipeExecErrorOut world
	| res == -1    = passLastOSErrorToParent pipeExecErrorIn world
	// set FD_CLOEXEC such that parent is informed if 'execvp' succeeds
	# (res, world) = fcntlArg pipeExecErrorIn F_SETFD FD_CLOEXEC world
	| res == -1    = passLastOSErrorToParent pipeExecErrorIn world
	//Chdir
	# (res,world) = case mCurrentDirectory of
		?Just dir -> chdir (packString dir) world
		?None     -> (0, world)
	| res <> 0 = passLastOSErrorToParent pipeExecErrorIn world
	//Exec
	# (argv, world) = runProcessMakeArgv [path:args] world
	# (res, world)  = execvp (path +++ "\0") argv world
	// this part is only executed if 'execvp' failed
	// in this case the error is passed to the parent
	= passLastOSErrorToParent pipeExecErrorIn world
where
	passLastOSErrorToParent :: !Int !*World -> (MaybeOSError ProcessHandle, *World)
	passLastOSErrorToParent pipe world
		# (errno, world) = errno world
		# (_, world)     = writePipe (toString errno) (WritePipe pipe) world
		// potential error of 'writePipe' cannot be handled properly
		= exit errno world

	runProcessMakeArgv :: [String] *World -> (!{#Pointer}, *World)
	runProcessMakeArgv argv_list world
		# args_size = argvLength argv_list 0
		  args_string = createArgsString args_size argv_list
		  args_memory = malloc args_size
		| args_memory == 0
			= abort "malloc failed\n"
		# args_memory = memcpy_string_to_pointer args_memory args_string args_size
		# (argv, args_memory) = readP (createArgv argv_list) args_memory
		= (argv, world)
	where
		argvLength [a:as] l
			= argvLength as (l+((size a +(IF_INT_64_OR_32 8 4)) bitand (IF_INT_64_OR_32 -8 -4)))
		argvLength [] l
			= l

		createArgsString args_size argv_list
			# s = createArray args_size '\0'
			= copyArgs argv_list 0 s
		where
			copyArgs [a:as] i s
				# s = copyChars 0 a i s
				= copyArgs as (i+((size a +(IF_INT_64_OR_32 8 4)) bitand (IF_INT_64_OR_32 -8 -4))) s
			copyArgs [] i s
				= s

			copyChars :: !Int !{#Char} !Int !*{#Char} -> *{#Char}
			copyChars ai a si s
				| ai<size a
					# s = {s & [si]=a.[ai]}
					= copyChars (ai+1) a (si+1) s
				= s

		createArgv argv_list args_memory
			# n_args = length argv_list
			# argv = createArray (n_args+1) 0;
			= fillArgv 0 argv_list argv args_memory
		where
			fillArgv :: !Int ![{#Char}] !*{#Pointer} !Int -> *{#Pointer}
			fillArgv arg_n [a:as] argv args_memory
				# argv = {argv & [arg_n]=args_memory}
				  args_memory = args_memory + ((size a +(IF_INT_64_OR_32 8 4)) bitand (IF_INT_64_OR_32 -8 -4))
				= fillArgv (arg_n+1) as argv args_memory
			fillArgv arg_n [] argv args_memory
				= {argv & [arg_n]=0}

runProcessParentProcessCheckError :: !Int !Int !Int !*World -> (!MaybeOSError ProcessHandle, !*World)
runProcessParentProcessCheckError pid pipeExecErrorOut pipeExecErrorIn w
	# (res, w) = close pipeExecErrorIn w
	| res == -1 = getLastOSError w
	// this blocks until either an error is written to the pipe or 'execvp' succeeds
	# (mbErrno, w) = readPipeBlocking (ReadPipe pipeExecErrorOut) w
	| isError mbErrno = (liftError mbErrno, w)
	# errno = fromOk mbErrno
	| errno <> "" = (Error (osErrorCodeToOSError (toInt errno)), w)
	# (res, w) = close pipeExecErrorOut w
	| res == -1 = getLastOSError w
	= (Ok {ProcessHandle | pid = pid}, w)

_checkProcess :: !ProcessHandle !*World -> (!MaybeOSError (?Int), !*World)
_checkProcess {pid} w
	# status = createArray 1 0
	# (ret,w) = waitpid pid status WNOHANG w // non-blocking wait
	| ret == 0
		= (Ok ?None, w)
	| ret == pid
		= (Ok (?Just (waitpidToExitCode status.[0])), w)
		= getLastOSError w

_waitForProcess :: !ProcessHandle !*World -> (!MaybeOSError Int, !*World)
_waitForProcess {pid} w
	# status = createArray 1 0
	# (ret,w) = waitpid pid status 0 w //Blocking wait
	| ret == pid
		= (Ok (waitpidToExitCode status.[0]), w)
	| otherwise
		= getLastOSError w

/* Converts the value returned by `waitpid` to the exit code.
 * Considers normal termination and termination by signal. */
waitpidToExitCode :: !Int -> Int
waitpidToExitCode status
	| signal <> 0
		= 128 + signal
		= (status bitand 0xFF00) >> 8
where
	signal = status bitand 0x7F

_terminateProcess :: !ProcessHandle !Int !*World -> (!MaybeOSError (), !*World)
_terminateProcess {pid} sig w
	# (res, w) = kill pid sig w // Termination signal
	| res == -1 = getLastOSError w
	// otherwise process will remain as zombie
	# status = createArray 1 0
	# (res, w) = waitpid pid status 0 w
	| res == -1 = getLastOSError w
	| otherwise = (Ok (), w)
