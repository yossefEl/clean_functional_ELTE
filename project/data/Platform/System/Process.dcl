definition module System.Process

/**
 * This module provides functions to deal with external processes.
 *
 * Not yet implemented:
 * - Passsing environment, i.e. `[(!String,!String)]`, to created processes.
 */

import Data.Either
import System.OSError
import System.FilePath
from System._Process import :: ProcessHandle, :: WritePipe, :: ReadPipe,
	instance closePipe WritePipe, instance closePipe ReadPipe
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

:: ProcessIO =
	{ stdIn  :: !WritePipe
	, stdOut :: !ReadPipe
	, stdErr :: !ReadPipe
	}

/**
 * Starts a new process.
 *
 * @param Path to the executable.
 * @param A list of command-line arguments.
 * @param Optionally, a startup directory.
 * @result A process handle to the process.
 */
runProcess :: !FilePath ![String] !(?String) !*World -> (!MaybeOSError ProcessHandle, !*World)

/**
 * Starts a new process and opens pipes for IO.
 *
 * @param Path to the executable.
 * @param A list of command-line arguments.
 * @param Optionally, a startup directory.
 * @result A process handle to the process and pipes for IO.
 */
runProcessIO :: !FilePath ![String] !(?String) !*World -> (!MaybeOSError (ProcessHandle, ProcessIO), !*World)

/**
 * Starts a new process with a pseudoterminal for IO.
 *
 * Windows does not have the concept of pseudoterminals. On this platform, the
 * `ProcessPtyOptions` are ignored and `runProcessIO` is used instead.
 *
 * @param Path to the executable.
 * @param A list of command-line arguments.
 * @param Optionally, a startup directory.
 * @param The pseudoterminal options.
 * @result A process handle to the process and pipes for IO.
 */
runProcessPty :: !FilePath ![String] !(?String) !ProcessPtyOptions !*World -> (!MaybeOSError (ProcessHandle, ProcessIO), !*World)

/**
 * Options for the pseudoterminal.
 *
 * Use `defaultPtyOptions` for sensible defaults.
 *
 * This type is not used on Windows, which does not have the concept of
 * pseudoterminals.
 */
:: ProcessPtyOptions =
	{ childInNewSession :: !Bool //* Should the child process create a new terminal session (see `man setsid` for more info)?
	, childControlsTty  :: !Bool //* Should the child process control the terminal (check `man ioctl`/`TIOCSCTTY` for more info)?
	, useRawIO          :: !Bool //* Should the terminal do nothing with the IO (check `cfmakeraw` or `termios`)?
	}

defaultPtyOptions :: ProcessPtyOptions

//* This record contains the results of calling an external process.
:: ProcessResult =
	{ exitCode :: !Int //* The exitcode that was returned by the process.
	, stdout   :: !String //* The stdout that was returned by the process.
	, stderr   :: !String //* The stderr output that was returned by the process.
	}

/**
 * Check if a process is still running.
 *
 * @param The process handle to the process.
 * @result The return code if the process has finished, `?None` if the process
 *   is still running.
 */
checkProcess :: !ProcessHandle !*World -> (!MaybeOSError (?Int), !*World)

/**
 * Wait for a process to terminate, close the handle and return the exit code.
 *
 * @param The process handle to the process.
 * @result The exit code of the process.
 */
waitForProcess :: !ProcessHandle !*World -> (!MaybeOSError Int, !*World)

/**
 * Run a new process and wait for it to terminate.
 *
 * @param Path to the executable.
 * @param A list of command-line arguments.
 * @param Optionally, a startup directory.
 * @result The exit code of the process.
 */
callProcess :: !FilePath ![String] !(?String) !*World -> (!MaybeOSError Int, !*World)

/**
 * Run a new process and wait for it to terminate. Returns the output of the process.
 * NB, this function should only be used when: the process terminates and no input needs to be provided to the process.
 *
 * @param Path to the executable.
 * @param A list of command-line arguments.
 * @param Optionally, a startup directory.
 * @result The exit code, stdout and stderr output of the process.
 */
callProcessWithOutput :: !FilePath ![String] !(?String) !*World -> (!MaybeOSError ProcessResult, !*World)

/**
 * Run a new process and wait for it to terminate. All output produced by the
 * process is passed on to this process' `stdout` and `stderr` channels.
 *
 * On POSIX systems, this is the same behaviour as that of `callProcess`, but
 * on Windows, a process may open a new console window when called by
 * `callProcess`.
 *
 * See the documentation of `callProcess` for a description of the arguments.
 */
callProcessAndPassIO :: !FilePath ![String] !(?String) !*World -> (!MaybeOSError Int, !*World)

/**
 * Read the currently available data from a pipe without blocking if no data is
 * available.
 *
 * @param The pipe to read from.
 * @result The data read from the pipe.
 */
readPipeNonBlocking :: !ReadPipe !*World -> (!MaybeOSError String, !*World)

/**
 * Read at most *n* bytes from a pipe without blocking if no data is available.
 *
 * @param The pipe to read from.
 * @param The maximum number of bytes to read.
 * @result The data read from the pipe.
 */
readPipeNonBlockingN :: !ReadPipe !Int !*World -> (!MaybeOSError String, !*World)

/**
 * Read the currently available string from a pipe, blocking until some data is
 * available.
 *
 * @param The pipe to read from.
 * @result The data read from the pipe. The string may be empty only if the
 *   pipe has been closed.
 */
readPipeBlocking :: !ReadPipe !*World -> (!MaybeOSError String, !*World)

/**
 * Read at most *n* bytes from a pipe, blocking until some data is available.
 *
 * @param The pipe to read from.
 * @param The maximum number of bytes to read.
 * @result The data read from the pipe. The string may be empty only if the
 *   pipe has been closed.
 */
readPipeBlockingN :: !ReadPipe !Int !*World -> (!MaybeOSError String, !*World)

/**
 * Blocks until data is available for at least one of the given pipes. When
 * this is the case, all pipes are read in a non-blocking fashion. Therefore,
 * more than one string is non-empty. If all strings are empty, at least one
 * pipe was closed with no more data to read.
 *
 * @param The pipes to read from.
 * @result The data read from the pipes.
 */
readPipeBlockingMulti :: ![ReadPipe] !*World -> (!MaybeOSError [String], !*World)

/**
 * Writes data to a pipe.
 * This function may block if the buffer is full.
 *
 * @param The data to write.
 * @param The pipe to write to.
 */
writePipe :: !String !WritePipe !*World -> (!MaybeOSError (), !*World)

/**
 * Terminate a process (if it is still running) and release the process handle
 * resources.
 *
 * See `terminateProcessCode` if you have to use a specific signal (POSIX) or
 * exit code (Windows).
 *
 * @param The process handle.
 */
terminateProcess :: !ProcessHandle !*World -> (!MaybeOSError (), !*World)

/**
 * Terminate a process (if it is still running) and releases the process handle
 * resources.
 *
 * Compared to `terminateProcess`, this function allows you to use a specific
 * signal (POSIX) or exit code (Windows).
 *
 * @param The process handle.
 * @param On POSIX, the signal to send to the process; on Windows, the exit
 *   code of the process.
 */
terminateProcessCode :: !ProcessHandle !Int !*World -> (!MaybeOSError (), !*World)

//* Closes the IO channels of a process.
closeProcessIO :: !ProcessIO !*World -> (!MaybeOSError (), !*World)

//* Closes an individual IO channel.
class closePipe a :: !a !*World -> (!MaybeOSError (), !*World)
