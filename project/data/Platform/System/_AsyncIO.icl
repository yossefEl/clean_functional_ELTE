implementation module System._AsyncIO

import code from "cAsyncIO.o", "queue.o", "hashtable.o"

import StdEnv
import Data.Maybe
import qualified Data.List
import qualified Data.Map
from Data.Map import :: Map
from Data.Error import :: MaybeError (..), :: MaybeErrorString, isError, fromOk, fromError
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode, getLastOSError
from System.OS import IF_WINDOWS
from System.AsyncIO import :: AsyncIOFD, :: TimeoutMS, :: Port (..), :: MaxEvents, :: ConnectionType (..), instance == ConnectionType

instance == OS
where
	(==) Posix Posix = True
	(==) Windows Windows = True
	(==) _ _ = False


ioInit :: !*env -> (!MaybeOSError AsyncIOFD, !*env)
ioInit env
	# (asyncIoFd, env) = ioInitC env
	| asyncIoFd == -1 = getLastOSError env
	= (Ok asyncIoFd, env)
where
	ioInitC :: !*env -> (!AsyncIOFD, !*env)
	ioInitC env = code {
		ccall ioInitC ":I:A"
	}

ioGetEvents :: !AsyncIOFD !(?TimeoutMS) !MaxEvents !{#Int} !{#Int} !*env -> (!MaybeOSError Int, !*env)
ioGetEvents asyncIoFd timeout maxEvents fdList evKinds env
	# timeoutSettings = if (isNone timeout) (0, False) (fromJust timeout, True)
	# (numEvents, env) = ioGetEventsC asyncIoFd timeoutSettings maxEvents fdList evKinds env
	| numEvents == -1 = getLastOSError env
	= (Ok numEvents, env)
where
	ioGetEventsC :: !AsyncIOFD (!TimeoutMS,!Bool) !MaxEvents !{#Int} !{#Int} !*env -> (!Int, !*env)
	ioGetEventsC asyncIoFd (timeout,doTimeout) maxEvents fdList evKinds env = code {
		ccall ioGetEventsC "IIIIAA:I:A"
	}

accept :: !AsyncIOFD !FD !*env -> (!MaybeOSError (OS,FD), !*env)
accept mainFD listenFd env
	# ((err, fd), env) = acceptC mainFD listenFd env
	| err == -1 = getLastOSError env
	| err == -2 = (Error (0, "Connection request was aborted"), env)
	| err == 1 = (Ok (Windows,fd), env)
	= (Ok (Posix, fd), env)
where
	acceptC :: !AsyncIOFD !FD !*env -> (!(!OSErrorCode, !FD), !*env)
	acceptC mainFD listenFd env = code {
		ccall acceptCAsyncIO "II:VII:A"
	}

createTCPListener :: !AsyncIOFD !Port !*env -> (!MaybeOSError FD,!*env)
createTCPListener asyncIoFd (Port port) env
	# ((err, fd), env) = createTCPListenerC asyncIoFd port env
	| err == -1 = getLastOSError env
	= (Ok fd, env)
where
	createTCPListenerC :: !AsyncIOFD !Int !*env -> (!(!OSErrorCode, !FD),!*env)
	createTCPListenerC asyncIoFd port env = code {
		ccall tcplistenC "II:VII:A"
	}

connect :: !AsyncIOFD !Int !Port !*env -> (!MaybeOSError FD, !*env)
connect asyncIoFd ip (Port port) env
	# ((err, fd), env) = connectC asyncIoFd ip port env
	| err == -1 = getLastOSError env
	= (Ok fd, env)
where
	connectC :: !AsyncIOFD Int !Int !*env -> (!(!OSErrorCode, !FD), !*env)
	connectC asyncIoFd ip port env = code {
		ccall connectC "III:VII:A"
	}

queueWriteSock :: !AsyncIOFD !FD !String !*env -> (!MaybeOSError (), !*env)
queueWriteSock aioFd fd data env
	# (err, env) = queueWriteSockC aioFd fd (data) (size data) env
	| err == -1 = getLastOSError env
	= (Ok (), env)
where
	queueWriteSockC :: !AsyncIOFD !FD !String !Int !*env -> (!OSErrorCode, !*env)
	queueWriteSockC aioFd fd data size env = code {
		ccall queueWriteSockC "IIsI:I:A"
	}

signalWriteSock :: !AsyncIOFD !FD !*env -> (!MaybeOSError (), !*env)
signalWriteSock asyncIoFd socket env
	# (err, env) = signalWriteSockC asyncIoFd socket env
	| err == -1 = getLastOSError env
	= (Ok (), env)
where
	signalWriteSockC :: !AsyncIOFD !FD !*env -> (!OSErrorCode, !*env)
	signalWriteSockC asyncIoFd socket env = code {
		ccall signalWriteSockC "II:I:A"
	}

getpeername :: !FD !FD !*env -> (!MaybeOSError Int, !*env)
getpeername clientFd listenFd env
	# ((err, ip), env) = getpeernameC clientFd listenFd env
	| err == -1 = getLastOSError env
	= (Ok ip, env)
where
	getpeernameC :: !FD !FD !*env -> (!(!OSErrorCode, !Int), !*env)
	getpeernameC clientFd listenFd env = code {
		ccall getpeernameC "II:II:A"
	}

retrieveData :: !AsyncIOFD !FD !*env -> (!MaybeOSError (Bool, String), !*env)
retrieveData asyncIoFd fd env
	# ((err, data), env) = retrieveDataC asyncIoFd fd env
	| err == -1 = getLastOSError env
	// Disconnect detected.
	| err == 0 = (Ok (True, data), env)
	= (Ok (False, data), env)
where
	retrieveDataC :: !AsyncIOFD !FD !*env -> (!(!OSErrorCode, !String), !*env)
	retrieveDataC asyncIoFd fd env = code {
		ccall retrieveDataC "II:VIS:A"
	}

cleanupFd :: !AsyncIOFD !FD !ConnectionType !*env -> (!MaybeOSError (), !*env)
cleanupFd asyncIoFd fd cType env
	# isASocket = cType == Socket
	# (err, env) = cleanupFdC asyncIoFd fd isASocket env
	| err == -1 = getLastOSError env
	= (Ok (), env)
where
	cleanupFdC :: !AsyncIOFD !FD !Bool !*env -> (!Int, !*env)
	cleanupFdC asyncIoFd fd isASocket env = code {
		ccall cleanupFdC "III:I:A"
	}

anyPendingPackets :: !FD !*env -> (!MaybeOSError Bool, !*env)
anyPendingPackets fd env
	# (err, anyPendingPackets,env) = anyPendingPacketsC fd env
	| err == -1 = getLastOSError env
    = (Ok anyPendingPackets, env)
where
	anyPendingPacketsC :: !FD !*env -> (!Int, !Bool, !*env)
	anyPendingPacketsC fd env = code {
		ccall anyPendingPacketsC "I:II:A"
	}

windowsReadSock :: !FD !*env -> (!MaybeOSError (), !*env)
windowsReadSock socket env = abort "windowsReadSock should not be used on posix."

windowsIncPacketsToWrite :: !FD !Int !*env -> (!MaybeOSError (), !*env)
windowsIncPacketsToWrite fd numPackets env = abort "windowsIncPacketsToWrite should not be used on Posix."

windowsAccept :: !AsyncIOFD !FD !*env -> (!MaybeOSError (OS,FD), !*env)
windowsAccept mainFD listenFd env = abort "windowsAccept should not be used on Posix."

