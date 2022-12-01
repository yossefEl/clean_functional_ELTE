definition module System._AsyncIO

from System.AsyncIO import :: AsyncIOFD, :: TimeoutMS, :: Port, :: MaxEvents, :: ConnectionType
from System.OSError import :: MaybeOSError (..), :: OSError (..), :: OSErrorCode, :: OSErrorMessage
from StdClass import class <
from Data.Map import :: Map
from Data.Error import :: MaybeError (..), :: MaybeErrorString


/**
 * @representation File descriptor
 */
:: FD            :== Int

:: OS = Windows | Posix

instance == OS

ioInit :: !*env -> (!MaybeOSError AsyncIOFD, !*env)

ioGetEvents :: !AsyncIOFD !(?TimeoutMS) !MaxEvents !{#Int} !{#Int} !*env -> (!MaybeOSError Int, !*env)

windowsAccept :: !AsyncIOFD !FD !*env -> (!MaybeOSError (OS,FD), !*env)

accept :: !AsyncIOFD !FD !*env -> (!MaybeOSError (OS,FD), !*env)

createTCPListener :: !AsyncIOFD !Port !*env -> (!MaybeOSError FD,!*env)

connect :: !AsyncIOFD !Int !Port !*env -> (!MaybeOSError FD, !*env)

queueWriteSock :: !AsyncIOFD !FD !String !*env -> (!MaybeOSError (), !*env)

signalWriteSock :: !AsyncIOFD !FD !*env -> (!MaybeOSError (), !*env)

getpeername :: !FD !FD !*env -> (!MaybeOSError Int, !*env)

retrieveData :: !AsyncIOFD !FD !*env -> (!MaybeOSError (Bool, String), !*env)

windowsReadSock :: !FD !*env -> (!MaybeOSError (), !*env)

cleanupFd :: !AsyncIOFD !FD !ConnectionType !*env -> (!MaybeOSError (), !*env)

windowsIncPacketsToWrite :: !FD !Int !*env -> (!MaybeOSError (), !*env)

anyPendingPackets :: !FD !*env -> (!MaybeOSError Bool, !*env)

