implementation module System.AsyncIO

import StdEnv
import Data.Maybe
import Data.Functor
import qualified Data.List
import qualified Data.Map
import qualified Data.Array
import qualified Data.Foldable
import qualified System._AsyncIO
from Data.Func import $
from Data.Map import :: Map
from Data.Error import :: MaybeError (..), :: MaybeErrorString, isError, fromOk, fromError, instance Functor (MaybeError e)
                     , liftError
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode, getLastOSError
from System.OS import IF_WINDOWS
from TCPChannels import lookupIPAddress, :: IPAddress, class ChannelEnv
from TCPChannelClass import instance ChannelEnv World
from tcp import unpack_ipaddr, toDottedDecimal
from System._AsyncIO import :: OS (..), :: ConnectionType (..), :: FD, instance == OS
from Data.List import instance Foldable []
import Control.Monad

:: ConnectionId =: ConnectionId Int
:: IPAddr =: IPAddr String
:: AsyncIOFD     :== Int

/**
 * Data type used to represent events returned by the I/O multiplexer.
 *
 * AcceptEventSock : Incoming connection for TCP listener.
 * ConnectEventSock: Connection attempt succeeded for TCP client.
 * ReadEventSock: Data arrived.
 * WriteEventSock: All pending data was sent.
 * WriteNopEventSock: Data might have been sent but there is still data pending.
 * ReadAndWriteEventSock: Data was sent and received.
 * DisconnectEventSock: Disconnect was detected.
 */
:: IOEvent = AcceptEventSock | ConnectEventSock | ReadEventSock | WriteEventSock
           | WriteNopEventSock | ReadAndWriteEventSock | DisconnectEventSock

/**
 * Pair of a file descriptor and the IOEvent that occurred on the file descriptor.
 */
:: ConnectionEvent =
	{ cId :: !ConnectionId
	, ev :: !IOEvent
	}

instance < ConnectionId where
	(<) (ConnectionId x) (ConnectionId y) = x < y

instance toString ConnectionId where
	toString (ConnectionId id) = toString id

instance == ConnectionId
where
	(==) (ConnectionId a) (ConnectionId b) = a == b

instance toString Port where
	toString (Port p) = toString p

instance < Port where
	(<) (Port p1) (Port p2) = p1 < p2

instance == Port where
	(==) (Port p1) (Port p2) = p1 == p2

instance == IOEvent
where
	(==) AcceptEventSock AcceptEventSock = True
	(==) ConnectEventSock ConnectEventSock = True
	(==) ReadEventSock ReadEventSock = True
	(==) WriteEventSock WriteEventSock = True
	(==) WriteNopEventSock WriteNopEventSock = True
	(==) ReadAndWriteEventSock ReadAndWriteEventSock = True
	(==) DisconnectEventSock DisconnectEventSock = True
	(==) _ _ = False

instance == ConnectionType
where
	(==) Socket Socket = True
	(==) _ _ = False

emptyAIOState :: !*World -> (!MaybeOSError (*AIOState *env), !*World)
emptyAIOState world
	# (mbAioFd, world) = ioInit world
	| isError mbAioFd = (Error (fromError mbAioFd), world)
	# aioFd = fromOk mbAioFd
	= (Ok
	     { AIOState
		 | aioFd     = aioFd
		 , listeners = 'Data.Map'.newMap
		 , clients   = 'Data.Map'.newMap
		 , writeQueues   = 'Data.Map'.newMap
		 }
		 , world)

addListener :: !(ConnectionHandlers *env) !Port !*env -> *env | containsAIOState env
addListener handlers port env
	# (aioState=:{AIOState|listeners, clients, aioFd}, env) = getAIOState env
	# (mbListenId, env) = createTCPListener aioFd port env
	| isError mbListenId = printOSError (fromError mbListenId) env
	# listenId = fromOk mbListenId
	# (mbErr, env) = IF_WINDOWS (windowsAccept aioFd listenId env) (Ok (Posix, ConnectionId -1),env)
	| isError mbErr = printOSError (fromError mbErr) env
	# (os, clientId) = fromOk mbErr
	| os == Posix
		# listeners = 'Data.Map'.put listenId
			{Listener
			|handlers = handlers
			, removeOnClose = True
			, mbLastClient = ?None
			} listeners
		# aioState = {AIOState|aioState & listeners = listeners}
		= updAIOState aioState env
    // os == Windows, monitor client.
	# listeners
		= 'Data.Map'.put listenId
		{ Listener
		| handlers = handlers
		, removeOnClose = True
		, mbLastClient = ?Just clientId
		} listeners
	# clients
		= 'Data.Map'.put clientId
		{ Client
		| handlers = handlers
		, mbIpAddr = ?None
		, connected = False
		, closeAfterWrite = False
		, evaluateHandlers = True
		, removeOnClose = True
		} clients
	# aioState = {AIOState|aioState & listeners = listeners, clients = clients}
	= updAIOState aioState env

addConnection :: !(ConnectionHandlers *env) !Hostname !Port !*env
              -> (!MaybeOSError ConnectionId, !*env) | containsAIOState, containsWorld env
addConnection handlers hostaddr port env
	# (mbIp, env) = accWorld (lookupIPAddress hostaddr) env
	| isNone mbIp = (Error (-1,"Failed to connect to " +++ hostaddr +++ ", lookup failed"), env)
	# ip = unpack_ipaddr (fromJust mbIp)
	# (aioState=:{AIOState|aioFd, clients}, env) = getAIOState env
	# (mbClient, env) = connect aioFd ip port env
	| isError mbClient = (liftError mbClient, env)
	# clientId = fromOk mbClient
	// Client is not connected at this point in time.
	# clients = 'Data.Map'.put clientId
		{Client
		| handlers = handlers
		, mbIpAddr = ?Just (IPAddr (toString ip))
		, connected = False
		, closeAfterWrite = False
		, evaluateHandlers = True
		, removeOnClose = True
		}
		clients
	# aioState = {aioState & clients = clients}
	= (Ok clientId, updAIOState aioState env)

defaultLoop :: !(?Int) !*env -> *env | containsAIOState env
defaultLoop terminationBehavior env = loop terminationBehavior ?None 10 env

loop :: !(?Int) !(?TimeoutMS) !Int !*env -> *env | containsAIOState env
loop ?None timeoutMs maxEvents env
	# (aioState=:{AIOState|aioFd, writeQueues},env) = getAIOState env
	# aioState = {aioState & writeQueues = 'Data.Map'.newMap}
	# env = updAIOState aioState env
	= loop ?None timeoutMs maxEvents (tick timeoutMs maxEvents env)
loop (?Just ticksLeft) timeoutMs maxEvents env
	| ticksLeft == 0 = env
	# (aioState=:{AIOState|aioFd, writeQueues},env) = getAIOState env
	# aioState = {aioState & writeQueues = 'Data.Map'.newMap}
	# env = updAIOState aioState env
	# env = tick timeoutMs maxEvents env
	= loop (?Just (ticksLeft - 1)) timeoutMs maxEvents env

tick :: !(?TimeoutMS) !MaxEvents !*env -> *env | containsAIOState env
tick timeout maxEvents env
	# env = processIdle env
	# fdArr = createArray maxEvents -1
	# evKindsArr = createArray maxEvents -1
	# (aioState=:{AIOState|aioFd},env) = getAIOState env
	# (mbNEvents, env) = ioGetEvents aioFd timeout maxEvents fdArr evKindsArr env
	| isError mbNEvents = printOSError (fromError mbNEvents) env
	# nEvents = fromOk mbNEvents
	= ioProcessEvents nEvents fdArr evKindsArr env

closeAllConnections :: !*env -> *env | containsAIOState env
closeAllConnections env
	# ({AIOState|clients, listeners}, env) = getAIOState env
	= foldr (\cId e -> cleanupSocket cId e) env ('Data.Map'.keys clients)

processIdle :: !*env -> *env | containsAIOState env
processIdle env
	= processClients env

ioProcessEvents :: !Int !{#Int} !{#Int} !*env -> *env | containsAIOState env
ioProcessEvents nEvents fdArr evArr env
	# fds = [fd \\ fd <-: 'Data.Array'.takeArr nEvents fdArr]
	# evs = [ev \\ ev <-: 'Data.Array'.takeArr nEvents evArr]
	# connectionEvents = toConnectionEvents fds (toEvents evs)
	= processIO connectionEvents env
where
	toConnectionEvents :: [FD] [IOEvent] -> [ConnectionEvent]
	toConnectionEvents [fd:fds] [e:es] = [{cId=(ConnectionId fd), ev=e}:(toConnectionEvents fds es)]
	toConnectionEvents [] [] = []
	toConnectionEvents x y = abort ("AsyncIO.icl: Error in toConnectionEvents, lengths do not match: " +++ toString (length x) +++ toString (length y) )

processIO :: ![ConnectionEvent] !*env -> *env | containsAIOState env
processIO [] env = env
processIO [connectionEv=:{cId,ev}:connectionEvs] env
	= processSockIO
where
	processSockIO
		# ({AIOState|clients, listeners}, env) = getAIOState env
		| isNone ('Data.Map'.get cId listeners) && isNone ('Data.Map'.get cId clients) = env
		# env = processEvent connectionEv env
		= processIO connectionEvs env

processEvent :: !ConnectionEvent !*env -> *env | containsAIOState env
processEvent {cId, ev=AcceptEventSock} env
	# (aioState=:{AIOState|listeners, writeQueues}, env) = getAIOState env
	# listener=:{Listener|handlers={ConnectionHandlers|onConnect}} = 'Data.Map'.find cId listeners
	# (mbErr, env) = acceptConnection cId env
	| isError mbErr
		# err = fromError mbErr
		# errCode = fst err
		# conReqWasAborted = errCode == 0
		= if (conReqWasAborted) env (printOSError err env)
	# (clientId, ip) = fromOk mbErr
	# (out, close, env) = onConnect clientId (IPAddr (toString o toDottedDecimal $ ip)) env
	# env = writeData clientId out env
	| close = closeConnection clientId env
	= env

processEvent {cId, ev=ConnectEventSock} env
	# (mbErr, env) = IF_WINDOWS (windowsReadSock cId env) (Ok (), env)
	| isError mbErr
		# env = cleanupSocket cId env
		= printOSError (fromError mbErr) env
	# (aioState=:{AIOState|clients}, env) = getAIOState env
	| isNone ('Data.Map'.get cId clients) = printStdErr_n "ConnectEventSock, client not found" env
	# client=:{Client|mbIpAddr, handlers={ConnectionHandlers|onConnect}} = 'Data.Map'.find cId clients
	# (out, close, env) = onConnect cId (fromJust mbIpAddr) env
	# env = writeData cId out env
	# (aioState=:{AIOState|clients}, env) = getAIOState env
	# clients = 'Data.Map'.put cId {client & connected = True} clients
	# aioState = {aioState & clients = clients}
	# env = updAIOState aioState env
	| close = closeConnection cId env
	= env

processEvent readEv=:{cId=ConnectionId fd, ev=ReadEventSock} env
	# cId = readEv.cId
	# ({AIOState|aioFd, writeQueues}, env) = getAIOState env
	# (closed, env) = isConnectionBeingClosed cId env
	| closed = env
	# (mbErr, env) = retrieveData aioFd cId env
	| isError mbErr
		# error=:(errCode, errMsg) = fromError mbErr
		# receiveBufferIsEmpty = IF_WINDOWS False (errCode == 11 || errCode == 35) // EAGAIN OR EWOULDBLOCK (POSIX)
		= if (not receiveBufferIsEmpty) (printOSError error env) env
	# (disconnected, data) = fromOk mbErr
	| disconnected
		# env = IF_WINDOWS (processData data env) env
		= cleanupSocket cId env
	# (mbErr, env) = IF_WINDOWS (windowsReadSock cId env) (Ok (), env)
	| isError mbErr
		# env = IF_WINDOWS (processData data env) env
		# env = cleanupSocket cId env
		= printOSError (fromError mbErr) env
	# env = processData data env
	= IF_WINDOWS env (processEvent readEv env)
where
	processData data env
		# cId = readEv.cId
		# (aioState=:{AIOState|writeQueues, clients}, env) = getAIOState env
		| isNone ('Data.Map'.get cId clients) = printStdErr_n "ReadEventSock: could not find client." env
		# {Client|handlers={ConnectionHandlers|onData}} = 'Data.Map'.find cId clients
		# (out, close, env) = onData cId data env
		# env = writeData cId out env
		| close
			= closeConnection cId env
		= env

processEvent {cId, ev=WriteEventSock} env
	# ({AIOState|clients}, env) = getAIOState env
	# mbClient = 'Data.Map'.get cId clients
	| isNone mbClient = env
	# {Client|closeAfterWrite} = fromJust mbClient
	| closeAfterWrite = cleanupSocket cId env
	= env

processEvent {cId, ev=WriteNopEventSock} env
	= env

// This is a Linux only event. Could go wrong since socket may be both readable and writable yet WriteEventNop should be returned/processed.
processEvent {cId, ev=ReadAndWriteEventSock} env
	# env = processEvent {ConnectionEvent|cId = cId, ev = ReadEventSock} env
	= processEvent {ConnectionEvent|cId = cId, ev = WriteEventSock} env

processEvent {cId, ev=DisconnectEventSock} env
	# env = IF_WINDOWS env (processEvent {ConnectionEvent|cId = cId, ev = ReadEventSock} env)
	= cleanupSocket cId env

writeData :: !ConnectionId ![OutputData] !*env -> *env | containsAIOState env
writeData cId out env
	# (aioState=:{AIOState|aioFd}, env) = getAIOState env
	# (mbErr, env) = IF_WINDOWS (windowsIncPacketsToWrite cId (length out) env) (Ok (), env)
	| isError mbErr = printOSError (fromError mbErr) env
	# (mbErr, env)
		= foldl (\(mbErr, env) msg -> if (isError mbErr) (mbErr, env) (queueWriteSock aioFd cId msg env))
			(Ok (), env) out
	| isError mbErr = printOSError (fromError mbErr) env
	= env

addToWriteMap :: ![String] !(?(![String], !Bool)) -> (?(![String], !Bool))
addToWriteMap [] x = x
addToWriteMap toAdd ?None = ?Just (toAdd, False)
addToWriteMap toAdd (?Just (existing, closed)) = ?Just (existing ++ toAdd, closed)

toEvents :: ![Int] -> [IOEvent]
toEvents evs = 'Data.List'.map toEvent evs
where
	toEvent 0  = AcceptEventSock
	toEvent 1  = ConnectEventSock
	toEvent 2  = ReadEventSock
	toEvent 4  = WriteEventSock
	toEvent 6  = ReadAndWriteEventSock
	toEvent 8  = DisconnectEventSock
	toEvent 10 = WriteNopEventSock

isConnectionBeingClosed :: !ConnectionId !*env -> (!Bool, !*env) | containsAIOState env
isConnectionBeingClosed cId env
	# ({AIOState|clients, writeQueues}, env) = getAIOState env
	# mbClient = 'Data.Map'.get cId clients
	| isNone mbClient = (True, env)
	# {Client|closeAfterWrite, evaluateHandlers} = fromJust mbClient
	= (closeAfterWrite || not evaluateHandlers, env)

closeConnection :: !ConnectionId !*env -> *env | containsAIOState env
closeConnection cId env
	# (mbAnyPendingPackets, env) = anyPendingPackets cId env
	| isError mbAnyPendingPackets = printOSError (fromError mbAnyPendingPackets) env
	# anyPendingPackets = fromOk mbAnyPendingPackets
	| not anyPendingPackets = cleanupSocket cId env
	# (aioState=:{AIOState|clients}, env) = getAIOState env
	# clients = 'Data.Map'.alter (\mbClient -> mbClient >>= \client -> pure {client & evaluateHandlers = False}) cId clients
	# aioState & clients = clients
	= updAIOState aioState env

cleanupSocket :: !ConnectionId !*env -> *env | containsAIOState env
cleanupSocket cId env
	# (aioState=:{AIOState|clients}, env) = getAIOState env
	// Disconnect being detected by ioGetEvents may lead to a read which may detect a disconnect as well.
	# alreadyCleanedUp = not ('Data.Map'.member cId clients)
	| alreadyCleanedUp = env
	# {Client|handlers={ConnectionHandlers|onDisconnect}} = 'Data.Map'.find cId clients
	# env = onDisconnect cId env
	# (aioState=:{AIOState|clients, aioFd}, env) = getAIOState env
	# clients = 'Data.Map'.del cId clients
	# aioState = {aioState & clients = clients}
	# env = updAIOState aioState env
	# (mbErr, env) = cleanupFd aioFd cId Socket env
	| isError mbErr = printStdErr_n "cleanupSocket: cleanupFd failed." env
	= env

acceptConnection :: !ConnectionId
                    !*env
                    -> (!MaybeOSError (ConnectionId, Int), !*env) | containsAIOState env
acceptConnection cId env
	# (aioState=:{AIOState|aioFd, listeners, clients}, env) = getAIOState env
	# mbListener = 'Data.Map'.get cId listeners
	| isNone mbListener = (Error (-1, "AsyncIO: AcceptEventSock, no listener found."), env)
	# listener=:{Listener|handlers, removeOnClose, mbLastClient} = fromJust mbListener
	// Accept connection.
	# (mbErr,env) = accept aioFd cId env
	| isError mbErr
		# (errCode, errMsg) = fromError mbErr
		| errCode == -2 = (Error(0, "Connection request was aborted."), env) // Connection request was aborted, nothing should be done.
		= getLastOSError env
	#! (os, newClientId) = fromOk mbErr
	# connectedClientId = case os of
		Posix = newClientId
		Windows = fromJust mbLastClient
	// Initialize read on Windows
	# (mbErr, env) = IF_WINDOWS (windowsReadSock connectedClientId env) (Ok (), env)
	| isError mbErr
		# env = cleanupSocket cId env
		= (Error (-1, "AsyncIO: AcceptEventSock, read failed"), printOSError (fromError mbErr) env)
	// Retrieve IP addr for peer.
	# (mbErr,env) = getpeername connectedClientId cId env
	| isError mbErr = getLastOSError env
	# ip = fromOk mbErr
	// Deal with State.
	# client = {Client|mbIpAddr = ?None, handlers = listener.Listener.handlers, connected = True
                   , closeAfterWrite = False, evaluateHandlers = True, removeOnClose = removeOnClose}
	# clients  = 'Data.Map'.put connectedClientId client clients
	# listeners = 'Data.Map'.put cId {listener & mbLastClient = ?Just newClientId} listeners
	# aioState = {aioState & clients = clients, listeners = listeners}
	# env = updAIOState aioState env
	= (Ok (connectedClientId, ip), env)

processClients :: !*env -> *env | containsAIOState env
processClients env
	# ({AIOState|clients}, env) = getAIOState env
	= 'Data.Map'.foldrWithKey` processClient env clients
where
	processClient :: !ConnectionId (Client *env) *env -> *env | containsAIOState env
	processClient clientId client=:{Client|connected, handlers={ConnectionHandlers|onTick}} env
		| not connected = env
		# (aioState=:{aioFd}, env) = getAIOState env
		# (mbErr, env) = signalWriteSock aioFd clientId env
		# (closed, env) = isConnectionBeingClosed clientId env
		| closed
			# (aioState=:{AIOState|clients, aioFd}, env) = getAIOState env
			# clients = 'Data.Map'.put clientId {Client|client & closeAfterWrite = True} clients
			# aioState = {aioState & clients = clients}
			= updAIOState aioState env
		# (out, close, env) = onTick clientId env
		# env = writeData clientId out env
		| close
			# env = closeConnection clientId env
			# (aioState=:{clients}, env) = getAIOState env
			# clients
				= 'Data.Map'.alter
					(\mbClient -> mbClient >>= \client -> pure {client & closeAfterWrite = True})
					clientId clients
			# aioState = {aioState & clients = clients}
			= updAIOState aioState env
		# (aioState=:{aioFd}, env) = getAIOState env
		= env

listenerEvents :: [IOEvent]
listenerEvents = [AcceptEventSock]

ioInit :: !*env -> (!MaybeOSError AsyncIOFD, !*env)
ioInit env = 'System._AsyncIO'.ioInit env

ioGetEvents :: !AsyncIOFD !(?TimeoutMS) !MaxEvents !{#Int} !{#Int} !*env -> (!MaybeOSError Int, !*env)
ioGetEvents asyncIoFd timeout maxEvents fdList evKinds env = 'System._AsyncIO'.ioGetEvents asyncIoFd timeout maxEvents fdList evKinds env

windowsAccept :: !AsyncIOFD !ConnectionId !*env -> (!MaybeOSError (OS,ConnectionId), !*env)
windowsAccept mainFD (ConnectionId listenFd) env
	# (mbOsClient, env) = 'System._AsyncIO'.windowsAccept mainFD listenFd env
	= (fmap (\(os, clientFd) -> (os, ConnectionId clientFd)) mbOsClient, env)

accept :: !AsyncIOFD !ConnectionId !*env -> (!MaybeOSError (OS,ConnectionId), !*env)
accept mainFD (ConnectionId listenFd) env
	# (mbOsClient, env) = 'System._AsyncIO'.accept mainFD listenFd env
	= (fmap (\(os, clientFd) -> (os, ConnectionId clientFd)) mbOsClient, env)

createTCPListener :: !AsyncIOFD !Port !*env -> (!MaybeOSError ConnectionId,!*env)
createTCPListener asyncIoFd port env
	# (mbListener, env) = 'System._AsyncIO'.createTCPListener asyncIoFd port env
	= (fmap (\listenFd -> ConnectionId listenFd) mbListener, env)

connect :: !AsyncIOFD !Int !Port !*env -> (!MaybeOSError ConnectionId, !*env)
connect asyncIoFd ip port env
	# (mbClient, env) = 'System._AsyncIO'.connect asyncIoFd ip port env
	= (fmap (\clientFd -> ConnectionId clientFd) mbClient, env)

queueWriteSock :: !AsyncIOFD !ConnectionId !String !*env -> (!MaybeOSError (), !*env)
queueWriteSock aioFd (ConnectionId fd) data env = 'System._AsyncIO'.queueWriteSock aioFd fd data env

signalWriteSock :: !AsyncIOFD !ConnectionId !*env -> (!MaybeOSError (), !*env)
signalWriteSock asyncIoFd (ConnectionId fd) env = 'System._AsyncIO'.signalWriteSock asyncIoFd fd env

getpeername :: !ConnectionId !ConnectionId !*env -> (!MaybeOSError Int, !*env)
getpeername (ConnectionId clientFd) (ConnectionId listenFd) env = 'System._AsyncIO'.getpeername clientFd listenFd env

retrieveData :: !AsyncIOFD !ConnectionId !*env -> (!MaybeOSError (Bool, String), !*env)
retrieveData asyncIoFd (ConnectionId fd) env = 'System._AsyncIO'.retrieveData asyncIoFd fd env

windowsReadSock :: !ConnectionId !*env -> (!MaybeOSError (), !*env)
windowsReadSock (ConnectionId fd) env = 'System._AsyncIO'.windowsReadSock fd env

cleanupFd :: !AsyncIOFD !ConnectionId !ConnectionType !*env -> (!MaybeOSError (), !*env)
cleanupFd asyncIoFd (ConnectionId fd) cType env = 'System._AsyncIO'.cleanupFd asyncIoFd fd cType env

windowsIncPacketsToWrite :: !ConnectionId !Int !*env -> (!MaybeOSError (), !*env)
windowsIncPacketsToWrite (ConnectionId fd) numPackets env = 'System._AsyncIO'.windowsIncPacketsToWrite fd numPackets env

anyPendingPackets :: !ConnectionId !*env -> (!MaybeOSError Bool, !*env)
anyPendingPackets (ConnectionId fd) env = 'System._AsyncIO'.anyPendingPackets fd env

// TODO: Should be in System.OSError or some other module.
printOSError :: (!OSErrorCode, !OSErrorMessage) !*env -> *env
printOSError (errCode, errMsg) env = printStdErr_n ("OS error occurred, code: " +++ toString errCode +++ " message: " +++ errMsg) env

// TODO: Should be in Data.Error or some other module.
// Currently, showErr is part of iTasks.Util but this function should be usable outside of iTasks.
printStdErr_n :: !String !*env -> *env
printStdErr_n s env
	#! console          = fwrites (s +++ "\n") stderr
	# (_, env)          = (fclose_ console, env)
	= env
	where
	fclose_ :: !*File -> Bool
	fclose_ f = code inline {
			.d 0 2 f
				jsr	closeF
			.o 0 1 b
		}
