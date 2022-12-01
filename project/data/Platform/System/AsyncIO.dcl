definition module System.AsyncIO

/**
 * This module provides AsyncIO for communication channels.
 * The communication channels are TCP connections (both from a client and server perspective) and pipes or PTYs
 *
 * The module is named AsyncIO since it provides I/O in an asynchronous manner.
 * The user may indicate their wish to perform network I/O, perform certain other operations and start an event loop
 * The user is then notified of I/O events though callback functions
 * that are evaluated at a later stage during program execution.
 *
 * This is an asynchronous way of providing I/O because the user expressed interest in performing I/O at one point in time
 * and then reacts to I/O at another point in time, being able to perform unrelated activities in the meantime.
 *
 * The user makes use of callback functions to react to I/O events occuring
 * (Network I/O connection requests, data being received).
 * The callbacks that may be specified by the user of the module are specified
 * through the ConnectionHandlers type.
 *
 * This module implements TCP functionality (Thought: Maybe this should be provided by a separate module)
 *
 * It is possible to use the functions that are provided by the module through a custom environment
 * by providing instances for the containsAIOState and containsWorld classes for the custom environment.
 */

from Data.Error import :: MaybeError (..), :: MaybeErrorString
from System.OSError import :: MaybeOSError (..), :: OSError (..), :: OSErrorCode, :: OSErrorMessage
from StdClass import class <
from Data.Map import :: Map
import StdOverloaded

/**
 * Creates an empty AIOState.
 *
 * @param The World
 * @result Failure: an error
 * @result Success: The empty AIOState
 * @result The World
 */
emptyAIOState :: !*World -> (!MaybeOSError (*AIOState *env), !*World)

/**
 * Creates a  TCP listener which listens for connections on the provided port.
 * The listener is monitored for I/O, clients may then connect to the listener.
 * The listener commmunicates with the clients through the callback record
 * of type :: ConnectionHandlers that is provided by the user of the module.
 * (see :: ConnectionHandlers for more detail)
 *
 * @param The callback record used to communicate with clients that connect to the TCP listener
 * @param The port to listen on
 * @param A unique environment
 * @result A unique environment
 */
addListener :: !(ConnectionHandlers *env) !Port !*env -> *env | containsAIOState env

/**
 * Connects to a TCP listener.
 * The listener that is connected to may be communicated with using
 * a callback record of :: ConnectionHandlers that is provided by the user of the module.
 * (see :: ConnectionHandlers for more detail)
 *
 * @param The callback record used to communicate with the peer to which a connection was established
 * @param The hostname of the listener that should be connected to
 * @param The port on which the listener is listening
 * @param A unique environment
 * @result The ConnectionId of the client for which a connection is established.
 * @result A unique environment
 */
addConnection :: !(ConnectionHandlers *env) !Hostname !Port !*env
             -> (!MaybeOSError ConnectionId, !*env) | containsAIOState, containsWorld env

/**
 * Performs an event loop tick, which retrieves an processes I/O events that occurred on the communication channels.
 *
 * @param ?None = block indefinitely waiting for an I/O event to occur. ?Just x = block for a maximum of x milliseconds.
 * @param The maximum amount of I/O events to process for each event loop tick, provide 10 if unsure.
 * @param A unique environment
 * @result A unique environment
 */
tick :: !(?TimeoutMS) !MaxEvents !*env -> *env | containsAIOState env

/**
 * Default event loop function that provides sensible default values for loop.
 * For more information, see the loop function.
 *
 * If you do not want the event loop to terminate but are unsure about how many iterations to perform,
 * Use 100 iterations for simple programs that are not supposed to communicate indefinitely.
 *
 * @param Should the event loop terminate? If not: ?None. If yes: ?Just <amount of iterations to perform>
 * @param A unique environment
 * @result A unique environment
 */
defaultLoop :: !(?Int) !*env -> *env | containsAIOState env

/**
 * Event loop function to be used in programs that make use of the AIOWorld environment.
 *
 * The loop function retrieves I/O events and processes I/O events.
 * The processing involves evaluating callback functions that are used to react to network I/O events occurring.
 * Such I/O events may involve receiving data or a connection request occurring.
 * The callback functions are defined by the user of the module (see e.g :: ConnectionHandlers).
 * As a result, the loop function can be considered the "Engine" of the network I/O communication.
 *
 * The first parameter indicates how often the function should recursively call itself
 * if ?None is provided, this function will not terminate and the function will retrieve and process I/O events
 * until the program is terminated by the user.
 * If you do not want the event loop to terminate but are unsure about how many iterations to perform,
 * Use 100 iterations for simple programs that are not supposed to communicate indefinitely.
 *
 * The second parameter specifies the maximum amount of milliseconds an event loop iteration should spend
 * waiting for an I/O event to occur.
 * If an I/O event occurs before the timeout expires, the function will not block any longer
 * and the I/O event is processed immediately. If no event occurs, the function blocks until the timeout expires.
 * If this parameter is ?None, loop will wait indefinitely
 *
 * The third parameter specifies how many I/O events should be dealt with at most during a single event loop iteration.
 * This is added to be able to make sure the processing of I/O events does not take too long and the event loop
 * is regularly evaluated.
 *
 * @param Should the event loop terminate? If not: ?None. If yes: ?Just <amount of iterations to perform>
 * @param Maximum time in milliseconds to block waiting for an I/O event, provide ?None if unsure
 * @param Maximum number of I/O events to retrieve per event loop iteration, provide 10 if unsure
 * @param A unique environment
 * @result A unique nvironment
 */
loop :: !(?Int) !(?TimeoutMS) !Int !*env -> *env | containsAIOState env

/**
 * Closes the connection with the specified ConnectionId.
 *
 * @param ConnectionId of the connection which should be closed
 * @param A unique environment
 * @result A unique environment
 */
closeConnection :: !ConnectionId !*env -> *env | containsAIOState env

/**
 * Returns whether the connection is in the process of being closed.
 *
 * @param The ConnectionId of the connection for which it should be checked if the connection has closed.
 * @param A unique environment
 * @result Is the connection in the process of being closed? Yes: True, No: False
 * @result A unique environment
 */
isConnectionBeingClosed :: !ConnectionId !*env -> (!Bool, !*env) | containsAIOState env

/**
 * Writes output to the communication channel with the specified Connection Id.
 *
 * @param ConnectionId of communication which should be closed
 * @param The output to write over the connection
 * @param A unique environment
 * @result A unique environment
 */
writeData :: !ConnectionId ![OutputData] !*env -> *env | containsAIOState env

/**
 * Closes all communication channels that are currently performing AsyncIO.
 *
 * @param A unique environment
 * @result A unique environment
 */
closeAllConnections :: !*env -> *env | containsAIOState env

/**
 * Contains callback functions for TCP connection channels.
 *
 * @var The type of the environment passed on to the callback functions.
 */
:: ConnectionHandlers *env =
	{/**
	  * This callback is evaluated when a client connects to the listener.
	  *
	  * @param The connectionId argument contains an identifier which identifies the client
	  * @param The IPAddr argument contains the ip address of the client
	  * @param The env argument contains the state, which may be altered by the callback
	  * @result The first return value contains a list of output to be sent back to the client
	  * @result The second return value indicates whether the connection should be closed after writing the output
	  * @result The third return value contains the env, this environment may contain an altered state
	  */
	  onConnect    :: !ConnectionId IPAddr env -> *([OutputData], CloseConnection, env)
     /**
	  * The onData callback is evaluated when data is received.
	  *
	  * @param The connectionId argument contains an identifier which identifies the client
	  * @param The received data is provided as an argument alongside the environment
	  * @param The env argument contains the state, which may be altered by the callback
	  * @result The first return value contains a list of output to be sent back to the client
	  * @result The second return value indicates whether the connection should be closed after writing the output
	  * @result The third return value contains the env, the state located within the env may be altered
	  */
	, onData       :: !ConnectionId InputData env -> *([OutputData], CloseConnection, env)
     /**
	  * The onTick callback is evaluated every event loop iteration.
	  *
	  * @param The connectionId argument contains an identifier which identifies the client
	  * @paramThe env argument contains the state, which may be altered by the callback
	  * @result The first return value contains a list of output to be sent back to the client
	  * @result The second return value indicates whether the connection should be closed after writing the output
	  * @result The third return value contains the env, the state located within the env may be altered.
	  */
	, onTick       :: !ConnectionId env -> *([OutputData], CloseConnection, env)

    /**
	 * The onDisconnect callback is evaluated when the connection to a client was closed.
	 *
	 * @param The connectionId argument contains an identifier which identifies the client
	 * @param The env argument contains the state, which may be altered by the callback
	 * @result The return value is the new environment, which may contain an altered state
	 */
	, onDisconnect :: !ConnectionId env -> env
	}

/**
 * Class used to group types which contain an AIOState.
 *
 * @var The type which contains an AIOState
 */
class containsAIOState env
where
	/**
	 * Retrieves AIOState from env.
	 *
	 * @param The environment
	 * @result The AIOState contained within the environment
	 * @result The environment
	 */
	getAIOState :: !*env -> (!(AIOState *env), !*env)
	/**
	 * Updates AIOState of environment
	 *
	 * @param The AIOState to update the environment with
	 * @param The environment
	 */
	updAIOState :: !(AIOState *env) !*env -> *env

/**
 * Class used to group types which contain a World.
 *
 * @var The type which contains a World.
 */
class containsWorld env
where
	/**
	 * Applies a World function yielding a result and returns the result alongside the environment containing the world.
	 *
	 * @param The World function to apply
	 * @param The environment containing the World
	 * @result The result of the World function
	 * @result The environment containing the updated world
	 */
	accWorld :: !(*World -> (a, *World)) !*env -> (!a, !*env)
	/**
	 * Applies a World function yielding no result and returns the environment containing the World.
	 *
	 * @param The World function to apply
	 * @param The environment containing the world
	 * @result The environment containing the updated world
	 */
	appWorld :: !(*World -> *World) !*env -> *env

/**
 * Contains the state related to the communication channels alongside the provided callback functions.
 *
 * This type should be considered to be an abstract type but the compiler does not allow to make it abstract.
 *
 * @var The type of the environment passed on to the callback functions
 */
:: AIOState *env =
	{ listeners  :: !Map ConnectionId (Listener env) // States of TCP clients which listen for TCP connections.
	, clients    :: !Map ConnectionId (Client env) // States of TCP clients which communicate over TCP.
	, aioFd      :: !AsyncIOFD // Contains reference to I/O multiplexer (used to retrieve I/O events).
	, writeQueues :: !Map ConnectionId ([OutputData], Bool)
		// Stores the data to be sent for a communication channel during an individual event loop iteration.
	}

/*
 * Data record for monitored TCP listener.
 *
 * This type should be considered an abstract type but the compiler does not allow to make it abstract.
 */
:: Listener *env =
	{ mbLastClient  :: !?ConnectionId
		// The connection id of the last client which was created through async Windows accept.
	, handlers      :: !ConnectionHandlers env // Contains callback functions used to react to I/O events.
	, removeOnClose :: !Bool // Whether the connection states of disconnected clients should be removed on disconnect.
	}

/*
 * Data record for monitored TCP client.
 *
 * This type should be considered an abstract type but the compiler does not allow to make it abstract.
 */
:: Client *env =
	{ mbIpAddr         :: !?IPAddr // Used for tcpconnect, onConnect needs to be called with IP.
	, handlers         :: !ConnectionHandlers env // Contains callback functions used to react to I/O events.
	, connected        :: !Bool // Whether client is currently connected (used for Windows async connect, tcpconnect).
	, closeAfterWrite  :: !Bool // Whether client file descriptor should be closed once all pending data has arrived.
	, evaluateHandlers :: !Bool // Whether the Client handlers should be evaluated.
	, removeOnClose    :: !Bool // Whether connection state should be removed on disconnect.
	}

//@representation Uniquely identifies a communication channel.
:: ConnectionId (=: ConnectionId Int)
// String form of an IP Address (e.g 127.0.0.1)
:: IPAddr =: IPAddr String
// Port number of the connection (should be a number >= 0).
:: Port =: Port Int
//Type indicating the type of the communication channel.
:: ConnectionType = Socket
/**
 * @representation Incoming data that is received through a communication channel.
 */
:: InputData :== String
/**
 * @representation Output data that will be sent through a communication channel.
 */
:: OutputData :== String
/**
 * @representation Boolean indicating whether the communication channel should be closed after writing output.
 */
:: CloseConnection :== Bool
/**
 * Hostname, used by addConnection (e.g: localhost, example.org)
 */
:: Hostname :== String
/**
 * @representation File descriptor returned through initializing I/O multiplexing mechanism.
 */
:: AsyncIOFD     :== Int
/**
 * @representation Maximum amount of time in milliseconds to block waiting for an I/O event each event loop tick.
 */
:: TimeoutMS     :== Int
/**
 * Maximum number of I/O events to process each event loop tick.
 */
:: MaxEvents     :== Int

instance < ConnectionId

instance toString ConnectionId

instance == ConnectionId

instance toString Port

instance == Port

instance < Port

instance == ConnectionType

