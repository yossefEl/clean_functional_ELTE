definition module System.AsyncIO.AIOWorld

from System.AsyncIO import :: AIOState, class containsAIOState, class containsWorld
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode
from Data.Error import :: MaybeError

/**
 * This module provides a unique environment which can be used to interact with the System.AsyncIO module.
 */

/*
 * Contains the AIOState alongside the World and a state.
 * The AIOWorld can be used to develop simple programs which use the System.AsyncIO module
 *
 * @var The type of the state which is passed on to the handlers in the AIOState (see AIOState).
 */
:: *AIOWorld st = {aioState :: !AIOState *(AIOWorld st), world :: !*World, state :: !st}

instance containsAIOState (AIOWorld a)
instance containsWorld (AIOWorld a)

/**
 * Creates an AIOWorld with an empty AIOState
 * The first argument passed to createAIOWorld is the default state of the AIOWorld.
 * It can be of any type. This state can be altered as a response to I/O events occurring.
 * (see :: System.AsyncIO.SocketHandlers for more detail)
 *
 * @param The initial state of the AIOWorld
 * @param The World
 * @result Failure: an error
 * @result Success: An AIOWorld
 */
createAIOWorld :: !st !*World -> MaybeOSError (*AIOWorld st)
