implementation module System.AsyncIO.AIOWorld

import System.AsyncIO
import Data.Error

instance containsAIOState (AIOWorld st) where
	getAIOState :: !*(AIOWorld st) -> (!AIOState *(AIOWorld st), !*AIOWorld st)
	getAIOState aioworld = (aioworld.aioState, {aioworld & aioState = aioworld.aioState})
	updAIOState :: !(AIOState (AIOWorld st)) !(AIOWorld st) -> AIOWorld st
	updAIOState aioState aioworld = {aioworld & aioState = aioState}

instance containsWorld (AIOWorld st) where
	accWorld :: !(*World -> (a, *World)) !*(AIOWorld st) -> (!a, !*(AIOWorld st))
	accWorld f aioworld
		# (a, world) = f aioworld.world
		= (a, {aioworld & world = world})
	appWorld :: !(*World -> *World) !*(AIOWorld st) -> *(AIOWorld st)
	appWorld f aioworld
		# world = f aioworld.world
		= {aioworld & world = world}

createAIOWorld :: !st !*World -> MaybeOSError (*AIOWorld st)
createAIOWorld s world
	# (mbAioState, world) = emptyAIOState world
	| isError mbAioState = Error (fromError mbAioState)
	# aioState = fromOk mbAioState
	= Ok {AIOWorld|aioState = aioState, world = world, state = s}
