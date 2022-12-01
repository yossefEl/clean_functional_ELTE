implementation module System._Time

import StdEnv
import Text
import System.OSError
import System.Time
import System._Pointer
import System._Posix

_timegm :: !{#Int} -> Int
_timegm tm = timegm tm

packTimespec :: !Timespec -> {#Int}
packTimespec ts = {#ts.tv_sec, ts.tv_nsec}

unpackTimespec :: !{#Int} -> Timespec
unpackTimespec a = {Timespec | tv_sec=a.[0], tv_nsec=a.[1]}

_nsTime :: !*World -> (!Timespec, !*World)
_nsTime w
# buf = {#0, 0}
# (r, w) = clock_gettime` 0 buf w
| r <> 0
	# (err, w) = getLastOSError w
	// No need to handle Ok case as getLastOSError always returns Error.
	# (errno, errmsg) = fromError err
	= abort (concat4 "clock_gettime failed, errno: " (toString errno) " error message: " errmsg)
= (unpackTimespec buf, w)

EINTR :== 0x04

_tsSleep :: !Timespec !*World -> (!MaybeOSError (), !*World)
_tsSleep req w
	# req = packTimespec req
	# (r, w) = nanosleep req 0 w
	| r <> 0
		= case getLastOSError w of
			(Error (EINTR, ""), w) = (Ok (), w)
			err = err
	= (Ok (), w)
where
	nanosleep :: !{#Int} !Pointer !*e -> (!Int, !*e)
	nanosleep req rem w = code {
			ccall nanosleep "Ap:I:A"
		}
