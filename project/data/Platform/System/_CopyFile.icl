implementation module System._CopyFile

import StdEnv
import System.OSError

_copyFile :: !String !String !*World -> (!MaybeOSError (), !*World)
_copyFile src dest w
	# (ok, srcF, w) = fopen src FReadData w
	| not ok = getLastOSError w
	# (ok, destF, w) = fopen dest FWriteData w
	| not ok = exitWithCurrentError (snd o fclose srcF) w
	# (srcF, destF) = actuallyCopy srcF destF
	# (ok, w) = fclose srcF w
	| not ok = exitWithCurrentError (snd o fclose destF) w
	# (ok, w) = fclose destF w
	| not ok = getLastOSError w
	= (Ok (), w)
where
	// Exit with the current error but do some things first that may change the OSError
	exitWithCurrentError :: !.(*World -> *World) !*World -> (!MaybeOSError (), !*World)
	exitWithCurrentError cont w = case getLastOSError w of
		(Error e, w) = (Error e, cont w)
		(Ok (), w) = abort "Shouldn't occur"

	actuallyCopy :: !*File !*File -> (!*File, !*File)
	actuallyCopy src dst
		# (end, src) = fend src
		| end = (src, dst)
		# (s, src) = freads src 65536
		# dst = dst <<< s
		= actuallyCopy src dst
