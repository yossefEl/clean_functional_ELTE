implementation module System._Environment

import StdOverloaded, StdInt
import System._Pointer

_getEnvironmentVariable :: !String !*World -> (?String, *World)
_getEnvironmentVariable name world
	# ptr = getenvC (packString name)
	| ptr == 0
		= (?None, world)
		= (?Just (derefString ptr), world)
	where
		getenvC :: !{#Char} -> Pointer
		getenvC a0 = code {
			ccall getenv "s:p"
		}

_setEnvironmentVariable :: !String !String !*World -> *World
_setEnvironmentVariable name value world
	# (_,world) = setenvC (packString name) (packString value) 1 world
	= world
	where
		setenvC :: !{#Char} !{#Char} !Int !*World -> (!Int, !*World)
		setenvC a0 a1 a2 a3 = code {
			ccall setenv "ssI:I:A"
		}

_unsetEnvironmentVariable :: !String !*World -> *World
_unsetEnvironmentVariable name world
	# (_,world) = unsetenvC (packString name) world
	= world
	where
		unsetenvC :: !{#Char} !*World -> (!Int, !*World)
		unsetenvC a0 a1 = code {
			ccall unsetenv "s:I:A"
		}
