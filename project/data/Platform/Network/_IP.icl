implementation module Network._IP

import StdEnv
import System._Pointer
import Text

_lookupIPAddress :: !String !*World -> (!?Int, !*World)
_lookupIPAddress name world
	# (ptrhe,world) = gethostbynameC (packString name) world
	| ptrhe == 0 = (?None, world)
	# ptrli = readInt ptrhe (IF_INT_64_OR_32 24 16)
	# ptrad = readInt ptrli 0
	# addr = readInt4Z ptrad 0
	= (?Just addr, world)
	where
		gethostbynameC :: !{#Char} !*World -> (!Pointer, !*World)
		gethostbynameC a0 a1 = code {
			ccall gethostbyname "s:p:p"
		}
