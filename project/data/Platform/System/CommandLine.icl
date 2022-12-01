implementation module System.CommandLine

import StdInt, StdList, StdEnum
import System._Pointer, System.OS

getCommandLine :: !*World -> (![String],!*World)
getCommandLine world
	# argc = readInt4Z global_argc 0
	# argv = derefInt global_argv
	= ([derefString (readInt argv (i << (IF_INT_64_OR_32 3 2)) ) \\ i <- [0..argc - 1]], world)
where
	//The pushLc ABC instruction should work on all platforms / architectures
	//Since it does not always work properly we use a fallback to pushL in some cases
	//Fallback currently neccessary on:	
	// - 64 bit windows, 

	//Global argc pointer
	global_argc :: Pointer
	global_argc = IF_POSIX_OR_WINDOWS global_argclc (IF_INT_64_OR_32 global_argcl global_argclc)
	
	global_argclc :: Pointer
	global_argclc = code {
		pushLc global_argc
	}
	global_argcl :: Pointer
	global_argcl = code {
		pushL global_argc
	}
	//Global argv pointer
	global_argv :: Pointer
	global_argv = IF_POSIX_OR_WINDOWS global_argvlc (IF_INT_64_OR_32 global_argvl global_argvlc)
	
	global_argvlc :: Pointer
	global_argvlc = code {
		pushLc global_argv
	}
	
	global_argvl :: Pointer
	global_argvl = code {
		pushL global_argv
	}

setReturnCode :: !Int !*World -> *World
setReturnCode i world = IF_INT_64_OR_32 (set_return_code_world_64 i world) (set_return_code_world_32 i world)
where
	set_return_code_world_64 :: !Int !*World -> *World
	set_return_code_world_64 i world = code {
		pushI 0xffffffff
		and%
		pushLc return_code
		:xxx
	| assume 4 byte aligned, little endian
	| i<<32 if 8 byte misaligned
		pushI 3
		push_b 1
		pushI 4
		and%
		shiftl%
		push_b 2
		shiftl%
		update_b 0 2
		pop_b 1
	| pointer and not 4
		pushI -5
		and%
	| or 8 bytes
		pushI -8
		addI
		push_b_a 0
		pop_b 1
		pushI_a 0
		or%
		fill1_r _ 0 1 0 01
	.keep 0 2
		fill_a 1 2
		pop_a 2
	}
	
	set_return_code_world_32 :: !Int !*World -> *World
	set_return_code_world_32 i world = code {
		pushI -4
		pushLc return_code
		addI
		:xxx
		push_b_a 0
		pop_b 1
		fill1_r _ 0 1 0 01
	.keep 0 2
		fill_a 1 2
		pop_a 2
	}
