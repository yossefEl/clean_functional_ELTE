implementation module System._Posix

import StdEnv
import System.OS
import System.OSError
import System.Time
import System._Pointer
import System._Architecture

errno :: !*w -> (!Int,!*w)
errno world = (getErrno,world)
where
	getErrno :: Int
	getErrno = readInt4S (IF_MAC errnoAddr_mac (IF_ANDROID errnoAddr_android errnoAddr_linux)) 0

	errnoAddr_android :: Pointer
	errnoAddr_android = code {
		ccall __errno ":p"
	}
	errnoAddr_linux :: Pointer
	errnoAddr_linux = code {
		ccall __errno_location ":p"
	}
	errnoAddr_mac :: Pointer
	errnoAddr_mac = code {
		ccall __error ":p"
	}

strerr :: !Int -> Pointer
strerr world = code {
	ccall strerror "I:p"
}

stat :: !{#Char} !{#Char} !*w -> (!Int,!*w)
stat path buf world = IF_MAC (stat_mac path buf world) (stat_linux path buf world)
where
	stat_linux :: !{#Char} !{#Char} !*w -> (!Int,!*w)
	stat_linux path buf world = code {
		ccall stat "ss:I:A"
	}
	stat_mac :: !{#Char} !{#Char} !*w -> (!Int,!*w)
	stat_mac path buf world = code {
		ccall stat$INODE64 "ss:I:A"
	}

unlink :: !{#Char} !*w -> (!Int,!*w)
unlink path world = code {
	ccall unlink "s:I:A"
}
fork :: !*w -> (!Int,!*w)
fork world = code {
	ccall fork ":I:A"
}
execvp :: !{#Char} !{#Pointer} !*w -> (!Int,!*w)
execvp name argv world = code {
	ccall execvp "sA:I:A"
}
waitpid :: !Int !{#Int} !Int !*w -> (!Int,!*w)
waitpid pid status_p options world = code {
    ccall waitpid "IAI:I:A"
}
exit :: !Int !*w -> (!.a,!*w)
exit num world = code {
	ccall exit "I:p:A"
}
getcwd :: !{#Char} !Int !*w -> (!Pointer,!*w)
getcwd buf size_t world = code {
	ccall getcwd "sI:p:A"
}
chdir :: !{#Char} !*w -> (!Int,!*w)
chdir name world = code {
	ccall chdir "s:I:A"
}
mkdir :: !{#Char} !Int !*w -> (!Int,!*w)
mkdir name mode world = code {
	ccall mkdir "sI:I:A"
}
rmdir :: !{#Char} !*w -> (!Int,!*w)
rmdir name world = code {
	ccall rmdir "s:I:A"
}
rename :: !{#Char} !{#Char} !*w -> (!Int,!*w)
rename old new world = code {
	ccall rename "ss:I:A"
}
opendir	:: !{#Char} !*w -> (!Pointer,!*w)
opendir path world = code {
	ccall opendir "s:p:A"
}
closedir :: !Pointer !*w -> (!Int,!*w)
closedir dir world = code {
	ccall closedir "p:I:A"
}
readdir	:: !Pointer !*w -> (!Pointer,!*w)
readdir dir world = code {
	ccall readdir "p:p:A"
}
pipe :: !Pointer !*w -> (!Int, !*w)
pipe arr world = code {
    ccall pipe "p:I:A"
}
posix_openpt :: !Int !*w -> (!Int, !*w)
posix_openpt flags w = IF_ANDROID (android_unimplemented "posix_openpt") (posix_openpt flags w)
where
	posix_openpt :: !Int !*w -> (!Int, !*w)
	posix_openpt _ _ = code {
		ccall posix_openpt "I:I:A"
	}
grantpt :: !Int !*w -> (!Int, !*w)
grantpt fp w = IF_ANDROID (android_unimplemented "grantpt") (grantpt fp w)
where
	grantpt :: !Int !*w -> (!Int, !*w)
	grantpt _ _ = code {
		ccall grantpt "I:I:A"
	}
unlockpt :: !Int !*w -> (!Int, !*w)
unlockpt fp w = IF_ANDROID (android_unimplemented "unlockpt") (unlockpt fp w)
where
	unlockpt :: !Int !*w -> (!Int, !*w)
	unlockpt _ _ = code {
		ccall unlockpt "I:I:A"
	}
ptsname :: !Int !*w -> (!Pointer, !*w)
ptsname fp w = IF_ANDROID (android_unimplemented "ptsname") (ptsname fp w)
where
	ptsname :: !Int !*w -> (!Pointer, !*w)
	ptsname _ _ = code {
		ccall ptsname "I:p:A"
	}
open :: !Pointer !Int !*w -> (!Int, !*w)
open p flags w = IF_ANDROID (android_unimplemented "open") (open p flags w)
where
	open :: !Pointer !Int !*w -> (!Int, !*w)
	open _ _ _ = code {
		ccall open "pI:I:A"
	}
//Special open for strings to save an allocation
opens :: !{#Char} !Int !*w -> (!Int, !*w)
opens p flags w = IF_ANDROID (android_unimplemented "opens") (open p flags w)
where
	open :: !{#Char} !Int !*w -> (!Int, !*w)
	open _ _ _ = code {
			ccall open "sI:I:A"
		}
tcgetattr :: !Int !Pointer !*w -> (!Int, !*w)
tcgetattr fp f w = IF_ANDROID (android_unimplemented "tcgetattr") (tcgetattr fp f w)
where
	tcgetattr :: !Int !Pointer !*w -> (!Int, !*w)
	tcgetattr _ _ _ = code {
		ccall tcgetattr "Ip:I:A"
	}
cfmakeraw :: !Pointer !*w -> *w
cfmakeraw p w = IF_ANDROID (android_unimplemented "cfmakeraw") (cfmakeraw p w)
where
	cfmakeraw :: !Pointer !*w -> *w
	cfmakeraw _ _ = code {
		ccall cfmakeraw "p:V:A"
	}
tcsetattr :: !Int !Int !Pointer !*w -> (!Int, !*w)
tcsetattr fp strategy p w = IF_ANDROID (android_unimplemented "tcsetattr") (tcsetattr fp strategy p w)
where
	tcsetattr :: !Int !Int !Pointer !*w -> (!Int, !*w)
	tcsetattr _ _ _ _ = code {
		ccall tcsetattr "IIp:I:A"
	}
setsid :: !*w -> *w
setsid w = IF_ANDROID (android_unimplemented "setsid") (setsid w)
where
	setsid :: !*w -> *w
	setsid w = code {
		ccall setsid ":V:A"
	}
dup2 :: !Int !Int !*w -> (!Int, !*w)
dup2 old new world = code {
    ccall dup2 "II:I:A"
}
close :: !Int !*w -> (!Int, !*w)
close fd world = code {
    ccall close "I:I:A"
}

ioctl :: !Int !Int !Pointer !*w -> (!Int, !*w)
ioctl fd op ptr world = code {
    ccall ioctl "IIp:I:A"
}

fcntlArg :: !Int !Int !Int !*w -> (!Int, !*w)
fcntlArg fd op arg world = code {
    ccall fcntl "III:I:A"
}

read :: !Int !Pointer !Int !*w -> (!Int, !*w)
read fd buffer nBuffer world = code {
    ccall read "IpI:I:A"
}

write :: !Int !{#Char} !Int !*w -> (!Int, !*w)
write fd buffer nBuffer world = code {
    ccall write "IsI:I:A"
}

select_ :: !Int !Pointer !Pointer !Pointer !Pointer !*w -> (!Int, !*w)
select_ nfds readfds writefds exceptfds timeout world = code {
    ccall select "Ipppp:I:A"
}

kill :: !Int !Int !*w -> (!Int, !*w)
kill pid sig world = code {
    ccall kill "II:I:A"
}

timegm :: !{#Int} -> Int
timegm tm = IF_ANDROID (android_unimplemented "timegm") (timegm tm)
where
	timegm :: !{#Int} -> Int
	timegm tm = code {
		ccall timegm "A:I"
	}

malloc :: !Int -> Pointer
malloc num = code {
	ccall malloc "I:p"
}
mallocSt :: !Int !*w -> (!Pointer, !*w)
mallocSt num w = code {
	ccall malloc "I:p:A"
}
free :: !Pointer -> Int
free ptr = code {
	ccall free "p:I"
}
freeSt :: !Pointer !*w -> *w
freeSt ptr world = code {
   ccall free "p:V:A"
}
memcpy_string_to_pointer :: !Pointer !{#Char} !Int -> Pointer
memcpy_string_to_pointer p s n = code {
    ccall memcpy "psp:p"
}
clock_gettime :: !Int !Pointer !*w -> (!Int, !*w)
clock_gettime id res w = IF_ANDROID (android_unimplemented "clock_gettime") (clock_gettime id res w)
where
	clock_gettime :: !Int !Pointer !*w -> (!Int, !*w)
	clock_gettime _ _ _ = code {
		ccall clock_gettime "Ip:I:A"
	}
clock_gettime` :: !Int !{#Int} !*w -> (!Int, !*w)
clock_gettime` id res w = IF_ANDROID (android_unimplemented "clock_gettime") (clock_gettime id res w)
where
	clock_gettime :: !Int !{#Int} !*w -> (!Int, !*w)
	clock_gettime _ _ _ = code {
		ccall clock_gettime "IA:I:A"
	}

//Mapping to/from byte arrays
unpackStat :: !{#Char} -> Stat
unpackStat s = IF_MAC mac (IF_ANDROID android (IF_ARM linux_arm64 linux))
where
	linux =
		{ st_dev       = IF_INT_64_OR_32 (unpackInt8  s 0)  (unpackInt4S s 0 /* 8 bytes */)
		, st_ino       = IF_INT_64_OR_32 (unpackInt8  s 8)  (unpackInt4S s 12)
		, st_mode      = IF_INT_64_OR_32 (unpackInt4S s 24) (unpackInt4S s 16)
		, st_nlink     = IF_INT_64_OR_32 (unpackInt8  s 16) (unpackInt4S s 20)
		, st_uid       = IF_INT_64_OR_32 (unpackInt4S s 28) (unpackInt4S s 24)
		, st_gid       = IF_INT_64_OR_32 (unpackInt4S s 32) (unpackInt4S s 28)
		, st_rdev      = IF_INT_64_OR_32 (unpackInt8  s 40) (unpackInt4S s 32 /* 8 bytes */)
		, st_size      = IF_INT_64_OR_32 (unpackInt8  s 48) (unpackInt4S s 44)
		, st_blocks    = IF_INT_64_OR_32 (unpackInt8  s 64) (unpackInt4S s 52)
		, st_blksize   = IF_INT_64_OR_32 (unpackInt8  s 56) (unpackInt4S s 48)
		, st_atimespec = IF_INT_64_OR_32
			{tv_sec=unpackInt8  s 72, tv_nsec=unpackInt8  s 80} /* 16 bytes */
			{tv_sec=unpackInt4S s 56, tv_nsec=unpackInt4S s 60} /* 8 bytes */
		, st_mtimespec = IF_INT_64_OR_32
			{tv_sec=unpackInt8  s 88, tv_nsec=unpackInt8  s 96} /* 16 bytes */
			{tv_sec=unpackInt4S s 64, tv_nsec=unpackInt4S s 68} /* 8 bytes */
		, st_ctimespec = IF_INT_64_OR_32
			{tv_sec=unpackInt8  s 104, tv_nsec=unpackInt8  s 112} /* 16 bytes */
			{tv_sec=unpackInt4S s 72,  tv_nsec=unpackInt4S s 76}  /* 8 bytes */
		, st_birthtimespec = {tv_sec=0, tv_nsec=0} // unused
		, st_flags     = 0 // unused
		, st_gen       = 0 // unused
		}
	linux_arm64 =
		{ st_dev       = unpackInt8  s 0
		, st_ino       = unpackInt8  s 8
		, st_mode      = unpackInt4S s 16
		, st_nlink     = unpackInt4S s 20
		, st_uid       = unpackInt4S s 24
		, st_gid       = unpackInt4S s 28
		, st_rdev      = unpackInt8  s 32
		, st_size      = unpackInt8  s 48
		, st_blksize   = unpackInt8  s 56
		, st_blocks    = unpackInt8  s 64
		, st_atimespec = {tv_sec=unpackInt8  s 72, tv_nsec=unpackInt8  s 80}
		, st_mtimespec = {tv_sec=unpackInt8  s 88, tv_nsec=unpackInt8  s 96}
		, st_ctimespec = {tv_sec=unpackInt8  s 104, tv_nsec=unpackInt8  s 112}
		, st_birthtimespec = {tv_sec=0, tv_nsec=0} // unused
		, st_flags     = 0 // unused
		, st_gen       = 0 // unused
		}
	mac =
		{ st_dev       = unpackInt4S s 0
		, st_ino       = unpackInt8  s 8
		, st_mode      = unpackInt2S s 4
		, st_nlink     = unpackInt2S s 6
		, st_uid       = unpackInt4S s 16
		, st_gid       = unpackInt4S s 20
		, st_rdev      = unpackInt8  s 24
		, st_size      = unpackInt8  s 96
		, st_blocks    = unpackInt8  s 104
		, st_blksize   = unpackInt4S s 112
		, st_atimespec = {tv_sec=unpackInt8 s 32, tv_nsec=unpackInt8 s 40}
		, st_mtimespec = {tv_sec=unpackInt8 s 48, tv_nsec=unpackInt8 s 56}
		, st_ctimespec = {tv_sec=unpackInt8 s 64, tv_nsec=unpackInt8 s 72}
		, st_birthtimespec = {tv_sec=unpackInt8 s 80, tv_nsec=unpackInt8 s 88}
		, st_flags     = unpackInt4S s 116
		, st_gen       = unpackInt4S s 120
		}
	android =
		{ st_dev       = IF_INT_64_OR_32 (unpackInt8  s 0)  (unpackInt4S s 0 /* 8 bytes */)
		, st_ino       = IF_INT_64_OR_32 (unpackInt8  s 8)  (unpackInt4S s 96)
		, st_mode      = IF_INT_64_OR_32 (unpackInt4S s 24) (unpackInt4S s 16)
		, st_nlink     = IF_INT_64_OR_32 (unpackInt8  s 16) (unpackInt4S s 20)
		, st_uid       = IF_INT_64_OR_32 (unpackInt4S s 28) (unpackInt4S s 24)
		, st_gid       = IF_INT_64_OR_32 (unpackInt4S s 32) (unpackInt4S s 28)
		, st_rdev      = IF_INT_64_OR_32 (unpackInt8  s 40) (unpackInt4S s 32 /* 8 bytes */)
		, st_size      = IF_INT_64_OR_32 (unpackInt8  s 48) (unpackInt4S s 44)
		, st_blocks    = IF_INT_64_OR_32 (unpackInt8  s 64) (unpackInt4S s 64)
		, st_blksize   = IF_INT_64_OR_32 (unpackInt8  s 56) (unpackInt4S s 56)
		, st_atimespec = IF_INT_64_OR_32
			{tv_sec=unpackInt8  s 72, tv_nsec=unpackInt8  s 80} /* 16 bytes */
			{tv_sec=unpackInt4S s 72, tv_nsec=unpackInt4S s 76} /* 8 bytes */
		, st_mtimespec = IF_INT_64_OR_32
			{tv_sec=unpackInt8  s 88, tv_nsec=unpackInt8  s 96} /* 16 bytes */
			{tv_sec=unpackInt4S s 80, tv_nsec=unpackInt4S s 84} /* 8 bytes */
		, st_ctimespec = IF_INT_64_OR_32
			{tv_sec=unpackInt8  s 104, tv_nsec=unpackInt8  s 112} /* 16 bytes */
			{tv_sec=unpackInt4S s 88,  tv_nsec=unpackInt4S s 92}  /* 8 bytes */
		, st_birthtimespec = {tv_sec=0, tv_nsec=0} // unused
		, st_flags     = 0 // unused
		, st_gen       = 0 // unused
		}

sizeOfStat :: Int
sizeOfStat = IF_ANDROID 104 (IF_INT_64_OR_32 144 88)

pipeBufferSize :: !Int !*w -> (!MaybeOSError Int, !*w)
pipeBufferSize fd world
	| IF_ANDROID True False = android_unimplemented "pipeBufferSize"
	# (res, world) = isatty fd world
	| res == -1 = getLastOSError world
	| res == 1 = (Ok 65336, world)
	# (ptr, world) = mallocSt 4 world
	| ptr == 0 = getLastOSError world
	# (res, world) = ioctl fd FIONREAD ptr world
	| res == -1 = getLastOSError (freeSt ptr world)
	# (bufsize, ptr) = readP (\p->readInt4Z p 0) ptr
	= (Ok bufsize, freeSt ptr world)
where
	isatty fd w = IF_MAC (isattyC fd w) (0, w)
	isattyC :: !Int !*w -> (!Int, !*w)
	isattyC _ _ = code {
		ccall isatty "I:I:A"
	}

android_unimplemented :: !String -> .a
android_unimplemented name = abort ("System._Posix: "+++name+++" is not implemented on Android\n")
