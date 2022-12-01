definition module System._Posix

from StdInt import IF_INT_64_OR_32
from System.OS import IF_MAC, IF_ANDROID
from System.Time import :: Tm, :: Timespec
from System.OSError import :: MaybeOSError, :: MaybeError, :: OSError,
	:: OSErrorMessage, :: OSErrorCode
from System._Pointer import :: Pointer

WNOHANG		:==	0x00000001
WUNTRACED	:== 0x00000002
MAXPATHLEN	:== 1024

//* @type Int
DIRENT_D_NAME_OFFSET :== IF_MAC 8 (IF_ANDROID 19 (IF_INT_64_OR_32 19 11))

S_IFMT		:== 0170000
S_IFIFO		:== 0010000
S_IFCHR		:== 0020000
S_IFDIR		:== 0040000
S_IFBLK		:== 0060000
S_IFREG		:== 0100000
S_IFLNK		:== 0120000
S_IFSOCK	:== 0140000
S_IFWHT		:== 0160000

STDIN_FILENO  :== 0
STDOUT_FILENO :== 1
STDERR_FILENO :== 2

FIONREAD   :== IF_MAC 0x4004667F 0x541B

F_SETFD    :== 2
FD_CLOEXEC :== 1
O_CLOEXEC  :== 02000000

O_RDONLY   :== 00
O_WRONLY   :== 01
O_RDWR     :== 02
O_CREAT    :== IF_MAC 01000 0100
O_NOCTTY   :== IF_MAC 0x20000 0400

TCSANOW    :== 0
TIOCSCTTY  :== IF_MAC 0x20007461 0x540E

ECHO       :== 0x8
ECHONL     :== 0x40
ICANON     :== 0x2

//Posix API calls
errno		:: !*w -> (!Int,!*w)
strerr		:: !Int -> Pointer
stat		:: !{#Char} !{#Char} !*w -> (!Int,!*w)
unlink		:: !{#Char} !*w -> (!Int,!*w)
fork		:: !*w -> (!Int,!*w)
execvp		:: !{#Char} !{#Pointer} !*w -> (!Int,!*w)
waitpid		:: !Int !{#Int} !Int !*w -> (!Int,!*w)
exit		:: !Int !*w -> (!.a,!*w) 
getcwd		:: !{#Char} !Int !*w -> (!Pointer,!*w)
chdir		:: !{#Char} !*w -> (!Int,!*w)
mkdir		:: !{#Char} !Int !*w -> (!Int,!*w)
rmdir		:: !{#Char} !*w -> (!Int,!*w)
rename		:: !{#Char} !{#Char} !*w -> (!Int,!*w)
opendir		:: !{#Char} !*w -> (!Pointer,!*w)
closedir	:: !Pointer !*w -> (!Int,!*w)
readdir		:: !Pointer !*w -> (!Pointer,!*w)
pipe        :: !Pointer !*w -> (!Int, !*w)

//* NB: this function is not implemented on Android.
posix_openpt :: !Int !*w -> (!Int, !*w)
//* NB: this function is not implemented on Android.
grantpt     :: !Int !*w -> (!Int, !*w)
//* NB: this function is not implemented on Android.
unlockpt    :: !Int !*w -> (!Int, !*w)
//* NB: this function is not implemented on Android.
ptsname     :: !Int !*w -> (!Pointer, !*w)
//* NB: this function is not implemented on Android.
open        :: !Pointer !Int !*w -> (!Int, !*w)
//* Also calls open but with a string as an argument to save an allocation
opens       :: !{#Char} !Int !*w -> (!Int, !*w)
//* NB: this function is not implemented on Android.
tcgetattr   :: !Int !Pointer !*w -> (!Int, !*w)
//* NB: this function is not implemented on Android.
cfmakeraw   :: !Pointer !*w -> *w
//* NB: this function is not implemented on Android.
tcsetattr   :: !Int !Int !Pointer !*w -> (!Int, !*w)
//* NB: this function is not implemented on Android.
setsid      :: !*w -> *w

dup2        :: !Int !Int !*w -> (!Int, !*w)
close       :: !Int !*w -> (!Int, !*w)
ioctl       :: !Int !Int !Pointer !*w -> (!Int, !*w)
// variant requiring an argument as third parameter
fcntlArg    :: !Int !Int !Int !*w -> (!Int, !*w)
read        :: !Int !Pointer !Int !*w -> (!Int, !*w)
write       :: !Int !{#Char} !Int !*w -> (!Int, !*w)
select_     :: !Int !Pointer !Pointer !Pointer !Pointer !*w -> (!Int, !*w)
kill        :: !Int !Int !*w -> (!Int, !*w)

//* NB: this function is not implemented on Android.
timegm      :: !{#Int} -> Int
//* NB: this function is not implemented on Android.
clock_gettime :: !Int !Pointer !*w -> (!Int, !*w)
clock_gettime` :: !Int !{#Int} !*w -> (!Int, !*w)

//Memory (impure)
malloc :: !Int -> Pointer
//* NB: this function is not implemented on Android.
mallocSt :: !Int !*w -> (!Pointer, !*w)
free :: !Pointer -> Int
freeSt :: !Pointer !*w -> *w
memcpy_string_to_pointer :: !Pointer !{#Char} !Int -> Pointer

//Posix datastructures
:: Stat =
	{ st_dev           :: !Int
	, st_ino           :: !Int
	, st_mode          :: !Int
	, st_nlink         :: !Int
	, st_uid           :: !Int
	, st_gid           :: !Int
	, st_rdev          :: !Int
	, st_size          :: !Int
	, st_blocks        :: !Int
	, st_blksize       :: !Int
	, st_ctimespec     :: !Timespec
	, st_mtimespec     :: !Timespec
	, st_atimespec     :: !Timespec
	, st_birthtimespec :: !Timespec //* This field is only used on Mac.
	, st_flags         :: !Int      //* This field is only used on Mac.
	, st_gen           :: !Int      //* This field is only used on Mac.
	}
//Mapping to/from byte arrays
unpackStat	:: !{#Char} -> Stat
sizeOfStat	:: Int

//* NB: this function is not implemented on Android.
pipeBufferSize :: !Int !*w -> (!MaybeOSError Int, !*w)
