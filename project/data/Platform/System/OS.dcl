definition module System.OS

OS_NAME :== "Mac OS X (64-bit)"
OS_PATH_SEPARATOR :== '/'
OS_NEWLINE :== "\n"

IF_POSIX_OR_WINDOWS posix windows	:== posix

IF_WINDOWS win other				:== other
IF_WINDOWS32 win other				:== other
IF_WINDOWS64 win other				:== other
IF_POSIX posix other				:== posix
IF_LINUX linux other				:== other
IF_LINUX32 linux other				:== other
IF_LINUX64 linux other				:== other
IF_MAC mac other					:== mac
IF_ANDROID android other                        :== other
