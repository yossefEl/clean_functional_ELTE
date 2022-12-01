definition module System.Socket.Unix

/**
 * This module implements the System.Socket interface for Unix sockets.
 * It is only available on POSIX systems.
 */

from System.FilePath import :: FilePath(..)
from StdOverloaded import class toString
from System.Socket import class SocketAddress

/**
 * A Unix socket, implementing the System.Socket interface.
 * This type and its instances are only available on POSIX systems.
 */
:: UnixSocketAddress =
	{ unix_socket_path :: !FilePath
	}

instance SocketAddress UnixSocketAddress
instance toString UnixSocketAddress
