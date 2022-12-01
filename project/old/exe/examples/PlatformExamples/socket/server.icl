module server

import StdDebug

import StdEnv
import Data.Error
import Data.Maybe
import System.Socket
import System.Socket.Ipv4

Start :: *World -> (MaybeOSError (), *World)
Start w
	= case socket SocketStream w of
		(Error e, w) = (Error e, w)
		(Ok sockfd, w)
			#! (merr, sockfd) = bind {ipv4_socket_port=8124,ipv4_socket_addr= ?None} sockfd
			| isError merr = (merr, w)
			#! (merr, sockfd) = listen 3 sockfd
			| isError merr = (merr, w)
			= case accept sockfd of
				(Error e, sockfd) = (Error e, w)
				(Ok (sock, addr), sockfd)
					# (merr, sock) = send "Hello world!" [] sock
					| isError merr = (liftError merr, w)
					# (merr, w) = close sock w
					| isError merr = (merr, w)
					# (merr, w) = close sockfd w
					| isError merr = (merr, w)
					= (Ok (), w)
