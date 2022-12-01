definition module System.Socket.Ipv6

from StdOverloaded import class toString
from Network.IP import :: IPAddress
from System.Socket import class SocketAddress

:: Ipv6SocketAddress =
	{ ipv6_socket_port     :: !Int
	, ipv6_socket_flowinfo :: !Int
	, ipv6_socket_addr     :: !?String
	, ipv6_socket_scope_id :: !Int
	}
instance SocketAddress Ipv6SocketAddress
instance toString Ipv6SocketAddress
