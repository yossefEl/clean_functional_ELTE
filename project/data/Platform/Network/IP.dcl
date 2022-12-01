definition module Network.IP

/**
 * This module provides basic IPv4 functionality.
 */

from StdOverloaded import class toString, class fromString, class toInt, class fromInt

//* An IPv4 address.
:: IPAddress (=: IPAddress Int)

//* Convert an IP address to its 'dotted decimal' string representation.
instance toString IPAddress
//* Convert an IP address from its 'dotted decimal' string representation.
instance fromString IPAddress

instance toInt IPAddress
instance fromInt IPAddress

//* Look up the DNS A record for a domain name (e.g www.example.com).
lookupIPAddress :: !String !*World -> (!?IPAddress, !*World)
