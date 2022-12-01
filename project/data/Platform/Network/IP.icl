implementation module Network.IP

import StdEnv

import Data.Functor
import Data.Maybe
import Data.Tuple
import qualified Network._IP
import Text

/* We can fit an IP address into a 32 bit integer by putting the four quads in
 * big endian order (network byte order). */
:: IPAddress =: IPAddress Int

instance toString IPAddress
where
	toString (IPAddress ip) = concat [toString b1,".",toString b2,".",toString b3,".",toString b4]
	where
		b1 =  ip        bitand 255
		b2 = (ip >> 8)  bitand 255
		b3 = (ip >> 16) bitand 255
		b4 = (ip >> 24) bitand 255

instance fromString IPAddress
where
	fromString s = case split "." s of
		[b1,b2,b3,b4] -> IPAddress (toInt b1 + ((toInt b2) << 8) + ((toInt b3) << 16) + ((toInt b4) << 24))
		_             -> IPAddress 0

instance toInt IPAddress where toInt (IPAddress ip) = ip
instance fromInt IPAddress where fromInt i = IPAddress i

lookupIPAddress :: !String !*World -> (!?IPAddress, !*World)
lookupIPAddress name w = appFst (fmap fromInt) ('Network._IP'._lookupIPAddress name w)
