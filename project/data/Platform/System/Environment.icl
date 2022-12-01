implementation module System.Environment

import qualified System._Environment

getEnvironmentVariable :: !String !*World -> (!?String, !*World)
getEnvironmentVariable v w = 'System._Environment'._getEnvironmentVariable v w

setEnvironmentVariable :: !String !String !*World -> *World
setEnvironmentVariable v x w = 'System._Environment'._setEnvironmentVariable v x w

unsetEnvironmentVariable :: !String !*World -> *World
unsetEnvironmentVariable v w = 'System._Environment'._unsetEnvironmentVariable v w
