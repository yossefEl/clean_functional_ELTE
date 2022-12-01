implementation module ArgEnv

import qualified System.CommandLine
import qualified System.Environment

import System._Unsafe
import StdEnv

getEnvironmentVariable :: !{#Char} -> *EnvironmentVariable
getEnvironmentVariable s = case accUnsafe ('System.Environment'.getEnvironmentVariable s) of
	?None   = EnvironmentVariableUndefined
	?Just s = EnvironmentVariable {c\\c<-:s}

getCommandLine :: {.{#Char}}
getCommandLine = {{c\\c<-:a}\\a<-accUnsafe 'System.CommandLine'.getCommandLine}
