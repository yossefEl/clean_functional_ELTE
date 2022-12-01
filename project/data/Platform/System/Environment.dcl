definition module System.Environment

/**
 * Module for accessing environment variables
 */

getEnvironmentVariable :: !String !*World -> (!?String, !*World)

setEnvironmentVariable :: !String !String !*World -> *World

unsetEnvironmentVariable :: !String !*World -> *World
