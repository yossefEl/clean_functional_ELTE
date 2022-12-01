definition module System._Environment

/**
 * This is a platform-specific module. Use the general interface in
 * System.Environment instead.
 */

_getEnvironmentVariable :: !String !*World -> (?String, *World)

_setEnvironmentVariable :: !String !String !*World -> *World

_unsetEnvironmentVariable :: !String !*World -> *World
