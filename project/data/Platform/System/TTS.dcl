definition module System.TTS

from System._TTS import :: Voice

/**
 * Convert a string to speech and play it on the system's audio.
 *
 * This function is implemented differently on different platforms:
 * - On Linux, it uses `spd-say` (provided by the speed-dispatcher package on
 *   Debian-based systems);
 * - On Mac, it uses `/usr/bin/say`;
 * - On Windows, it uses the Microsoft Speech API's SpVoice interface;
 * - On other platforms it is not implemented.
 */
tts :: !String !*World -> *World

/**
 * Like `tts`, but allows to specify a voice. Be aware that `Voice` has
 * different constructors on different platforms.
 */
ttsWithVoice :: !Voice !String !*World -> *World
