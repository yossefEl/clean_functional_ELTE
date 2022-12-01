definition module System._TTS

/**
 * This module contains the platform-specific implementation for System.TTS.
 * Use the functions in System.TTS for a platform-independent interface.
 */

/**
 * A voice for the text-to-speech function `ttsWithVoice`.
 * This type contains different constructors on each platform.
 */
:: Voice
	= Agnes
	| Albert
	| Alex
	| BadNews
	| Bahh
	| Bells
	| Boing
	| Bruce
	| Bubbles
	| Cellos
	| Deranged
	| Fred
	| GoodNews
	| Hysterical
	| Junior
	| Kathy
	| PipeOrgan
	| Princess
	| Ralph
	| Trinoids
	| Vicki
	| Victoria
	| Whisper
	| Zarvox

//* Platform-specific text-to-speech implementation for System.TTS.
_tts :: !(?Voice) !String !*World -> *World

/**
 * Save the text-to-speech audio generated for a string (the first argument) to
 * a file (the second argument).
 *
 * This function only exists on Mac.
 */
ttsToFile :: !String !String !*World -> *World

/**
 * Like `ttsToFile`, but with a `Voice`.
 *
 * This function only exists on Mac.
 */
ttsWithVoiceToFile :: !Voice !String !String !*World -> *World
