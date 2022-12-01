implementation module System.TTS

import System._TTS
import qualified System._TTS

tts :: !String !*World -> *World
tts s w = 'System._TTS'._tts ?None s w

ttsWithVoice :: !Voice !String !*World -> *World
ttsWithVoice v s w = 'System._TTS'._tts (?Just v) s w
