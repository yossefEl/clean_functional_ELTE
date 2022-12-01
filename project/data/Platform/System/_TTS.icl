implementation module System._TTS

import StdTuple, StdOverloaded
import System.Process

_tts :: !(?Voice) !String !*World -> *World
_tts (?Just v) s w = say ["-v", toString v, s] w
_tts ?None     s w = say [s] w

ttsToFile :: !String !String !*World -> *World
ttsToFile str fn world = say [str, "-o", fn] world

ttsWithVoiceToFile :: !Voice !String !String !*World -> *World
ttsWithVoiceToFile voice str fn world = say ["-v", toString voice, str, "-o", fn] world

say :: [String] *World -> *World
say args world = snd (runProcess "/usr/bin/say" args ?None world)

instance toString Voice where
  toString Agnes      =  "Agnes"
  toString Albert     =  "Albert"
  toString Alex       =  "Alex"
  toString BadNews    =  "Bad News"
  toString Bahh       =  "Bahh"
  toString Bells      =  "Bells"
  toString Boing      =  "Boing"
  toString Bruce      =  "Bruce"
  toString Bubbles    =  "Bubbles"
  toString Cellos     =  "Cellos"
  toString Deranged   =  "Deranged"
  toString Fred       =  "Fred"
  toString GoodNews   =  "Good News"
  toString Hysterical =  "Hysterical"
  toString Junior     =  "Junior"
  toString Kathy      =  "Kathy"
  toString PipeOrgan  =  "Pipe Organ"
  toString Princess   =  "Princess"
  toString Ralph      =  "Ralph"
  toString Trinoids   =  "Trinoids"
  toString Vicki      =  "Vicki"
  toString Victoria   =  "Victoria"
  toString Whisper    =  "Whisper"
  toString Zarvox     =  "Zarvox"
