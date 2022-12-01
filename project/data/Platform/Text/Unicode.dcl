definition module Text.Unicode

from Text.Unicode.UChar import :: UChar
from StdOverloaded import class +++
from Data.List import instance +++ [a]

:: UString :== [UChar]

class fromUnicode a :: !UString -> a
class toUnicode a :: !a -> UString
