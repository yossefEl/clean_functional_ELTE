definition module PMHtml

from StdOverloaded import class toString
from Text.HTML import :: HtmlTag

makePage :: String String [HtmlTag] -> HtmlTag
makeForm :: String [HtmlTag] -> HtmlTag

makeTable :: [String] [[HtmlTag]] -> HtmlTag

makeFormLayout :: [(String,[HtmlTag])] -> HtmlTag
makeFieldSet :: String [HtmlTag] -> HtmlTag

makeToolbar :: [HtmlTag] -> HtmlTag
makeLinkButton :: String String (?String) -> HtmlTag
makeSubmitButton :: String (?String) -> HtmlTag

makeIntInput :: String Int -> HtmlTag
makeBoolInput :: String Bool -> HtmlTag
makeStringInput :: String String -> HtmlTag
makeHiddenInput :: String a -> HtmlTag | toString a

makeSubsetInput :: String [String] [String] -> HtmlTag

makeIntSelect :: String Int [(Int,String)] -> HtmlTag

joinHtml :: HtmlTag [HtmlTag] -> [HtmlTag]

makeDoneIcon :: Bool -> HtmlTag
