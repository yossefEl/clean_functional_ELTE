implementation module PMHtml

import StdEnv
import Text.HTML
import Text

makePage :: String String [HtmlTag] -> HtmlTag
makePage title message content
	= HtmlTag [] [HeadTag [] head, BodyTag [] body]
where

	head =	[ TitleTag [] [Text title]
			, LinkTag [RelAttr "stylesheet", HrefAttr "/PM.css", TypeAttr "text/css"]
			, ScriptTag [SrcAttr "/PM.js", TypeAttr "text/javascript"] []
			]
	body =	[ DivTag [IdAttr "main-title"] [H1Tag [] [Text title]]
			, DivTag [IdAttr "main-menu"] makeMenu
			, DivTag [IdAttr "main-content"] (msg ++ content)
			]
	msg  = if (message <> "") [DivTag [IdAttr "main-message"] [ImgTag [SrcAttr "/icons/information.png"],Text message]] []

makeMenu :: [HtmlTag]
makeMenu = [ATag [HrefAttr link] [Text title] \\ (title, link) <- items]
where
	items = [("Projects","/projects"),("Employees","/employees")]

makeForm :: String [HtmlTag] -> HtmlTag
makeForm action content = FormTag [ActionAttr action, MethodAttr "post"] content

makeTable :: [String] [[HtmlTag]] -> HtmlTag
makeTable headers rows = TableTag [ClassAttr "pm-table"] [head:body]
where
	head	= TrTag [] [ThTag [] [Text th] \\ th <- headers]
	body	= [TrTag [] [TdTag [] [td] \\ td <- row] \\ row <- rows ]

makeFormLayout :: [(String,[HtmlTag])] -> HtmlTag
makeFormLayout rows = TableTag [ClassAttr "pm-formlayout"] content
where
	content = [TrTag [] [ThTag [] [Text label], TdTag [] field] \\ (label,field) <- rows]

makeFieldSet :: String [HtmlTag] -> HtmlTag
makeFieldSet title content
	= FieldsetTag [ClassAttr "pm-fieldset"] [LegendTag [] [Text title] : content]

makeToolbar :: [HtmlTag] -> HtmlTag
makeToolbar content = DivTag [ClassAttr "pm-toolbar"] content

makeLinkButton :: String String (?String) -> HtmlTag
makeLinkButton label href icon 
	= ButtonTag [OnclickAttr ("window.location='" +++ href +++ "'; return false;")] ((icontag icon)++ [Text label])
where
	icontag ?None		= []
	icontag (?Just file) = [ImgTag [SrcAttr ("/icons/" +++ file +++ ".png")]]


makeSubmitButton :: String (?String) -> HtmlTag
makeSubmitButton label icon
	= ButtonTag [TypeAttr "submit"] ((icontag icon)++ [Text label])
where
	icontag ?None		= []
	icontag (?Just file) = [ImgTag [SrcAttr ("/icons/" +++ file +++ ".png")]]

makeIntInput :: String Int -> HtmlTag
makeIntInput name value = InputTag [ClassAttr "pm-int",NameAttr name, ValueAttr (toString value), SizeAttr "4"]

makeBoolInput :: String Bool -> HtmlTag
makeBoolInput name value = InputTag ([ClassAttr "pm-bool",NameAttr name, TypeAttr "checkbox", ValueAttr "True"] ++ (if value [CheckedAttr] []))

makeStringInput :: String String -> HtmlTag
makeStringInput name value = InputTag [ClassAttr "pm-string",NameAttr name, ValueAttr value]

makeHiddenInput :: String a -> HtmlTag | toString a
makeHiddenInput name value = InputTag [NameAttr name, TypeAttr "hidden", ValueAttr (toString value)]

makeSubsetInput :: String [String] [String] -> HtmlTag
makeSubsetInput name full sub 
	= DivTag [] [TableTag [ClassAttr "pm-subset"] [TrTag [] header, TrTag [] [left,buttons,right]],input]
where
	header = [ThTag [] [Text "Available"],ThTag [] [], ThTag [] [Text "Selected"]]
	left = TdTag [] [SelectTag [IdAttr (name +++ "-left"),SizeAttr "5",MultipleAttr] [OptionTag [ValueAttr option] [Text option] \\ option <- (removeMembers full sub)]]
	right = TdTag [] [SelectTag [IdAttr (name +++ "-right"),SizeAttr "5",MultipleAttr] [OptionTag [ValueAttr option] [Text option] \\ option <- sub]]
	buttons = TdTag [] [ ButtonTag [OnclickAttr ("subset_select('" +++ name +++ "');return false;")] [ImgTag [SrcAttr "/icons/arrow_right.png",AltAttr ">"] ]
					   , BrTag []
					   , ButtonTag [OnclickAttr ("subset_deselect('" +++ name +++ "');return false;")] [ImgTag [SrcAttr "/icons/arrow_left.png",AltAttr "<"]]
					   ]
	input = InputTag [TypeAttr "hidden", NameAttr name, IdAttr (name +++ "-value"), ValueAttr (join "-" sub)]

makeIntSelect :: String Int [(Int,String)] -> HtmlTag
makeIntSelect name value options
	= SelectTag [ClassAttr "pm-int-sel", NameAttr name] [OptionTag [] [Text "Select..."] :[OptionTag ([ValueAttr (toString v)] ++ (if (v == value) [SelectedAttr] []) ) [Text l] \\ (v,l) <- options]]

joinHtml :: HtmlTag [HtmlTag] -> [HtmlTag]
joinHtml sep [] = []
joinHtml sep [x] = [x]
joinHtml sep [x:xs] = [x,sep:joinHtml sep xs]

makeDoneIcon :: Bool -> HtmlTag
makeDoneIcon True = ImgTag [SrcAttr "/icons/tick.png", AltAttr "Done"]
makeDoneIcon False = ImgTag [SrcAttr "/icons/cross.png", AltAttr "Not done"]

