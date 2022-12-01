module WebDemo
/**
* Simple CGI application built solely on clean-platform libs
*/
import StdEnv
import Text.HTML
import Internet.HTTP
import Internet.HTTP.CGI
import qualified Data.Map

page = HtmlTag [] [head,body] 
head = HeadTag [] [TitleTag [] [Text "Hello World!"]]
body = BodyTag [] [H1Tag [] [Text "Hello World!"]]

helloPage :: !HTTPRequest !*World -> (!HTTPResponse,!*World)
helloPage req world
		= ({okResponse & rsp_data = toString (page (name req))},world)
where
	name req	= case 'Data.Map'.get "name" req.arg_get of
		?None	 	= "world"
		(?Just n)	= n
	page name = HtmlTag [] [head name,body name] 
	head name = HeadTag [] [TitleTag [] [Text "Hello ", Text name]]
	body name = BodyTag [] [H1Tag [] [Text "Hello ", Text name]]

Start :: *World -> *World
Start world = startCGI [] [(const True, helloPage)] world
