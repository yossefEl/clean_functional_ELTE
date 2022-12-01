module PM

import StdEnv, StdGeneric

import Data.Maybe
import Database.SQL
import Database.SQL.MySQL
import Database.SQL.RelationalMapping
import Internet.HTTP
import Internet.HTTP.CGI
import Text
import Text.Encodings.UrlEncoding
import Text.HTML

from Data.Map import :: Map(..), get, fromList

import PMHtml
import PMForms
import PMDataModel
import PMDatabase

//Database settings
dbHostname	:== "localhost"
dbUsername	:==	"root"
dbPassword	:==	"test"
dbDatabase	:== "pmdemo"

//Generic derives
derive relMap Employee, EmployeeID, Project, ProjectID, Task, TaskID
derive bimap []

//********************************************************************************
// Framework
//********************************************************************************

//Start the CGI Wrapper
Start :: *World -> *World
Start world = startCGI [CGIOptStaticFallback True] [(isPage, pageHandler)] world

helloPage :: !HTTPRequest !*World -> (!HTTPResponse,!*World)
helloPage req world
		= ({okResponse & rsp_data = toString (page (name req))},world)
where
	name req	= case get "name" req.arg_get of
		?None	 	= "world"
		(?Just n)	= n
	page name = HtmlTag [] [head name,body name] 
	head name = HeadTag [] [TitleTag [] [Text "Hello ", Text name]]
	body name = BodyTag [] [H1Tag [] [Text "Hello ", Text name]]


//Predicate which checks if a request should be handled by this application
isPage :: !String -> Bool
isPage page
	| endsWith ".png" page			= False	//Images are served statically
	| endsWith ".jpg" page			= False	
	| endsWith ".css" page			= False	//Stylesheets as well
	| endsWith ".js" page 			= False	//And javascript too
									= True

//Main request handler. Initializes and then decides what page generation function should be applied
pageHandler :: !HTTPRequest !*World -> (HTTPResponse, *World)
pageHandler req world
	//Initialialize the database
	# (context,connection,cursor,world) = initDatabase dbHostname dbUsername dbPassword dbDatabase world
	//Parse the request 
	# (mbSection, mbId, mbAction)		= parseUrl req.HTTPRequest.req_path
	# message							= maybe "" id (get "msg" req.arg_get)
	//Create page content
	# (mbRedirect, title, content, cursor)	= makeContent mbSection mbId mbAction req cursor
	//Finalize database
	# world								= endDatabase context connection cursor world
	//Create http response
	| isJust mbRedirect					//Redirect
		# (url,msg)						= fromJust mbRedirect
		# url							= url +++ "?msg=" +++ urlEncode msg
		# rsp							= {HTTPResponse | newHTTPResponse 301 "Moved Permanently" & rsp_headers = [("Status","301 Moved Permanently"),("Location",url)]}
		= (rsp, world)
	| otherwise							//Show a page
		# rsp							= {okResponse & rsp_data = toString (makePage title message content)}
		= (rsp, world)

parseUrl :: String -> (?String, ?String, ?String)
parseUrl url 
	# parts 			= split "/+" url
	| length parts > 2	= (?None, ?None, ?None)
	# (url,action)		= if (length parts == 2) (hd parts, ?Just (last parts)) (hd parts, ?None)
	# parts 			= split "/" url
	# parts				= if (last parts == "") (init parts) parts
	| length parts == 2	= (?Just (last parts), ?None, action)
	| length parts == 3 = (?Just (join "/" (init (tl parts))), ?Just (last parts), action)
						= (?None, ?None, action)
	
makeContent :: (?String) (?String) (?String) HTTPRequest !*cur -> (?(String,String), String, [HtmlTag], !*cur) | SQLCursor cur & bimap{|*|} cur
makeContent (?Just "projects")	?None				(?Just "add")	req	cursor	= createProjectPage req cursor
makeContent (?Just "projects")	?None				_				req	cursor	= listProjectsPage cursor
makeContent (?Just "projects")	(?Just projectNr)	(?Just "edit")	req	cursor	= editProjectPage (toInt projectNr) req cursor
makeContent (?Just "projects")	(?Just projectNr)	(?Just "delete")	req	cursor	= deleteProjectPage (toInt projectNr) req cursor
makeContent (?Just "projects")	(?Just projectNr)	_				req	cursor	= showProjectPage (toInt projectNr) cursor
makeContent (?Just "employees")	?None				(?Just "add")	req	cursor	= createEmployeePage req cursor
makeContent (?Just "employees")	?None				_				req	cursor	= listEmployeesPage cursor
makeContent (?Just "employees")	(?Just name)			(?Just "edit")	req	cursor	= editEmployeePage name req cursor
makeContent (?Just "employees")	(?Just name)			(?Just "delete")	req	cursor	= deleteEmployeePage name req cursor
makeContent (?Just "employees")	(?Just name)			_				req	cursor	= showEmployeePage name cursor
makeContent _					_					_				req	cursor	= startPage cursor

//********************************************************************************
// Pages
//********************************************************************************

startPage :: !*cur -> (?(String,String), !String, ![HtmlTag], !*cur) | SQLCursor cur
startPage cursor = (?None,"PM Demo", content, cursor)
where
	content =
		[ PTag []	[ Text "This is a simple project management system "
					, Text "which makes use of the Clean GenSQL database library."
					]
		, PTag []	[ Text "All mapping between the underlying relational database and the "
					, Text "Clean data structures used in this web application is "
					, Text "done by just one generic function."
					]
		]

//****** Projects ******//

listProjectsPage :: !*cur -> (?(String, String), !String, ![HtmlTag], !*cur) | SQLCursor cur
listProjectsPage cursor 
	# (mbErr,cursor)		= execute "SELECT projectNr,description FROM project" [] cursor
	| isJust mbErr			= (?None, "Error",[Text (toString (fromJust mbErr))],cursor)
	# (_,rows,cursor)		= fetchAll cursor
	# rows					= map makeRow rows
	= (?None, "Projects", [toolbar, makeTable ["Project Nr","Description"] rows], cursor)
	where
		makeRow [SQLVInteger id, SQLVVarchar description] = [ATag [HrefAttr ("/projects/" +++ (toString id))] [Text (toString id)],Text description]
		toolbar	= makeToolbar [makeLinkButton "Add project" "/projects/+add" (?Just "add")]

showProjectPage :: !Int !*cur -> (?(String,String), !String, ![HtmlTag], !*cur ) | SQLCursor cur & bimap{|*|} cur
showProjectPage pid cursor 
	# (mbErr, mbProject, cursor)	= mapRead pid cursor	//Relational mapping
	| isJust mbErr					= (?None, "Error",[Text (toString (fromJust mbErr))],cursor)
	| isNothing mbProject			= (?None, "Error",[Text ("There is no project with nr " +++ toString pid)], cursor)
	# project						= fromJust mbProject
	= (?None, project.project_description,[toolbar, showProjectForm project],cursor)
	where
		toolbar	= makeToolbar [makeLinkButton "Edit" ("/projects/" +++ (toString pid)+++ "/+edit") (?Just "edit"),makeLinkButton "Delete" ("/projects/" +++ (toString pid) +++ "/+delete") (?Just "delete")]

createProjectPage :: !HTTPRequest !*cur -> (?(String,String), !String, [HtmlTag], !*cur) | SQLCursor cur & bimap{|*|} cur
createProjectPage req cursor
	| req.HTTPRequest.req_method == HTTP_POST
		# project						= editProjectUpd req.arg_post
		# (mbErr,mbId, cursor)			= mapCreate project cursor //Relational mapping
		| isJust mbErr					= (?None, "Error",[Text (toString (fromJust mbErr))],cursor)
		= (?Just ("/projects/" +++ toString (int (fromJust mbId)),"Succesfully created project nr " +++ toString (fromJust mbId)),"",[],cursor)
	| otherwise
		# project						= defaultProject
		# (projects, cursor)			= getProjectOptions cursor
		# (employees,cursor)			= getEmployeeOptions cursor
		= (?None, project.project_description,[editProjectForm True project projects employees],cursor)


editProjectPage :: !Int !HTTPRequest !*cur -> (?(String,String), !String, [HtmlTag], !*cur) | SQLCursor cur & bimap{|*|} cur
editProjectPage pid req cursor
	| req.HTTPRequest.req_method == HTTP_POST
		# project						= editProjectUpd req.arg_post
		# (mbErr,mbId, cursor)			= mapUpdate project cursor //Relational mapping
		| isJust mbErr					= (?None, "Error",[Text (toString (fromJust mbErr))],cursor)
		= (?Just ("/projects/" +++ toString (int (fromJust mbId)), "Successfully updated project " +++ toString pid),"",[],cursor)
		//# (mbErr,cursor)				= updateProject project cursor
		//| isJust mbErr					= (?None, "Error",[Text (toString (fromJust mbErr))],cursor)
		//= (?Just ("/projects/" +++ toString pid, "Successfully updated project " +++ toString pid),"",[], cursor)
	| otherwise
		# (mbErr, mbProject, cursor)	= mapRead pid cursor	//Relational mapping
		| isJust mbErr					= (?None, "Error",[Text (toString (fromJust mbErr))],cursor)
		| isNothing mbProject			= (?None, "Error",[Text ("There is no project with project nr " +++ toString pid)], cursor)
		# project						= fromJust mbProject
		# (projects, cursor)			= getProjectOptions cursor
		# (employees,cursor)			= getEmployeeOptions cursor
		= (?None, project.project_description,[editProjectForm False project projects employees],cursor)

deleteProjectPage :: !Int !HTTPRequest !*cur -> (?(String,String), !String, ![HtmlTag], !*cur) | SQLCursor cur & bimap{|*|} cur
deleteProjectPage pid req cursor
	# (mbErr, mbProject, cursor)	= mapDelete pid cursor	//Relational mapping
	| isJust mbErr					= (?None, "Error",[Text (toString (fromJust mbErr))],cursor)
	| isNothing mbProject			= (?None, "Error",[Text ("There is no project with project nr " +++ toString pid)], cursor)
	# project						= fromJust mbProject
	= (?Just ("/projects","Successfully deleted project nr " +++ toString project.Project.project_projectNr),"",[],cursor)

//****** Employees ******//
listEmployeesPage :: !*cur -> (?(String,String), !String, [HtmlTag], !*cur) | SQLCursor cur
listEmployeesPage cursor
	# (mbErr,cursor)		= execute "SELECT name,description FROM employee" [] cursor
	| isJust mbErr			= (?None, "Error",[Text (toString (fromJust mbErr))],cursor)
	# (_,rows,cursor)		= fetchAll cursor
	# rows					= map makeRow rows
	= (?None, "Employees", [toolbar, makeTable ["Name","Description"] rows], cursor)
	where
		makeRow [SQLVVarchar name, SQLVVarchar description] = [ATag [HrefAttr ("/employees/" +++ name)] [Text name], Text description]
		toolbar	= makeToolbar [makeLinkButton "Add employee" "/employees/+add" (?Just "add")]

showEmployeePage :: !String !*cur -> (?(String,String), !String, ![HtmlTag], !*cur) | SQLCursor cur & bimap{|*|} cur
showEmployeePage name cursor
	# (mbErr, mbEmployee, cursor)	= mapRead name cursor	//Relational mapping
	| isJust mbErr					= (?None, "Error",[Text (toString (fromJust mbErr))],cursor)
	| isNothing mbEmployee			= (?None, "Error",[Text ("There is no employee with name " +++ name)], cursor)
	# employee						= fromJust mbEmployee
	= (?None, employee.Employee.employee_name,[toolbar,showEmployeeForm employee],cursor)
	where
		toolbar	= makeToolbar [makeLinkButton "Edit" ("/employees/" +++ name +++ "/+edit") (?Just "edit"),makeLinkButton "Delete" ("/employees/" +++ name +++ "/+delete") (?Just "delete")]

createEmployeePage :: !HTTPRequest !*cur -> (?(String,String), !String, ![HtmlTag], !*cur) | SQLCursor cur & bimap{|*|} cur
createEmployeePage req cursor
	| req.HTTPRequest.req_method == HTTP_POST
		# employee					= editEmployeeUpd req.arg_post
		# (mbErr,mbName, cursor)	= mapCreate employee cursor //Relational mapping
		| isJust mbErr				= (?None, "Error",[Text (toString (fromJust mbErr))],cursor)
		= (?Just ("/employees/" +++ fromJust mbName,"Succesfully created employee " +++ fromJust mbName),"",[],cursor)
	| otherwise
		# employee						= defaultEmployee
		# (projects, cursor)			= getProjectOptions cursor
		= (?None, "New employee", [editEmployeeForm True employee projects], cursor)

editEmployeePage :: !String !HTTPRequest !*cur -> (?(String, String), !String, ![HtmlTag], !*cur) | SQLCursor cur & bimap{|*|} cur
editEmployeePage name req cursor
	| req.HTTPRequest.req_method == HTTP_POST
		# employee					= editEmployeeUpd req.arg_post
		# (mbErr,mbName, cursor)	= mapUpdate employee cursor //Relational mapping
		| isJust mbErr				= (?None, "Error",[Text (toString (fromJust mbErr))],cursor)
		= (?Just ("/employees/" +++ fromJust mbName,"Succesfully updated employee " +++ fromJust mbName),"",[],cursor)
	| otherwise
		# (mbErr, mbEmployee, cursor)	= mapRead name cursor	//Relational mapping
		| isJust mbErr					= (?None, "Error",[Text (toString (fromJust mbErr))],cursor)
		| isNothing mbEmployee			= (?None, "Error",[Text ("There is no employee with name " +++ name)], cursor)
		# employee						= fromJust mbEmployee
		# (projects, cursor)			= getProjectOptions cursor
		= (?None, name,[editEmployeeForm False employee projects],cursor)

deleteEmployeePage :: !String !HTTPRequest !*cur -> (?(String, String), !String, ![HtmlTag], !*cur) | SQLCursor cur & bimap{|*|} cur
deleteEmployeePage name req cursor
	# (mbErr, mbEmployee, cursor)	= mapDelete name cursor	//Relational mapping
	| isJust mbErr					= (?None, "Error",[Text (toString (fromJust mbErr))],cursor)
	| isNothing mbEmployee			= (?None, "Error",[Text ("There is no employee with name " +++ name)], cursor)
	# employee						= fromJust mbEmployee
	= (?Just ("/employees","Successfully deleted employee " +++ employee.Employee.employee_name),"",[],cursor)

//********************************************************************************
// Utility functions
//********************************************************************************
getProjectOptions :: !*cur -> ([ProjectID], *cur) | SQLCursor cur
getProjectOptions cursor
	# (_,cursor)		= execute "SELECT projectNr FROM project" [] cursor
	# (_,rows,cursor)	= fetchAll cursor
	= ([{ProjectID | project_projectNr = x} \\ [SQLVInteger x] <- rows],cursor)

getEmployeeOptions :: !*cur -> ([EmployeeID], *cur) | SQLCursor cur
getEmployeeOptions cursor
	# (_,cursor)		= execute "SELECT name FROM employee" [] cursor
	# (_,rows,cursor)	= fetchAll cursor
	= ([{EmployeeID | employee_name = x} \\ [SQLVVarchar x] <- rows],cursor)

defaultEmployee :: Employee
defaultEmployee = 	{ Employee
					| employee_name								= ""
					, employee_description						= ""
					, projectworkers_project_ofwhich_employee	= []
					}

defaultProject :: Project
defaultProject =	{ Project
					| project_projectNr							= 0
					, project_description						= ""
					, project_parent							= ?None
					, task_ofwhich_project						= []
					, project_ofwhich_parent					= []
					, projectworkers_employee_ofwhich_project	= []
					}

int :: Int -> Int
int x = x
