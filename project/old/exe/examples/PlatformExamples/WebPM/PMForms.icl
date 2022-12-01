implementation module PMForms

import StdEnv
import Data.Maybe
import Text
import Text.HTML

from Data.Map import :: Map(..), get, toList, toAscList, foldrWithKey

import PMHtml, PMDataModel

showProjectForm :: Project -> HtmlTag
showProjectForm p = DivTag [] [ shortinfo, makeFieldSet "Tasks" [tasks] ]
where
	shortinfo = makeFormLayout (zip2 labels fields)
	labels = ["Project nr","Description","Parent project","Child projects", "Assigned employees"]
	fields = [[Text (toString p.Project.project_projectNr)],[Text p.project_description],[parent],children,employees]
	parent = case p.Project.project_parent of
		?None										= Text "-"
		?Just {ProjectID|project_projectNr = pid}	= ATag [HrefAttr ("/projects/" +++ toString pid)] [Text (toString pid)]
	children = joinHtml (Text ", ") [ATag [HrefAttr ("/projects/" +++ toString pid)] [Text (toString pid)] \\ {ProjectID | project_projectNr = pid} <- p.project_ofwhich_parent]
	employees = joinHtml (Text ", ") [ATag [HrefAttr ("/employees/" +++ name)] [Text name] \\ {EmployeeID | employee_name = name} <- p.projectworkers_employee_ofwhich_project]
	tasks = makeTable ["Task Nr","Description","Done"] [[Text (toString t.Task.task_taskNr),Text t.Task.task_description, makeDoneIcon t.Task.task_done] \\ t <- p.task_ofwhich_project]


editProjectForm :: Bool Project [ProjectID] [EmployeeID] -> HtmlTag
editProjectForm create project projects employees = makeForm (if create "+add" "+edit") [content,makeFieldSet "Tasks" tasks,buttons]
where
	content	= makeFormLayout (zip2 labels fields)
	labels = ["Project Nr","Description","Parent project","Child projects", "Assigned employees"]
	fields = [ [Text (if create "-" (toString project.Project.project_projectNr)), makeHiddenInput "project_projectNr" project.Project.project_projectNr]
			 , [makeStringInput "project_description" project.Project.project_description]
			 , [makeIntSelect "project_parent" parent [(p,toString p) \\ {ProjectID|project_projectNr = p} <- projects | p <> project.Project.project_projectNr]]
			 , [Text (join ", " children), makeHiddenInput "project_ofwhich_parent" (join "-" children)]
			 , [makeSubsetInput "projectworkers_employee_ofwhich_project" 
								[e.EmployeeID.employee_name \\ e <- employees]
								[e.EmployeeID.employee_name \\ e <- project.Project.projectworkers_employee_ofwhich_project]]
			 ]
	children =[toString pid \\ {ProjectID | project_projectNr = pid} <- project.project_ofwhich_parent]
	tasks = [makeTable ["","Task Nr","Description","Done"]
				[[ ATag [HrefAttr "#", OnclickAttr "delTask(this);"] [ImgTag [SrcAttr "/icons/delete.png", AltAttr "Remove"]]
				 , Text (toString t.Task.task_taskNr)
				 , makeStringInput ("task_description-" +++ toString t.Task.task_taskNr) t.Task.task_description
				 , makeBoolInput ("task_done-" +++ toString t.Task.task_taskNr) t.Task.task_done]
				 \\ t <- project.task_ofwhich_project] 
			,DivTag [ClassAttr "pm-addrow"] [ATag [HrefAttr "#", OnclickAttr "addTask(this);"] [ImgTag [SrcAttr "/icons/add.png", AltAttr "Remove"]],ATag [HrefAttr "#", OnclickAttr "addTask(this);"] [Text "Add another task"]]
			]
	parent = case project.project_parent of
		?None									= 0
		?Just {ProjectID| project_projectNr = x}	= x
	buttons = makeToolbar [makeLinkButton "Cancel" "." (?Just "cross"), makeSubmitButton "Ok" (?Just "tick")]

editProjectUpd :: (Map String String) -> Project
editProjectUpd args
	# pid = maybe 0 toInt (get "project_projectNr" args)
	# parent = maybe 0 toInt (get "project_parent" args)
	# children 	= get "project_ofwhich_parent" args
	# children	= maybe [] (split "-") children
	# children	= [{ProjectID | project_projectNr = toInt pid} \\ pid <- children]
	# employees	= get "projectworkers_employee_ofwhich_project" args
	# employees = maybe [] (split "-") employees
	# employees = [{EmployeeID | employee_name = name} \\ name <- employees | name <> ""]
	# tasks = [ t \\ ?Just t <- map (makeTask pid args) (toList args)]
	=	{ project_projectNr	= maybe 0 toInt (get "project_projectNr" args)
		, project_description = maybe "" id (get "project_description" args)
		, project_parent = if (parent <> 0) (?Just {ProjectID| project_projectNr = parent}) ?None
		, project_ofwhich_parent = children
		, task_ofwhich_project = tasks
		, projectworkers_employee_ofwhich_project = employees
		}
where
	makeTask :: Int (Map String String) (String,String) -> ?Task
	makeTask pid args (name,value)
		| name % (0,15) <> "task_description"	= ?None
		# taskNr = toInt (name % (17,size name))
		# done	= isJust (get ("task_done-" +++ toString taskNr) args)
		= ?Just ({Task | task_taskNr = (if (taskNr < 0) 0 taskNr), task_description = value, task_project = {ProjectID | project_projectNr = pid}, task_done = done})

showEmployeeForm :: Employee -> HtmlTag
showEmployeeForm e = makeFormLayout (zip2 labels fields)
where
	labels = ["Name","Description","Works on projects"]
	fields = [[Text e.Employee.employee_name], [Text e.employee_description], projects]
	projects = joinHtml (Text ", ")[ ATag [HrefAttr ("/projects/" +++ toString pid)]
		[Text (toString pid)] \\ {ProjectID | project_projectNr = pid} <- e.projectworkers_project_ofwhich_employee]

editEmployeeForm :: Bool Employee [ProjectID] -> HtmlTag
editEmployeeForm create employee projects = makeForm (if create "+add" "+edit") [content, buttons]
where
	content	= makeFormLayout (zip2 labels fields)
	buttons = makeToolbar [makeLinkButton "Cancel" "." (?Just "cross"), makeSubmitButton "Ok" (?Just "tick")]
	labels = ["Name","Description","Works on projects"]
	fields = [ if create [makeStringInput "employee_name" employee.Employee.employee_name] [SpanTag [] [Text employee.Employee.employee_name,makeHiddenInput "employee_name" employee.Employee.employee_name]]
			 , [makeStringInput "employee_description" employee.Employee.employee_description]
			 , [makeSubsetInput "projectworkers_project_ofwhich_employee"
								[toString p.ProjectID.project_projectNr \\ p <- projects]
								[toString p.ProjectID.project_projectNr \\ p <- employee.Employee.projectworkers_project_ofwhich_employee]]
			 ]

editEmployeeUpd :: (Map String String) -> Employee
editEmployeeUpd args
	# projects		= get "projectworkers_project_ofwhich_employee" args
	# projects		= maybe [] (split "-") projects
	# projects		= [{ProjectID | project_projectNr = toInt pid} \\ pid <- projects | toInt pid > 0]
	=	{ employee_name = maybe "" id (get "employee_name" args)
		, employee_description = maybe "" id (get "employee_description" args)
		, projectworkers_project_ofwhich_employee = projects
		}

instance fromString Bool
where
	fromString "True" = True
	fromString _	  = False
