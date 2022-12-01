implementation module PMDatabase

import StdEnv
import Database.SQL.MySQL
import Text

import PMDataModel

initDatabase :: !String !String !String !String !*World -> (!*MySQLContext, !*MySQLConnection, !*MySQLCursor, *World)
initDatabase hostname username password database world
	# (mbErr,mbContext,world) 		= openContext world
	| isJust mbErr					= abort "Failed to initialize database library"
	# context						= fromJust mbContext
	# (mbErr,mbConn,context)		= openConnection {host= ?Just hostname,username= ?Just username,password= ?Just password,database=database} context
	| isJust mbErr					= abort ("Failed to connect to database: " +++ toString (fromJust mbErr))
	# connection					= fromJust mbConn
	# (mbErr,mbCursor,connection)	= openCursor connection
	| isJust mbErr					= abort ("Failed to open database cursor: " +++ toString (fromJust mbErr))
	# cursor						= fromJust mbCursor
	= (context,connection,cursor,world)

endDatabase :: !*MySQLContext !*MySQLConnection !*MySQLCursor !*World -> *World
endDatabase context connection cursor world
	# (_,connection)			= closeCursor cursor connection
	# (_,context)				= closeConnection connection context
	# (_,world)					= closeContext context world
	= world

//Boring function we do no longer have to write
updateProject :: Project !*cur -> (?SQLError, *cur) | SQLCursor cur
updateProject project =: {Project | project_projectNr = pid} cursor
	//Update the project record
	# (mbErr,cursor)		= execute "UPDATE project SET description = ?, parent = ? WHERE projectNr = ?" pvalues cursor
	| isJust mbErr			= (mbErr, cursor)
	//Update/create the linked employees
	# (mbErr, ids, cursor)	= linkEmployees project.projectworkers_employee_ofwhich_project cursor
	| isJust mbErr			= (mbErr, cursor)
	//Garbage collect linked employees	
	# (mbErr,cursor)		= execute ("DELETE FROM projectworkers WHERE project = ?" +++ ematch ids) (evalues ids) cursor
	| isJust mbErr			= (mbErr, cursor)
	//Update/add the tasks
	# (mbErr,ids,cursor)	= updateTasks project.task_ofwhich_project cursor
	| isJust mbErr			= (mbErr, cursor)
	//Garbage collect tasks
	# (mbErr,cursor)		= execute ("DELETE FROM task WHERE project = ?" +++ tmatch ids) (tvalues ids) cursor
	| isJust mbErr			= (mbErr, cursor)
	= (?None, cursor)
where
	pvalues = [SQLVVarchar project.project_description, pparent project.project_parent, SQLVInteger project.Project.project_projectNr]
	pparent ?None = SQLVNull
	pparent (?Just {ProjectID| project_projectNr = x}) = SQLVInteger x

	linkEmployees [] cursor = (?None, [], cursor)
	linkEmployees [{EmployeeID | employee_name = e}:es] cursor
		# (mbErr, cursor)	= execute "SELECT * FROM projectworkers WHERE project = ? AND employee = ?" [SQLVInteger pid, SQLVVarchar e] cursor
		| isJust mbErr		= (mbErr,[],cursor)
		# (mbErr, num, cursor) = numRows cursor
		| num == 0
			# (mbErr, cursor)		= execute "INSERT INTO projectworkers (project,employee) VALUES (?,?)" [SQLVInteger pid, SQLVVarchar e] cursor
			| isJust mbErr			= (mbErr,[],cursor)
			# (mbErr,ids,cursor)	= linkEmployees es cursor
			= (mbErr,[e:ids],cursor)
		| otherwise
			# (mbErr,ids,cursor)	= linkEmployees es cursor
			= (mbErr,[e:ids],cursor)

	ematch []	= ""
	ematch ids	= " AND NOT (employee IN (" +++ (join "," ["?" \\ x <- ids]) +++ "))"
	evalues ids = [SQLVInteger pid: map SQLVVarchar ids]

	updateTasks [] cursor = (?None, [], cursor)
	updateTasks [{Task | task_taskNr = taskNr, task_description = description, task_done = done}:ts] cursor
		| taskNr == 0
			# vals					= [SQLVVarchar description, SQLVInteger (if done 1 0), SQLVInteger pid]
			# (mbErr, cursor)		= execute "INSERT INTO task (description,done,project) VALUES (?,?,?)" vals cursor
			| isJust mbErr			= (mbErr, [], cursor)
			# (mbErr, i, cursor)	= insertId cursor
			| isJust mbErr			= (mbErr, [], cursor)
			# (mbErr, ids, cursor)	= updateTasks ts cursor
			= (mbErr, [i:ids], cursor)	
		| otherwise	
			# vals 					= [SQLVVarchar description,SQLVInteger (if done 1 0),SQLVInteger pid,SQLVInteger taskNr]
			# (mbErr, cursor)		= execute "UPDATE task SET description = ?, done = ?, project = ? WHERE taskNr = ? " vals cursor
			| isJust mbErr			= (mbErr, [], cursor)
			# (mbErr, ids, cursor)	= updateTasks ts cursor
			= (mbErr, [taskNr:ids], cursor)

	tmatch []	= ""
	tmatch ids	= " AND NOT (taskNr IN (" +++ (join "," ["?" \\ x <- ids]) +++ "))"
	tvalues ids	= map SQLVInteger [pid:ids]
