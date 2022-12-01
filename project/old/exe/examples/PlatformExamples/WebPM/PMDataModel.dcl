definition module PMDataModel

:: Employee = 	{ employee_name								:: String
				, employee_description						:: String		
				, projectworkers_project_ofwhich_employee	:: [ProjectID]
				}

:: EmployeeID = { employee_name								:: String
				}

:: Project =	{ project_projectNr							:: Int
				, project_description						:: String
				, project_parent							:: ? ProjectID

				, task_ofwhich_project						:: [Task]
				, project_ofwhich_parent					:: [ProjectID]

				, projectworkers_employee_ofwhich_project	:: [EmployeeID]	
				}

:: ProjectID =	{ project_projectNr							:: Int
				}

:: Task =		{ task_taskNr								:: Int
				, task_project								:: ProjectID	
				, task_description							:: String
				, task_done									:: Bool
				}

:: TaskID = 	{ task_taskNr								:: Int
				}
