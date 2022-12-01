definition module PMForms

from Text.HTML import :: HtmlTag
from Data.Map import :: Map

from PMDataModel import :: Project, :: ProjectID, :: Employee, :: EmployeeID

showProjectForm		:: Project -> HtmlTag
editProjectForm		:: Bool Project [ProjectID] [EmployeeID] -> HtmlTag
editProjectUpd		:: (Map String String) -> Project

showEmployeeForm	:: Employee -> HtmlTag
editEmployeeForm	:: Bool Employee [ProjectID] -> HtmlTag
editEmployeeUpd		:: (Map String String) -> Employee
