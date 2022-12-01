definition module PMDatabase

from Database.SQL import class SQLCursor, :: SQLError
from Database.SQL.MySQL import :: MySQLCursor, :: MySQLConnection, :: MySQLContext

from PMDataModel import :: Project

//Database initialization and finalization
initDatabase :: !String !String !String !String !*World -> (!*MySQLContext, !*MySQLConnection, !*MySQLCursor, *World)
endDatabase :: !*MySQLContext !*MySQLConnection !*MySQLCursor !*World -> *World

//Example of manually written database operation
updateProject :: Project !*cur -> (?SQLError, *cur) | SQLCursor cur
