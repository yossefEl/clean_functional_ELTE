definition module Text.CSV
/**
* This module provides functions for reading and writing comma separated vector (CSV) files.
*/
import StdFile
/**
* Read a single record from a CSV file
* A comma ',' is used as field separator, double quotes '"' may be used to enclose fields
* and the escape character is backslash '\'.
*
* @param The file handle to read from
*
* @return The record as a list of strings, or `?None` if there are no more records in the file.
* @return The file handle
*/
readCSVRecord :: !*File -> (!?[String],!*File)
/**
* Read a single record from a CSV file with custom separator characters.
*
* @param The field separator character
* @param The field enclosure character
* @param The escape character
* @param The file handle to read from
* 
* @return The record as a list of strings, or `?None` if there are no more records in the file.
* @return The file handle
*/
readCSVRecordWith :: !Char !Char !Char !*File -> (!?[String], !*File)
/**
* Read an entire CSV file.
*
* A comma ',' is used as field separator, double quotes '"' may be used to enclose fields
* and the escape character is backslash '\'.
*
* @param The file handle to read from
*
* @return The list of records which are lists of strings
* @return The file handle
*/
readCSVFile			:: !*File -> (![[String]],!*File)
/**
* Read an entire CSV file with custom separator characters.
*
* @param The field separator character
* @param The field enclosure character
* @param The escape character
* @param The file handle to read from
*
* @return The list of records which are lists of strings
* @return The file handle
*/
readCSVFileWith		:: !Char !Char !Char !*File -> (![[String]],!*File)

/**
* Read an entire CSV file with custom separator characters and accumulate a result given a function processing each row.
* This is the preferred method for reading large files, as the entire file content does not have to be kept in memory.
*
* @param The field separator character
* @param The field enclosure character
* @param The escape character
* @param The function adding each row to the accumulator state
* @param The initial accumulator state
* @param The file handle to read from
*
* @return The final accumulator state
* @return The file handle
*/
foldedCsvRowsWith :: !Char !Char !Char !([String] .acc -> .acc) !.acc !*File -> (!.acc, !*File)

/**
* Write a single record to a CSV file
* A comma ',' is used as field separator, double quotes '"' may be used to enclose fields
* and the escape character is backslash '\'.
*
* @param The record as a list of strings
* @param The file handle to write to
*
* @return The file handle
*/
writeCSVRecord		:: ![String] !*File -> *File
/**
* Write a single record to a CSV file with custom separator characters.
*
* @param The field separator character
* @param The field enclosure character
* @param The escape character
* @param The record as a list of strings
* @param The file handle to write to
*
* @return The file handle
*/ 
writeCSVRecordWith	:: !Char !Char !Char ![String] !*File -> *File
/**
* Write an entire CSV file.
*
* A comma ',' is used as field separator, double quotes '"' may be used to enclose fields
* and the escape character is backslash '\'.
*
* @param The list of records which are lists of strings
* @param The file handle to write to
*
* @return The file handle
*/
writeCSVFile		:: ![[String]] !*File -> *File
/**
* Write an entire CSV file with custom separator characters.
*
* @param The field separator character
* @param The field enclosure character
* @param The escape character
* @param The list of records which are lists of strings
* @param The file handle to write to
*
* @return The file handle
*/
writeCSVFileWith	:: !Char !Char !Char ![[String]] !*File -> *File
