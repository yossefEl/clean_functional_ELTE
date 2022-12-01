module pascal

/*
The Triangle of Pascal.

The result of this program is a real triangle of Pascal of height
Height, not just a list representing such a triangle:

        1
       1 1
      1 2 1
     1 3 3 1
    1 4 6 4 1    etc.
	
Run the program using the "Basic Values Only" option (Application options).
Use a non-proportional font for the output (e.g. Monaco 9).
*/

import StdEnv

/*	A row of the triangle is represented by a list of integers,
	the triangle as a list of rows:
*/

::Row		:== [Int]
::Triangle	:== [Row]

//	Some constants

NrRows :== 18	//	Number of rows to be shown.
Middle :== 40	//	The middle of a 80 character line.

//	Miscellaneous functions

NrOfDigits:: Int -> Int
NrOfDigits 0	=  0
NrOfDigits n	=  NrOfDigits (n / 10) + 1

//	Calculating the Triangle.

Pascal::Triangle
Pascal = p 
where 
	p = [[1] : [Next a \\ a <- p]]

	Next x =  AddRows [0:x] x

	AddRows::Row Row -> Row
	AddRows [a:x] [b:y] =  [a + b : AddRows x y]
	AddRows [a]   []    =  [a]
	AddRows []    []    =  []

//	Formatting the list representing the triangle as a real triangle.

FormatRows::Triangle -> [String]
FormatRows [f:r] =  [ FormatRow f  +++ "\n" : FormatRows r]
where
	FormatRow::Row -> String
	FormatRow row
		=   toString (spaces (Middle - Length_new row/2 ))  +++  FormatElems row 

	FormatElems::Row -> String
	FormatElems [f:r] =  " " +++ toString f +++  FormatElems r  
	FormatElems []    =  ""

	Length_new::Row -> Int
	Length_new [f:r] =  NrOfDigits f +  Length_new r + 1
	Length_new []	 =  -1
FormatRows []    =  []

/*	The Start rule: The first NrRows rows of the (infinite) triangle
	returned by Pascal are taken and shown on the screen as a
	triangle by means of FormatRows.
*/

Start::[String]
Start = FormatRows (take NrRows Pascal)
