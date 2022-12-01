module str_arit

/*
String Arithmetic.

This program demonstrates string arithmetic by mergesorting the characters of a large string.
*/

import StdEnv

//	*S is needed to create the large string. 
 
mul_S::Int String -> String
mul_S 0 string =  ""
mul_S n string =  string +++  mul_S (n-1) string 

//	The mergesort algorithm on strings.

MergeSort::String -> String
MergeSort str
	| len<=1	= 	str
	| otherwise	= 	Merge (MergeSort first) (MergeSort second)
where 
	first	=	str%(0,middle - 1)
	second	=	str%(middle,len - 1)
	middle	=	len /2
	len	=	size str

Merge::String String -> String
Merge str  ""   =  str
Merge ""   str  =  str
Merge str1 str2
	| ch1<ch2	= 	ch1 +++  Merge (RemoveFirstChar str1) str2 
	| otherwise	= 	ch2 +++  Merge str1 (RemoveFirstChar str2) 
where 
	ch1	= str1%(0,0)
	ch2	= str2%(0,0)
						

RemoveFirstChar::String -> String
RemoveFirstChar string =  string%(1,size string-1)

//	The Start rule: sort a large string (30*40 characters).
 
Start::String
Start 	= MergeSort (mul_S 30 "Sort this garbage properly, please :-). ")
