module hamming

/*
The Hamming Function.

The result of this program is a list of the first NrElements numbers
having only 2, 3 and 5 as prime factors. Result:

	[1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,...].

Run the program with the Show Constructors option on (Application options)
*/

import StdEnv

/*	Ham (the Hamming function) returns an infinite list of numbers
	having two, three and five as only prime factors by recursively
	mapping the functions ((*) 2), ((*) 3) and ((*) 5) over the list of
	Hamming numbers found sofar and merging these lists into one.

	The definition of y specifies a cyclic graph to be used yielding a
	polynomial complexity in stead of an exponential one.  

	The argument lists of merge never end with a Nil 
	and they are not Nil initially, so
	the definition of merge can be specified for 'infinite' lazy lists
	leaving out special cases for empty lists.	
*/

Ham::[Int]
Ham	=	y
where
	y	=	[1:merge (merge (map ((*) 2) y) (map ((*) 3) y)) (map ((*) 5) y)]

	merge f=:[a:b]	g=:[c:d]
		| a<c		=  [a: merge b g]
		| a==c		=  merge f d
		| otherwise	=  [c: merge f d]
									
Start::[Int]
Start = take NrElements Ham

NrElements :== 300	// The number of Hamming numbers to be calculated.

