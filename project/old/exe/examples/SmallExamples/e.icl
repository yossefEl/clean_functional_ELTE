module e

/*
Approximation of the number e.

Result: A list containing the first NrDigits digits of e = [2,7,1,8,2,8,1,8,2,8,...].
*/

import StdEnv

NrDigits :== 200	// The number of digits of the approximation of e

//	Approximating e:

Approx_e::[Int]
Approx_e =	[2:Expan ones] where ones= [1:ones]

//	Expan expects an infinite list of ones and returns an infinite
//	list containing the digits of the fraction of e ([7,1,8,2,8,...]).

Expan::[Int] -> [Int]
Expan f	= [hd ten:Expan	(tl ten)]
	where 
		ten = Ten 2 f

		Ten::Int [Int] -> [Int]
		Ten c [p:q]	| Safe k c	= 	[k / c,  k rem c  + a1 : b1]
								= 	[(k + a1) / c, (k + a1) rem c : b1]
		where 
			a1	=	hd ten
			b1	=	tl ten
			ten	=	Ten (c+1) q
			k	=	10 * p

Safe::Int Int -> Bool
Safe k c =   k/c  ==  (k + 9)/c 

/*
The Start rule	returns the first NrDigits elements of the
				list of digits returned by the function
				'Approx_e' by means of the function take.
*/
	
Start::[Int]
Start = take NrDigits Approx_e
