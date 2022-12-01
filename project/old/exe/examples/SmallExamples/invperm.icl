module invperm

/*
Inverse Permutation.

Inverts the permutation represented by the list InitPerm.
When a permutation of the form (n,n-1,...,2,1) is inverted this
algorithm runs in linear time. The average (and worst) case
behavior is however quadratic (in the size of the permutation).

Run the program with the Show Constructors option on (Application options)
*/

import StdInt, StdMisc

/*	A permutation (Perm) is represented as a list of integers.
	The resulting inverse permutation (TPerm) is built up as a list of
	tuples (TElt) containing an elements and its index, sorted on index.
*/
::Perm		:== [Int]
::Index_new	:== Int
::TElt		:== (Index_new,Int)
::TPerm		:== [TElt]

//	The initial permutation.
//	inverse: [10,12,17,7,1,11,19,18,3,8,4,6,5,14,15,16,2,9,13].

InitPerm::Perm
InitPerm = [5,17,9,11,13,12,4,10,18,1,6,2,19,14,15,16,3,8,7]

//	InvPerm returns the inverse of the permutation p by means of calling Ip.

InvPerm::Perm -> Perm
InvPerm p =  Ip 1 [] p

/*	Ip inverts a permutation (3rd arg) by using the i'th element of
	the initial permutation as index for i, which becomes the element
	of the inverse permutation ('imperative': ip[p[i]] := i). At the
	end the indices have to be removed from the inverse by means of 
	the function Strip.
*/
Ip::Index_new TPerm Perm -> Perm
Ip i ip []		=  Strip ip
Ip i ip [e:pr]	=  Ip (i + 1) (Update_new ip e i) pr


/*	Update adds an element to a list of (index,value)-pairs (a TPerm)
	that is sorted on index.
*/
Update_new::TPerm Index_new Int -> TPerm
Update_new []			   i x			=  [(i,x)]
Update_new [e=:(j,y) : ar] i x	| i<j	=  [(i,x), e : ar]
										=  [e : Update_new ar i x]

//	Strip removes the (superfluous) indices from the resulting permutation.

Strip::TPerm -> Perm
Strip [] 			=  []
Strip [(i,x):ar]	=  [x : Strip ar]
 

//	The Start rule: invert the initial permutation.

Start::Perm
Start = InvPerm InitPerm
