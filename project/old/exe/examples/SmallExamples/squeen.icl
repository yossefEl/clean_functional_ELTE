module squeen

/*
The Queens Problem.

Or: How to put n queens on a n*n chessboard in such a way that they
cannot attack each other.
	
The result of this program is the number of possible solutions for
the queens problem for a certain boardsize together with one solution.
When BoardSize is 8 the result will be: (92,[4,2,7,3,6,8,5,1]),
which means the queens are on a4, b2, c7, d3, e6, f8, g5 and h1.

Strictness annotations are used at certain points, because that makes
this program more than twice as fast (the strictness analyzer is not
able to deduce this strictness information). However, other Clean programs
for the Queens problem exist without strictness annotations added by the 
programmer that are only 40% slower than this solution (lqueen.icl).

*/

import StdEnv

BoardSize :== 8 // The size of the chessboard.

//	Finding all solutions for the queens problem.
	
Queens::Int [Int] [[Int]] -> [[Int]]
Queens row board boards
	| row>BoardSize	=  [board : boards]
	| otherwise		=  TryCols BoardSize row board boards

TryCols::Int Int [Int] [[Int]] -> [[Int]]
TryCols 0 row board boards =  boards
TryCols col row board boards
	| Save col 1 board	=	TryCols (col-1) row board queens
	| otherwise			= 	TryCols (col-1) row board boards
where	queens	= Queens (row+1) [col : board] boards

/*	The strictness analyzer can't derive strictness for the first and second
	argument of Save, because they are not used in the first alternative
	of that function. However, Save is strict in these arguments (in the
	context of this program) and adding the strictness annotations speeds
	up this program considerably. */

Save::!Int !Int [Int] -> Bool
Save  c1 rdiff [] =  True
Save  c1 rdiff [c2:cols]
	| cdiff==0 || cdiff==rdiff || cdiff==0-rdiff	=	False
	| otherwise										=	Save c1 (rdiff+1) cols
where	cdiff	= c1 - c2

/*	The Start Rule: Calculate the list of solutions, show the first
	solution and the length of that list. */
	
Start::(Int,[Int])
Start 	= 	(length solutions, hd solutions)
where	solutions	= Queens 1 [] []
