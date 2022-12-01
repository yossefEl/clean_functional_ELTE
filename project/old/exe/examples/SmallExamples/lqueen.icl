module lqueen

//	The Queens Problem, slow version.

import StdEnv
	
BoardSize :== 8 // The size of the chessboard.

//	Finding all solutions for the queens problem.
	
Queens::Int [Int] [[Int]] -> [[Int]]
Queens row board boards
	| row>BoardSize	= [board:boards]
	| otherwise		= TryCols BoardSize row board boards

//	The second alternative of TryCols is added to make sure Save is never
//	called with an empty list.
	
TryCols::Int Int [Int] [[Int]] -> [[Int]]
TryCols 0 row board boards	= 	boards
TryCols col row [] boards	= 	TryCols (col-1) row [] queens
where 
	queens	= Queens (row+1) [col] boards
		
TryCols col row board boards
	| Save col 1 board	= 	TryCols (col-1) row board queens					 
	| otherwise			= 	TryCols (col-1) row board boards
where 
	queens	= Queens (row+1) [col : board] boards
		

Save::Int Int [Int] -> Bool
Save c1 rdiff [c2]	=  cdiff<>0 && cdiff<>rdiff && cdiff<> 0 - rdiff
where
	cdiff = c1 - c2
		
Save c1 rdiff [c2:cols]	
	| cdiff==rdiff || cdiff==0 || cdiff==0-rdiff	= 	False
	| otherwise										= 	Save c1 (inc rdiff) cols
where 
	cdiff = c1 - c2
		

/*	The Start Rule: Calculate the list of solutions, show the first
	solution and the length of that list.
*/

Start::(Int,[Int])
Start = (length solutions, hd solutions)
	where 
		solutions = Queens 1 [] []
				
