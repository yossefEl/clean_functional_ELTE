module war_seq

/*
Sequential version of Warshall's shortest path algorithm
 
Calculates the lenghts of the shortest paths between all nodes of
a directed graph represented by its adjacency matrix using Warshall's
algorithm. The result of the program will be a matrix containing the
length of the shortest path between node i and node j on index (i,j).

Run the program with the Show Constructors option on (Application options)
*/

import StdClass; // RWS
import StdInt

::Row :== [Int]	// A row is represented as a list of integers.
::Mat :== [Row]	// A matrix is represented as a list of rows.

Size :== 6		// The size of the initial matrix.

//	The initial matrix.

InitMat::Mat								//	 Shortest path matrix:
InitMat =	[[  0,100,100, 13,100,100  ],	// [  0, 16,100, 13, 20, 20 ]
			[ 100,  0,100,100,  4,  9  ],	// [ 19,  0,100,  5,  4,  9 ]
			[  11,100,  0,100,100,100  ],	// [ 11, 27,  0, 24, 31, 31 ]
			[ 100,  3,100,  0,100,  7  ],	// [ 18,  3,100,  0,  7,  7 ]
			[  15,  5,100,  1,  0,100  ],	// [ 15,  4,100,  1,  0,  8 ]
			[  11,100,100, 14,100,  0 ]]	// [ 11, 17,100, 14, 21,  0 ]


//	Miscellaneous functions.

Min::Int Int -> Int
Min i j | i>j 	=  j
				=  i

Select::[x] Int -> x 
Select [f:r] 1 =  f
Select [f:r] k =  Select r (k - 1)

//	Warshall's shortest path algorithm.

Warshall::Mat -> Mat
Warshall mat = 	Iterate 1 mat

Iterate::Int Mat -> Mat
Iterate i mat	| i>Size 	=  mat
							=  Iterate (i+1) (WarRows i mat (Select mat i))

WarRows::Int Mat Row -> Mat
WarRows i [] rowi 			=  []
WarRows i [rowj:rs] rowi	=  [ UpdateRow (Select rowj i) rowj rowi : WarRows i rs rowi ]
	
UpdateRow::Int Row Row -> Row
UpdateRow ji [] [] 				=  []
UpdateRow ji [jk:rjs] [ik:ris]	=  [ Min jk (ji + ik) : UpdateRow ji rjs ris ]

//	The Start rule: apply Warshall's algorithm on the initial matrix.

Start::Mat
Start 	= Warshall InitMat
