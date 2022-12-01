module mulmat

/*
Matrix Multiplication.

This  program  performs  matrix multiplication  on matrices of  integers.
Lists are used to simulate matrices
The  initial matrices  (Mat1 & Mat2) can have  arbitrary size (Size). The
second matrix is transposed  first, in order to avoid traversing a matrix
by column, which is very inefficient. The result of the program shows the
initial  matrices  and the  resulting matrix. Run  the program  with  the
"Show Constructors" option on (Application Options).
*/

import StdInt, StdString

::Row		:== [Int]	// A row is a list of integers.
::Col		:== [Int]	// A column is a list of integers.
::Mat		:== [Row]	// A matrix is a list of rows.
::TMat		:== [Col]	// A transposed matrix is a list of columns.
::Index_new	:== Int		// An index is an integer.

Size :== 6	// The size of the matrices.

//	The initial matrices

Mat1::Mat
Mat1 	=	[[  1, 2, 3, 4, 5, 6 ]		// 
			,[  0, 1, 2, 3, 4, 5 ]		// 
			,[ -1, 0, 1, 2, 3, 4 ]		//	The product of these matrices:
			,[ -2,-1, 0, 1, 2, 3 ]		// 
			,[ -3,-2,-1, 0, 1, 2 ]		// 
			,[ -4,-3,-2,-1, 0, 1 ]]		//		[ 0 -9  0  5  1  7 ]
										//		[ 0 -8 -1  4  1  6 ]
Mat2::Mat								//		[ 0 -7 -2  3  1  5 ]
Mat2 	= 	[[  0, 1, 0, 0, 0,-1 ]		//		[ 0 -6 -3  2  1  4 ]
			,[  1, 0, 1, 1, 0, 1 ]		//		[ 0 -5 -4  1  1  3 ]
			,[ -1, 0, 1,-1, 0, 0 ]		// 		[ 0 -4 -5  0  1  2 ]
			,[ -1,-1,-1, 0,-1, 0 ]		// 
			,[  1, 0, 1, 0, 1, 0 ]		// 
			,[  0,-1,-1, 1, 0, 1 ]]		// 


//	Multiplying two matrices.

MulMat::Mat Mat -> Mat
MulMat m1 m2   =  TMulMat m1 (Transpose m2)

TMulMat::Mat TMat  -> Mat
TMulMat [r] m2    =  [ MulRow r m2 ]
TMulMat [r:rs] m2 =  [ MulRow r m2 : TMulMat rs m2 ]

MulRow::Row TMat -> Row
MulRow r [c]    =  [ Inprodukt r c ]
MulRow r [c:cs] =  [ Inprodukt r c : MulRow r cs ]

Inprodukt::Row Col  -> Int
Inprodukt [] []         =	0
Inprodukt [a:as] [b:bs] =	a * b  +  Inprodukt as bs 

//	Transposing a matrix.

Transpose::Mat -> TMat
Transpose m   =  Transp m 1

Transp::Mat Index_new -> TMat
Transp m i	| i == Size =  [ Column m i ]
						=  [ Column m i : Transp m (i + 1) ]

Column::Mat Index_new -> Col
Column [] i      =  []
Column [r:rs] i  =  [ Select r i : Column rs i ]

Select::Row Index_new -> Int
Select [a:as] 1  =  a
Select [a:as] i  =  Select as (i - 1)

//	The Start rule: show the initial matrices and their product.

Start::(Mat,String,Mat,String,Mat)
Start 	= (m1,"\ntimes\n",m2,"\nbecomes\n",MulMat m1 m2)
where 
  	m1	= Mat1; m2 = Mat2
  			 