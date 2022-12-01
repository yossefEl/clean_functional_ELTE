module acker

//	The Ackermann function.

import StdInt
    
Acker::Int Int -> Int
Acker 0 j =	j + 1
Acker i 0 = Acker (i - 1) 1
Acker i j = Acker (i - 1) (Acker i (j - 1))

Start::Int
Start = Acker 3 7
