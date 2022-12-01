module tak

/*
The Takeuchi function.

To generate an application for this program the Clean 0.8
application should be set to at least 1.1 Mb. To launch the
generated application another 150K of free memory is needed.
*/

import StdClass; // RWS
import StdInt

Tak::Int Int Int -> Int
Tak x y	z | x<=y	=  z
					=  Tak	(Tak (dec x) y z)
							(Tak (dec y) z x)
							(Tak (dec z) x y)

Start::Int
Start = Tak 24 16 8
