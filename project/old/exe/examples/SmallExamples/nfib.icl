module nfib

/*
The nfib function.

To obtain maximum performance guards are used instead of
pattern matching.
*/

import StdInt

Nfib::Int -> Int
Nfib n	| n < 2 = 	1
				= 	Nfib (n - 1) + Nfib (n - 2) + 1

Start::Int
Start = Nfib 30
