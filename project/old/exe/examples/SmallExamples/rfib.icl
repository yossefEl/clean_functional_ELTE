module rfib

/*
The Nfib function using reals.

To obtain maximum performance guards are used instead of
pattern matching.

To generate an application for this program the Clean 0.8
application should be set to at least 1.1 Mb. To launch the
generated application another 150K of free memory is needed.

On a machine without a math coprocessor the execution of this
program might take a (very) long time. Use a smaller start value.
*/

import StdReal

Nfib::Real -> Real
Nfib n	| n < 1.5	=  1.0
					=	Nfib (n - 1.0) + Nfib (n - 2.0) + 1.0
					  
Start::Real
Start = Nfib 26.0
