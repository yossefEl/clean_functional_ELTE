module stwice

/*
The strict Twice function.

An integer (0) is incremented 65536 times using the higher
order function Twice. The Twice function has a local
strictness annotation which makes it more efficient.

To generate an application for this program the Clean 0.8
application should be set to at least 1.1 Mb. To launch the
generated application another 400K of free memory is needed.
*/

import StdEnv

Twice::(a -> a) a -> a
Twice f x	#! evalfx = f x
			=  f evalfx

Start::Int
Start = Twice Twice Twice Twice inc 0
