module twice

/*
The Twice function.

Increase stack size and heap size to 1m to run this program.

An integer (0) is incremented 65536 times using the higher
order function Twice.
*/

import StdClass, StdInt

Twice::(x -> x) x -> x
Twice f x =  f (f x)

Start::Int
Start = Twice Twice Twice Twice inc 0
