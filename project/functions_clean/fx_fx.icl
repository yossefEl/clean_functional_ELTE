// module fx_fx
// import StdEnv
// func :: Int -> Int
// func x = x+1

// Start = id (func) // the function identity : returns the name of the function
// Start = const (func) // _f0
// Start = flip fx a b // Flips the arguments of a function. This is useful in function compositions.
// Start  = twice func 1  // runs the function twice
// Start = while ((<=)23) func 2  // runs the function until it returns 0 whilte (fx-> bool ) fx2
// Start =until ((<)100 ) func 1  // runs the function until it returns 1