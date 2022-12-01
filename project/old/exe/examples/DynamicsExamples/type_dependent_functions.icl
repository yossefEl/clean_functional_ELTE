module type_dependent_functions

// This application shows the use of type dependent functions. The application
// shows 'unwrap: wrong type' because the Start-function expects an integer.

import StdDynamic
import StdEnv

wrap :: a -> Dynamic | TC a
wrap x
	= dynamic x
	
unwrap :: Dynamic -> a | TC a
unwrap (d :: a^)
	= d
unwrap _
	= abort "unwrap: wrong type"
	
Start :: Int
Start
	= unwrap d
where
	d 
		= wrap 1
