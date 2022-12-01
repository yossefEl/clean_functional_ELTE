module pt8
import StdEnv



/*
    Youssef ELMOUMEN
    Neptun : XXXXXX
	Define a class called "Plus" of any type and define the binary operator ++++ for integer type which will 
            add the operands if both are even and 
            subtract them if both odd, 
            otherwise, don't do anything, results 0.
*/


class Plus a
where 
	 (++++) :: a a -> Int
	
     
instance Plus Int
	where  
		(++++) :: Int Int -> Int 
		(++++) n1 n2 
                    |  isEven n1 && isEven n2 = n1 + n2
                    |  isOdd n1 && isOdd n2 = n1 - n2
                    = 0

// Start = 5 ++++ 3	// 2
// Start = 5 ++++ 4	// 0
// Start = 6 ++++ 4	// 10
//Start = -4 ++++ 6   // 2
//Start = -4 ++++ -5  // 0
//Start = -3 ++++ -7	// 4