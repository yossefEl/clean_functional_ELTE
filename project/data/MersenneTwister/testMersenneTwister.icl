module testMersenneTwister

import MersenneTwister, StdEnv

testIntervals :: [Real] Int Real -> [Int]
testIntervals list num wid
	= map (\y -> length (filter (\x -> (x>y) && (x<=y+wid)) 
		(take num list))) [0.0, wid .. (1.0-wid)] 
// Tests, if the random numbers are uniformly distributed over
// evenly spaced intervals of [0,1].
// Input a list of random numbers, the number of random 
// numbers you are interested in and the width of the intervals
// to obtain a list of counts for each interval. 

Start = testIntervals (genRandReal 4357) 10000 0.1
