module hw5
import StdEnv

/*  
    Name         : Youssef ELMOUMEN
    Neptun Code  : XXXXXX
*/

/* You are given a list of tuples, where each tuple represents a complex number
(a,b) = a + b * i. Please find the absolute value of each complex number/tuple.

absolute value of complex number (a + b * i ) = sqrt(a^2 + b^2) 
Example: (5,6) = (5+6i) => |5+6i| = sqrt(25+36) = 6.40312423743285
 */
clcAbsValue :: Int Int -> Real 
clcAbsValue a b = sqrt ( toReal((abs a)^2) + toReal((abs b)^2 ))

absValue:: [(Int, Int)] -> [Real]
absValue [] = []
absValue list = [ clcAbsValue a b \\ (a,b)<- list] 

// Start = absValue [(5,6), (2,3), (-1, -4), (0,0)] // [7.81024967590665,3.60555127546399,4.12310562561766,0]
//Start = absValue [(3,-4)] // [5]
//Start = absValue [] // []

/* There is a dart competition. We have the x,y coordinate of each player's throw in a tuple.
From the list of players throw positions return the how many points they each earn.

If the dart lands outside the target, player gets 0 point.
If the dart lands in the outer circle,  1 point.
If the dart lands in the middle circle, 5 points.
If the dart lands in inner circle 10 points.

Outer circle has a radius of 10 units, middle circle 5 units, inner circle 1 unit.
The center of the dart will be at (0,0) coordinate. 

Hint : the formula of finding 2 points distance: sqrt((x2-x1)^2 + (y2-y1)^2)) */

calcScore :: Int Int -> Int 
calcScore a b 
|distance <=10 && distance>5 = 1
|distance <=5 && distance>1 = 5
|distance <=1 = 10
=0
where distance = toInt(sqrt( toReal(a^2) + toReal(b^2) )) 
 /* center is (0,0) means x1=0 and y1=0 
    =>  (x2-0)^2 + (y2-0)^2 
    <=> x2^2 + y2^2 
 */

dart :: [(Int, Int)] -> [Int]
dart [] = []
dart coordinates = [ calcScore a b \\ (a,b)<- coordinates] 

Start = dart [(-3,0),(2,2),(2,6),(1,2),(0,0),(-1,0), (7,8)]  
/*
    the number of the coordinates provided is 7
    while the provided result's length is 6 
    ----------------------------------------------
    Example's result    => [ 5, 5, 1, 10, 10, 0 ] 
    My output           => [ 5, 5, 1, 5,  10, 10, 0 ]
                                     (1,2)?
*/