module pt4
import StdEnv

//XXXXXX     
/* 2 students are playing dice game. In reach round they throw a dice, their results given in a
list. Student win if their sum of throws is higher. Determine which student will win.
Return 0 - draw
	   1 - 1st student wins
	   2 - 2nd student wins
Example:
student A, student B = [[1,2],[5,6],[3,6]] = (9, 14) = 2 (student B wins)
 */

sums :: [[Int]] -> [Int] 
sums [] = [0,0]
sums [xs,zs,ys] = [x+y+x \\ x<-xs & y<-ys & z<-zs]

// Start = sums [[1,2],[5,6],[3,6]]

pt4 :: [[Int]] -> Int
pt4 x  
| (hd (sums x) ) > (last (sums x))  = 1
| (hd (sums x)) < (last (sums x)) = 2
= 0



// Start =pt4 [[1,2],[5,6],[3,6]] // 2
// Start = pt4 [[1,4], [5,2], [4,4], [3,1]] // 1
Start = pt4 [[4,2], [2,4]] // 0
