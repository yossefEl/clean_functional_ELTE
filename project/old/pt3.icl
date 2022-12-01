module pt3
import StdEnv

// Write your Neptun code here:                      


/* Given 2 dimentional list of Reals,
sum all the numbers inside sublist and only keep the numbers if the fraction part is equal to zero. 
Example [[1.2, 1.3, 1.5], [3.53, 53.42, 53.21]] => [4.0, 110.16] => [4] 
Hint: You can use map function (not required)    */
sumReal :: [Real] -> Real
sumReal [] = 0
sumReal [x:xs] = x + sumReal xs

floor :: Real -> Integer
// change it to string and split it by dot
// take the second part and check if it is 0
// if it is 0 then return the first part
// else return 0
floor x = 
    let 
        str = show x
        splitted = split str "."
        second = splitted[1]
        first = splitted[0]
    in
        if second == "0" then first else 0


getFraction :: Real -> Real
getFraction x = x - ( floor x)


Start = getFraction 1.2


// pt3 :: [[Real]] -> [Int]
// pt3 []=[]
// pt3 [x:xs]
//     | ((foldr (+) 0 x) rem 1) ==0 = pt3 [(foldr (+) 0 x),xs]
//     | otherwise = pt3 xs


// Start = pt3 [[1.2, 1.3, 1.5], [3.53, 53.42, 53.21]] // [4]
// // //Start = pt3 [[1.2, 1.8, 3.9], [4.8, 7.9, 6.7], [6.9]] // []