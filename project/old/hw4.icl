module hw4
import StdEnv

// Name			: Youssef ELMOUMEN
// Neptun code	: XXXXXX

/* Two lists are given. The first list contains instructive elements of what to do with the second list
1st list of char : [x,y,z] -> x => how many times to implement y function
					 		  y => the function / possible values '+' '*' '/'
					 		  z => value to use in the function
					 		  
For example if the first list is ['2', '+', '4'], it means add 4 to each element of the second list
2 times. So if the second list is [3, 10, 35] the result will be 
[3 + 4 + 4, 10 + 4 + 4, 39 + 4 + 4] = [11, 18, 47]

Try to use higher order function if possible. */


expr :: Int Char Int -> Int
expr a ex b
|ex=='+'= a + b
|ex=='/' && b<>0 =a/b
|ex=='*'= a * b 
=abort "Unsupported operator or division by zero error"

integerToChar :: Int -> Char
integerToChar n = toChar(n+48)

calc :: [Char] [Int] -> [Int]
calc [] [] =[]
calc x [] =abort "An non empty array expected"
calc [] x =abort "List of instructions expected"
calc [nbTimes,op,value] list 
|digitToInt(nbTimes) == 0 = list
= calc [integerToChar(digitToInt(nbTimes)-1),op,value]  [expr (digitToInt value ) op x \\ x<- list ]

// Start = calc ['2', '+', '4'] [3, 10, 35] // [11, 18, 43]
// Start = calc ['0', '*', '1'] [24, 35, 56] //= [24,35,56]

/* In two dimensional list, you are given balances of clients of a bank. Each list 
represents the activities of one client. The first number is the starting balance of this month, 
and the following numbers are the deposits(+N) and the withdrawals(-N). Calculate their 
closing balance, and depending on the amount calculate their interest and add it and return in single list

Criterias of interest calculation:
if balance <= 30'000 : interest = 1%
if balance > 30'000 and balance <= 100'000  : interest = 5%
if balance > 100'000 and balance <= 200'000  : interest = 8%
if balance > 200'000 : interest = 10% (You can round up in case interest is a fractional number)


Example: interest [[50000, 900, -4000, 80000], [900, -800, 9000, 5000]] => (closing balance) [[126900], [14100]] => (interest added) [137052, 14241] */


interestCalc :: Int -> Int
interestCalc balance
|balance <= 30000 = balance + (balance * 1) / 100
|balance > 30000 && balance <= 100000 = balance + (balance * 5) / 100
|balance > 100000 && balance <= 200000 = balance + (balance * 8) / 100
|balance > 200000 = balance + (balance * 10) / 100

interest :: [[Int]] -> [Int]
interest balances
| length balances == 0 = []
| otherwise = [interestCalc (foldr (+)  0 x) \\ x <- balances]



// Start = interest [[0]] // [[0]]
Start = interest [[200000, -9000, 45000, -4578], [100000, 7895, -6782], [45936, -3792, 7849, 3739], [3543, 8953, -4932]] // [254564, 109202, 56419, 7640]