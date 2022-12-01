module practice
import StdEnv

/* Youssef ELMOUMEN  | Neptun ID XXXXXX */

/* Student recently learned Fibonacci numbers, but he got bored of the problems.
	So, he decided to try his own new sequence:
		f(0) = a
		f(1) = b
		f(n) = f(n-1) ^ f(n-2) where ^ denotes bitxor operation (Hint: bitxor operator can be found in the documentation)
	Given a, b, and n, make a function to help him calculate the nth number in his sequence.
	For example:
	
*/

//diffFibonacchi :: Int Int Int -> Int

diffFibonacchi a b n = if n == 0 then a else if n == 1 then b else bitxor (diffFibonacchi a b (n-1)) (diffFibonacchi a b (n-2))


//Start = diffFibonacchi 86 77 15 // = 86
diffFibonacchi 86 77 15



/* Given three numbers, determine if any of the two numbers addition's sum of digits produces a lucky number.
A lucky number is a number whose sum of digits is divisible by 3 */

isThereLucky :: Int Int Int -> Bool

isThereLucky a b c = if (mod (sumOfDigits a) 3 == 0) || (mod (sumOfDigits b) 3 == 0) || (mod (sumOfDigits c) 3 == 0) then True else False

Start = isThereLucky 1 2 3 // = False
isThereLucky 1 2 3

//isThereLucky :: Int Int Int -> Bool