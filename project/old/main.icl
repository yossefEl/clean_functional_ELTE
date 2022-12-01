module main
import StdEnv


// Youssef ELMOUMEN  | Neptun ID XXXXXX

diffFibonacchi :: Int Int Int -> Int
diffFibonacchi a b n
| n == 0 = a
| n == 1 = b
| otherwise = bitxor(diffFibonacchi (a b (n-1))  diffFibonacchi (a b (n-2))) 

//Start = diffFibonacchi 86 77 15 //86


sumOfDigits :: Int -> Int
sumOfDigits n
| n < 10 = n
| otherwise = n rem 10 + sumOfDigits (n / 10)


isThereLucky :: Int Int Int -> Bool
isThereLucky a b c 
| ((sumOfDigits a + b ) rem 3 == 0 ||  (sumOfDigits a+c ) rem 3 == 0 || (sumOfDigits c+b) rem 3 == 0) == True 
= False
Start = isThereLucky 1 2 3 // = False

// create a function that takes 2 integers and return the XOR of them (bitwise XOR)
bitwiseXOR :: Int Int -> Int
bitwiseXOR a b //6 5
| a == 0 = b
| b == 0 = a
| otherwise = bitwiseXOR (a / 2) (b / 2) * 2 + (a rem 2 + b rem 2) rem 2

