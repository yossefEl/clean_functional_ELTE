module ex_record_array
import StdEnv


//// Records - from lecture slides

:: Person = { name :: String
            , birthdate :: (Int,Int,Int)
            , fpprogramer :: Bool
            }

IsfpUser :: Person -> String
IsfpUser {fpprogramer = True} = "Yes"
IsfpUser _                    = "No"

//Start = IsfpUser { name = "Me"
  //               , birthdate = (1,1,1999)
  //               , fpprogramer = True}    // "Yes"

:: Q = { nom :: Int
       , den :: Int
       } 
       
QZero :: Q       
QZero = { nom = 0, den = 1 }    
QOne :: Q
QOne = { nom = 1, den = 1 }

simplify :: Q -> Q
simplify {nom=n,den=d}
  | d == 0 = abort " denominator is 0"
  | d < 0  = { nom = ~n/g, den = ~d/g}
  | otherwise =  { nom = n/g, den = d/g}
  where g = gcd n d

mkQ :: Int Int -> Q
mkQ n d = simplify { nom = n, den = d } 

//Start = mkQ 81 90

MyArray :: {Int}
MyArray = {1,3,5,7,9} 

//Start = MyArray
//Start = MyArray.[2] // 5

MapArray1 :: (Int -> Int) {Int} -> {Int}
MapArray1 f a = {f e \\ e <-: a}

//Start :: {Int}
//Start = MapArray1 inc MyArray


// Exercises


// 1. Create arrays using comprehensions for the followings:

// - powers of 10 from 1st to the 10th
arr :: {Int}
arr = {10^x \\ x<-[1..10]}

//Start :: {Int} 
//Start = arr

// - {(0,0),(1,1),..., (10,10)}
arr1 :: {(Int,Int)}
arr1 = {(x,x) \\ x<-[0..10]}

//Start :: {(Int,Int)}
//Start = arr1

//Start :: (Int, Int)
//Start = arr1.[3]

// - one number at its half, and so on until is 0 
// e.g. {100, 50, 25, 12, 6, 3, 1} 
f :: Int -> [Int] 
f n = takeWhile ((<)0) (iterate (\ x= x/2) n)

//Start = f 100 
arr2 :: Int -> {Int}
arr2 n = {x \\ x<-f n}

//Start :: {Int}
//Start = arr2 100


// 2. Write a function to test the equality of rational numbers.
IsEqual :: Q Q -> Bool
IsEqual q1 q2 = q1.nom * q2.den == q1.den * q2.nom

//Start = mkQ 81 90
//Start = IsEqual (mkQ 81 90) (mkQ 9 10)
//Start = IsEqual { nom = 10, den = 10 } QOne
//Start = IsEqual { nom = 1, den = 2 } { nom = 10, den = 101 }


// 3. Given the University algebraic type of 3 university names
// and the Student record type of id, university and list of grades.
// Select from an array of students the ones that have more then n grades,
// and return a list of (id,uni) pairs of such students.


:: University = Elte | Corvinus | BME

:: Student={id::Int
           ,uni::University
           ,grades::[Int]}

moreThanNGrades :: {Student} Int -> [(Int,University)]
moreThanNGrades a n = map (\x=(x.id,x.uni)) (filter (\x=length (x.grades)>n) list)
where
	list = [x \\ x<-: a]


moreThanNGrades1 :: {Student} Int -> [(Int,University)]
moreThanNGrades1 a n = [(x.id,x.uni) \\ x <-: a | length (x.grades)>n]

arrSt = {{id=1,uni=Elte,grades=[]},{id=2,uni=BME,grades=[5,5,5]},{id=3,uni=Corvinus,grades=[5,5,5,5]}}

//Start :: {Student}
//Start = arrSt

Start = moreThanNGrades arrSt 1
