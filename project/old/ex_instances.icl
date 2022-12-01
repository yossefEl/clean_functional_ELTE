module ex_instances

import StdEnv

// Instances: Defining the behaviour of some types for different operators

instance + String
where
   (+) s1 s2 = s1 +++ s2
     
//Start = "Hello "+"World"



// 1. Make an instance of the operator - for 
// lists of Int such that [1,2,3]-[2,2,2,3]=[1]

instance - [Int]
where
    (-) a b = [x \\ x<-a | not (isMember x b)]
     
//Start :: [Int]
//Start = [1,2,3]-[2,2,2,3]	



// 2. Write an instance of operator + for 
// lists of Int such that [1,2,3]+[2,2,2,3]=[3,4,5]

instance + [Int]
where
    (+) a b = [x+y \\ x<-a & y<-b]

//Start :: [Int]
//Start = [1,2,3]+[2,2,2,3]	



// 3. Create an * instance of lists such that list1 * list2 will give 
// a list of pairwise product of the two lists and if the length 
// of one list is greater than the other one just add the remaining 
// elements to the end of the new list.

fun :: [a] [a] -> [a] | * a
fun [] [] = []
fun a b
| length a >= length b= [x*y\\ x<-a & y<-b] ++( drop (length b) a)
= fun b a

instance * [a] | * a
where
	(*) a b = fun a b
	
//Start= [1,2]*[3,4,5,6,0]//[3,8,5,6,0]
// Start= [1,2,3,1,3,12,312] *[2,3]//[2,6,3,1,3,12,312]
//Start :: [Int]
//Start = [] * []//[]



:: University = {uniName::String,students::[Student],teachers::[Teacher]}
:: Teacher = {name::String,subject::String}
:: Student = {studentName::String,age::Int,grades::{Int},favoriteTeacher::Teacher}

ELTE::University
ELTE={uniName="ELTE",students=[Marko,Nikola,Josh,Dame],teachers=[Mary,Peter,John]}
BMI::University
BMI={uniName="BMI",students=[Ana,Josh,Sofi,Nikola],teachers=[Viktor,John,Peter]}
EmptyUni::University
EmptyUni={uniName="Empty",students=[],teachers=[]}

Peter::Teacher
Peter={name="Peter",subject="Functional"}
Viktor::Teacher
Viktor={name="Viktor",subject="Math"}
Mary::Teacher
Mary={name="Mary",subject="OOP"}
John::Teacher
John={name="John",subject="Functional"}

Marko::Student
Marko={studentName="Marko",age=19,grades={4,4,4,5},favoriteTeacher= Mary}
Sofi::Student
Sofi={studentName="Sofi",age=22,grades={5,5,4,5,5},favoriteTeacher=John}
Dame::Student
Dame={studentName="Dame",age=21,grades={2,3,4,5},favoriteTeacher=Peter}
Ana::Student
Ana={studentName="Ana",age=18,grades={5,5,5,5},favoriteTeacher=Viktor}
Nikola::Student
Nikola={studentName="Nikola",age=19,grades={4,4,4,4,2},favoriteTeacher=Peter}
Nik::Student
Nik={studentName="Nik",age=20,grades={4,4,4,4,3},favoriteTeacher=Peter}
Nik2::Student
Nik2={studentName="Nik2",age=22,grades={4,4,4,4,5},favoriteTeacher=Peter}
Josh::Student
Josh={studentName="Josh",age=22,grades={4,5,5},favoriteTeacher=John}



// 4. Given a University, return an array of all the 
// students names which have gpa greater than 4, 
// and a favorite teacher who teaches Functional.

gpaAndFavoriteTeacher :: University -> {String}
gpaAndFavoriteTeacher uni = {y.studentName \\ y<-uni.students | (toReal(sum [a\\a<-:y.grades]))/(toReal(length [a\\a<-:y.grades]))>4.0 && y.favoriteTeacher.subject=="Functional"}

//Start=gpaAndFavoriteTeacher BMI//{"Josh","Sofi"}
//Start=gpaAndFavoriteTeacher ELTE//{"Josh"}
//Start=gpaAndFavoriteTeacher EmptyUni//{}



// 5. Given a University, return an array of all the 
// students or teachers names which are shorter than 6.

shorterThan6 :: University -> {String}
shorterThan6 uni ={b \\ b <-[y.studentName\\y<-(uni.students)|length[a\\a<-:y.studentName]<6]++[y.name\\y<-(uni.teachers)|length[a\\a<-:y.name]<6]}

//Start=shorterThan6 BMI//{"Ana","Josh","Sofi","John","Peter"}
//Start=shorterThan6 ELTE//{"Marko","Josh","Dame","Mary","Peter","John"}
//Start=shorterThan6 EmptyUni//{}



// 6. Write a function which will take an array of Universities 
// and return the University with the highest overall gpa 
// (the average of the average of each student).

highestGpa::{University}->String
highestGpa unis
|length x == 0="No universities given"
= (last (sort x)).uniName
where x = [y\\y<-:unis]

instance < University 
where 
 (<) a b = calcGpa a < calcGpa b

instance == University 
where 
 (==) a b = calcGpa a == calcGpa b

calcGpa::University->Real
calcGpa a
|length a.students ==0=0.0
=(sum [studentGpa y\\y<-a.students])/toReal(length a.students)

studentGpa::Student->Real 
studentGpa stud = toReal(sum x)/(toReal (length x))
where x = [y\\y<-:(stud.grades)]

//Start = studentGpa Nikola

//Start=highestGpa {ELTE,BMI,EmptyUni}//"BMI"
//Start=highestGpa {ELTE,BMI} //"BMI"
//Start=highestGpa {EmptyUni,EmptyUni}//"Empty"
//Start=highestGpa {ELTE} //"ELTE"
//Start=highestGpa {}//"No universities given"



// 7. Write an instance of type Student, such that two students are equal if 
// their gpa differs in less than 0.3 and they have the same favorite teacher.

instance == Student
where 
	(==) a b = (abs (studentGpa a -studentGpa b))<0.3 && a.favoriteTeacher.name==b.favoriteTeacher.name

//Start= Nikola == Nik//True
//Start=Nikola == Nikola //True
//Start= Nik== Nik2//False
//Start= Nikola == Nik2//False



// 8. Create a toString instance for Student such that for given student 
// ex. Nikola={studentName="Nikola",age=19,grades={4,4,4,4,2},favoriteTeacher=Peter} 
// it gives "Nikola 3.6 Peter" where 3.6 is the student's gpa and 
// Peter is the student's favorite teacher's name.

instance toString Student
where toString a=a.studentName+++"  "+++toString (studentGpa a)+++"  "+++a.favoriteTeacher.name

//Start=toString Nikola//"Nikola  3.6  Peter"
//Start=toString Marko//"Marko  4.25  Mary"
//Start=toString Nik//"Nik  3.8  Peter"
//Start=toString Dame//"Dame  3.5  Peter"



:: Tree a = Node a (Tree a) (Tree a) | Leaf

instance == (Tree a) | == a
where
    (==) Leaf Leaf = True
    (==) (Node x1 l1 r1) (Node x2 l2 r2) = and[x1==x2, l1==l2, r1==r2]
    (==) _ _ = False

specialTree :: (Tree Int)
specialTree = Node 10 (Node 4 (Node 1 (Node 0 Leaf Leaf)(Node 2 Leaf Leaf))(Node 5 Leaf (Node 6 Leaf Leaf)))(Node 15 (Node 12 (Node 11 Leaf Leaf)(Node 13 Leaf Leaf))(Node 17 (Node 16 Leaf Leaf)(Node 19 (Node 18 Leaf Leaf)(Node 20 Leaf Leaf))))

notPrime :: Int -> Bool
notPrime x
| x <= 1 = True
= not(isEmpty[n\\n<-[2..(x-1)]|x rem n == 0])



// 9. Please write a function that, given a Tree and a predicate,
// will find nodes that do not return True for the predicate
// and will remove those nodes and their subtrees.
// Note: The expected return results are listed below with an equality
// for your convenience, so that you do not have to manually check your result.
// If your result is correct, the Start statement should return a True.

pruneTree :: (Tree a) (a -> Bool) -> (Tree a)
pruneTree Leaf pred = Leaf
pruneTree (Node x l Leaf) pred 
|pred x = (Node x (pruneTree l pred) Leaf)
=Leaf
pruneTree (Node x Leaf r) pred 
|pred x = (Node x Leaf (pruneTree r pred))
=Leaf
pruneTree (Node x l r) pred 
|pred x = (Node x (pruneTree l pred) (pruneTree r pred))
=Leaf

//Start = specialTree
//Start = pruneTree specialTree isEven == (Node 10 (Node 4 Leaf Leaf) Leaf) //True
//Start = pruneTree specialTree ((<)7) == (Node 10 Leaf (Node 15 (Node 12 (Node 11 Leaf Leaf) (Node 13 Leaf Leaf)) (Node 17 (Node 16 Leaf Leaf) (Node 19 (Node 18 Leaf Leaf) (Node 20 Leaf Leaf))))) //True
//Start = pruneTree specialTree notPrime == (Node 10 (Node 4 (Node 1 (Node 0 Leaf Leaf) Leaf) Leaf) (Node 15 (Node 12 Leaf Leaf) Leaf)) //True



// 10. Implement the following methods of the Dictionary ADT.
// -keysNum -valueForKey -insert -remove

:: Dictionary a b :== [(a,b)]
dict :: Dictionary String Int
dict =[("first",23),("second",234234),("third",21231)]
dict2 :: Dictionary String Int
dict2 =[("a",1)]

// a) keysNum - Calculate the number of keys in the dictionary 

keysNum :: (Dictionary String Int) -> Int
keysNum [] = 0
keysNum [x:xs] = 1 + keysNum xs
keysNum1 d = length d

//Start = keysNum dict // 3
//Start = keysNum dict2 // 1


//	b) valueForKey - Gives back the value associated with a given key.
//	If the key is not in the dictionary return "The key is not in the dictionary"

valueForKey :: (Dictionary String Int) String -> Int
valueForKey [] str = abort "The key is not in the dictionary"
valueForKey [x:xs] str
|fst x == str = snd x
= valueForKey xs str 

//Start=valueForKey dict "first" // 23
//Start=valueForKey dict "firstt" // The key is not in the dictionary


//	c) insert-Inserts a new tuple if the key value is not in the dictionary already,
//	or give back "The given key already exists" if the key is already in the dictionary

insert :: (Dictionary String Int) (String,Int)->(Dictionary String Int)
insert [x:xs] (z,y)
| length (filter (\(a,b) = a == z) [x:xs]) == 1 = abort "The given key already exists" 
= [x:xs] ++ [(z,y)]

//Start  = insert dict ("third",12312)//"The given key already exists"
//Start = insert dict ("fourth",1)//[("first",23),("second",234234),("third",21231),("fourth",1)]


//	d) remove-remove the (key, value) pair for a given key.
//	If the key is not in the dictionary return "The key is not in the dictionary"

remove::(Dictionary String Int) String->(Dictionary String Int)
remove [x:xs] str
| length (filter (\(a,b) = a == str) [x:xs]) == 0 = abort "The key is not in the dictionary"
 = [a \\ a <- [x:xs] | fst a <> str]

Start=remove dict "first"//[("second",234234),("third",21231)]
//Start=remove dict "someOtherKey"//The key is not in the dictionary


// TO DO
// 1. Implement the Q mathematical set in the given file.

// 2. Finish the implementation of the Stack 

// 3. Build a FIFO ADT with the same list of the operations like for the stack.
// Chose a representation for the type and give the implementation details.

