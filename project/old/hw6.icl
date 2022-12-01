module hw6
import StdEnv

/*
    Name        : Youssef ELMOUMEN
    Neptun Code : XXXXXX
*/

/* Given the Major algebraic type of 4 majors and a Student record,
find the student with the highest average grade for each of the major and return their (Major, grade)
pair. The order of the resulting list is not important. */

:: Major = Finance | CS | Math | Physics 


:: Student = {id :: Int, major::Major, grades::[Int]}

sts1 = {
        {id=1,major=Finance,grades=[5,5,5,5,5]}
        ,{id=2,major=CS,grades=[5,4,2,3,5,4,5]}
        , {id=4,major=Physics,grades=[2,3,4,2,5]}
        , {id=5,major=Math,grades=[3,4,5,3,2]}
        , {id=3,major=CS,grades=[5,4,4,4,4]}
        }

sts2 = {
    {id=0, major=Math, grades=[3,4,2,2]}
    }

sts3 = {}

/********* Start Helper functions *********/
instance == Major
where  
    (==) Finance  Finance = True
    (==) CS       CS      = True
    (==) Math     Math    = True
    (==) Physics  Physics = True
    (==) _         _       = False
/********* End Helper functions *********/ 
highestGrades :: {Student} -> [(Real, Major)]
highestGrades students = removeDup maxGradeMajorsPairs 
where 
    maxGradeMajorsPairs=[ (maxList [av \\ (av,maj) <- listAvgs | maj==major], major  )  \\ (average,major) <- listAvgs ]
    listAvgs = [(avg [toReal(gr) \\ gr<- st.grades], st.major) \\ st <-: students] 




// Start = highestGrades sts1 // [(5, Finance), (4.2, CS), (3.2, Physics), (3.4, Math)]
// Start = highestGrades sts2 // [(2.75, Math)]
// Start = highestGrades sts3 // []

/* Given a (Tree Int), write a function which gives back a list that contains the values of the nodes that
has single subtree(which means either the right or the left child is a Leaf) */
:: Tree a = Node a (Tree a) (Tree a) | Leaf
          
Tree1 :: Tree Int
Tree1 = (Node 5 (Node 10 (Node 31 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 17 (Node 31 (Node 14 (Node 12 Leaf Leaf) Leaf) Leaf) (Node 11 Leaf Leaf) ))

Tree2 :: Tree Int
Tree2 = Node 0 (Node 1 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf))  (Node 2 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf)) 

Tree3 :: Tree Int
Tree3 = Node 0 (Node 1 (Node 3 Leaf (Node 8 Leaf Leaf)) Leaf)  (Node 2 Leaf Leaf)
/********* Start Helper functions *********/
isLeaf ::  (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _    = False
/********* End Helper functions *********/
singleNodes :: (Tree Int)-> [Int]
singleNodes Leaf = []
singleNodes (Node x l r) 
| ( isLeaf r && not(isLeaf l)  )  || ( not (isLeaf r) && isLeaf l ) = [x : singleNodes l ++ singleNodes r]
 = singleNodes l ++ singleNodes r

Start = singleNodes Tree1 // [10,31,1,31,14] ===> [1] is Not a single node because it has 2 children of type Leaf
// Start = singleNodes Tree2 // []
// Start = singleNodes Tree3 // [1,3]


