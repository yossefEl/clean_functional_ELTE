# Utility for compiling and running [Clean programming language programs](http://clean.cs.ru.nl/Clean).
### Created by
[Youssef ELMOUMEN](https://www.linkedin.com/in/youssef-elmoumen/) @ [Eötvös Loránd University - Faculty Of Informatics](https://www.elte.hu/) <br> 
Date: 2022.09.30<br>
Version: 1.0.0<br>

### Description

This repository is a collection of quizzes and homework of Clean programming language, there is also a utility I created for windows and mac/linux for those who don't like/have Clean IDE


### Usage
### Linux or Mac

`
./build.sh [project_name]
`

Example<br>

`
./build.sh HelloWorld
`
<br>

### Windows
`
.\build.bat [project_name]
`

Example<br>

`
.\build.bat HelloWorld
`

# Clean Programming Language 🫧


## Util functions

```haskell
isPrime :: Int -> Bool
isPrime x = length [y \\ y <- [1..x] | x rem y == 0] == 2
```

## Char functions

```haskell
Start = 'z'-'a'
Start = 'a'+'d'
Start = toChar '1' does nothing
Start = toChar 68 //shows the relevant char in the ascii table 
Start = toUpper 'a' //retuns the upper case of the char
Start = toLower 'A' //returns the lower case of the char
Start = digitToInt '3' // returns the int value of the char if it is a digit, if the char is a letter it returns the ascii value 
Start = isLower 'a' // returns true if the char is lower case
Start = isUpper 'a' // returns false if the char is lower case
Start = isDigit '1' // returns true if the char is a digit 0-9
Start = isAlpha 'a'//checks if the char is a letter
Start = isAscii '-'//checks if the char is in the ascii table
Start = isControl '\t'//checks if the char is a control char like \t \n \r or \b 
Start = isSpace ' ' // checks if the char is a space char like ' ' or '\t' or '\n' or '\r' or '\f' or '\v'
Start = isPrint 'a'//checks if the char is printable
Start = isOctDigit '9'//checks if the char is an octal digit 0-7
Start = isHexDigit 'G' //checks if the char is a hex digit 0-9 a-f A-F
Start = isAlphanum '?'//checks if the char is a letter or a digit 0-9 , a-z , A-Z
```

## Int functions

```haskell
Start = 2^3//2^3 the power operator is right associative
Start = sign -5 // return the sign of the number -1 for negative, 0 for zero, 1 for positive
Start = ~39 // perator is right associative
Start = lcm 12 18 // least common multiple
Start = gcd 12 18 // greatest common divisor
Start = 2 rem 2 // remainder 
Start = isEven 2 // isEven is a function that returns true if the number is even
Start = isOdd 2 // isOdd is a function that returns true if the number is odd
Start = 1 bitand 2 // bitwise and operator 
Start = 1 bitor 2 // bitwise or operator
Start = 1 bitxor 2 // bitwise xor operator
Start  = 2<<3 // left shift operator 00000010 << 3 = 00001000
Start = 2>>3 // right shift operator 00000010 >> 3 = 00000000
Start = toInt("-hAello-") // 0
Start = bitnot 10 // bitwise not operator
```

## Real functions

```haskell
Start = 1.3 + 2.3 
Start - Real
Start zero Real
Start * Real
Start / Real
Start one Real
Start ^ Real
Start abs Real
Start sign Real
Start ~ Real
Start == Real
Start < Real
Start =ln 1.3
Start = entier 1.33333333
Start = Infinity :== 1E9999
Start= NaN  :== 1E9999+(-1E9999)
Start = isNaN 29932 //x :== if (x==x) False True
Start isInfinity x :== if (abs x==1E9999) True False
Start isFinite x :== if (x-x==0.0) True False
```

## String functions

```haskell
Start = toString Int
Start = toString Char
Start = toString Real
Start = toString Bool
Start = toString {#Char}
Start = fromString {#Char}
Start = % {#Char}
Start = +++ {#Char}
Start = "jedve" := (4,'A')	// update the nth element of a string
Start = "jedve" % (3,4)	// ve  : extract a substring from a string (start, end)
```

## List functions

```haskell
Start = length : 'a list -> int'
Start = Start = toString [1,2,3]// "1,2,3"
Start = Start = fromString "[1,2,3]"// "1,2,3"
Start = flatten [[1,2],[3,4]]// [1,2,3,4]
Start = isEmpty []// true 
Start = hd [1,2,3]// 1 it returns the first element of the list
Start = tl [1,2,3]// [2,3] it returns the tail of the list
Start = Start =init [1,2,3]// [1,2] ? it returns the list without the last element
Start = last [1,2,3]// 3 it returns the last element of the list
Start = Start = take 2 [1,2,3,4]// [1,2] it returns the first n elements of the list
Start = Start = drop 2 [1,2,3,4]// [3,4] it returns the list without the first n elements
Start = takeWhile func [1,2,3,4]// [1,2] it returns the first elements of the list that satisfy the predicate
Start = dropWhile func [1,2,3,4]// [3,4] it returns the list without the first elements that satisfy the predicate

Start = Start = span ((>=)1) [1,2,3,4]// [[1,2],[3,4]] it returns a pair of lists, the first one contains the first elements that satisfy the predicate, Start = the second one contains the rest of the list

Start = Start = filter isEven [1,2,3,4]// [2,4] it returns the elements of the list that satisfy the predicate
Start = reverse [1,2,3]// [3,2,1] it returns the list in reverse order
Start = insert 1 [2,3,4]// [1,2,3,4] it inserts an element in the list
Start = insertAt 1 2 [3,4,5]// [3,2,4,5] it inserts an element in the list at the given position,if the list is empty it returns the list with the element

Start = removeAt 1 [2,3,4]// [2,4] it removes the element at the given position
Start = updateAt 1 2 [3,4,5]

Start =splitAt 2 [2,3,4]// ([2],[3,4]) it splits the list at the given position

Start = foldl (+) 1 [1,2,3]// 6 it folds the list from the left. what is folding? it applies a function to the first two elements of the list, then it applies the function to the result and the third element of the list, and so on

Start = foldr (+) 1 [1,2,3]// 7 it folds the list from the right foldl vs foldr ? foldl is tail recursive, foldr is not , tail recursive means that the last thing that the function does is to call itself

Start = indexList [1,3,4,2,3]// [0,1,2,3,4] it returns a list of indexes

Start = iterate ((+)1) 0 // [1,2,3,4,5,6,7,8,9,10....inf] it returns an infinite list of repeated applications of a function to a value

Start = map ((+)1) [1,2,3]// [2,3,4] it applies a function to all the elements of the list with lambda
Start = map (\x = x+4) [1,2,3]// [2,3,4] it applies a function to all the elements of the list

Start = repeatn 3 [1,2,3]// [[1,2,3],[1,2,3],[1,2,3]] it repeats a list n times
Start = repeat 3 // [3,3,4....inf] it returns an infinite list of repeated elements

Start = scan (+) 10 [1,-1,0,3]// [10,11,10,10,13] it returns a list of successive reduced values from the left ? in other words it returns the list of the intermediate results of the foldl function

Start = unzip [(1,2),(3,4)]// ([1,3],[2,4]) it returns a pair of lists, the first one contains the first elements of the pairs, the second one contains the second elements of the pairs

Start = zip ([1,2,3],[4,5,6])// [(1,4),(2,5),(3,6)] it returns a list of pairs, the first element of the pair is the first element of the first list, the second element of the pair is the first element of the second list, and so on

Start = zip2 [1,2,3] [4,5,6]// [(1,4),(2,5),(3,6)] it returns a list of pairs, the first element of the pair is the first element of the first list, the second element of the pair is the first element of the second list, and so on

Start = diag3  [1,2] [7,8] [2,3]// [(1,7,2),(2,7,2),(1,7,3),(1,8,2),(2,7,3),(2,8,2),(1,8,3),(2,8,3)] it returns a list of triples, the first element of the triple is the first element of the first list, the second element of the triple is the first element of the second list, the third element of the triple is the first element of the third list, and so on

Start = diag2 [1,2] [7,8]// [(1,7),(2,7),(1,8),(2,8)] it returns a list of pairs, the first element of the pair is the first element of the first list, the second element of the pair is the first element of the second list, and so on

Start = and [True,True,False] // false it returns true if all the elements of the list are true
Start = or [True,True,False] // true it returns true if at least one element of the list is true
Start = any ((>)2) [1,2,3] // true it returns true if at least one element of the list satisfies the predicate
Start = all ((>)2) [1,2,3] // false it returns true if all the elements of the list satisfy the predicate
Start = isMember 2 [1,2,3] // true it returns true if the element is a member of the list
Start = isAnyMember [12,4] [1,2,3]  // true it returns true if at least one element of the list is a member of the second list

Start = removeDup  [1,2,3,1,2,3] // [1,2,3] it removes the duplicates from the list

Start = removeMember 2 [1,2,2,2,3,3] // [1,2,2,3,3] it removes the first occurrence of the element from the list

Start = removeMembers  [1,2,2,2,3,3] [2,3]  //  [1,2,2,3]  => removeMembers list membersToRemove it removes the first occurrence of the elements from the list

Start = removeIndex 2 [1,2,3,4] // (1,[1,3,4]) it removes the element at the given position and returns a tuple with the removed element and the new list

Start = limit [1,3,3,2,2,1]//3 Find the first element for which the next element is the same. E.g., limit [1,3,2,2,1,...] = 2

Start =sum [1,2,3]//6 it returns the sum of the elements of the list

Start =prod [1,2,6]//6 it returns the product of the elements of the list equivalent to foldl (*) 1 [1,2,6]

Start = avg [1,2,3]//2 it returns the average of the elements of the list <==> sum/length
```

