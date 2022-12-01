// module fx_list
// import StdEnv

// Start = length : 'a list -> int'
// Start = Start = toString [1,2,3]// "1,2,3"
// Start = Start = fromString "[1,2,3]"// "1,2,3"
// Start = flatten [[1,2],[3,4]]// [1,2,3,4]
// Start = isEmpty []// true 
// Start = hd [1,2,3]// 1 it returns the first element of the list
// Start = tl [1,2,3]// [2,3] it returns the tail of the list
// Start = Start =init [1,2,3]// [1,2] ? it returns the list without the last element
// Start = last [1,2,3]// 3 it returns the last element of the list
// Start = Start = take 2 [1,2,3,4]// [1,2] it returns the first n elements of the list
// Start = Start = drop 2 [1,2,3,4]// [3,4] it returns the list without the first n elements
// Start = takeWhile func [1,2,3,4]// [1,2] it returns the first elements of the list that satisfy the predicate
// Start = dropWhile func [1,2,3,4]// [3,4] it returns the list without the first elements that satisfy the predicate

// Start = Start = span ((>=)1) [1,2,3,4]// [[1,2],[3,4]] it returns a pair of lists, the first one contains the first elements that satisfy the predicate, Start = the second one contains the rest of the list

// Start = Start = filter isEven [1,2,3,4]// [2,4] it returns the elements of the list that satisfy the predicate
// Start = reverse [1,2,3]// [3,2,1] it returns the list in reverse order
// Start = insert 1 [2,3,4]// [1,2,3,4] it inserts an element in the list
// Start = insertAt 1 2 [3,4,5]// [3,2,4,5] it inserts an element in the list at the given position,if the list is empty it returns the list with the element

// Start = removeAt 1 [2,3,4]// [2,4] it removes the element at the given position
// Start = updateAt 1 2 [3,4,5]

// Start =splitAt 2 [2,3,4]// ([2],[3,4]) it splits the list at the given position

// Start = foldl (+) 1 [1,2,3]// 6 it folds the list from the left. what is folding? it applies a function to the first two elements of the list, then it applies the function to the result and the third element of the list, and so on

// Start = foldr (+) 1 [1,2,3]// 7 it folds the list from the right foldl vs foldr ? foldl is tail recursive, foldr is not , tail recursive means that the last thing that the function does is to call itself

// Start = indexList [1,3,4,2,3]// [0,1,2,3,4] it returns a list of indexes

// Start = iterate ((+)1) 0 // [1,2,3,4,5,6,7,8,9,10....inf] it returns an infinite list of repeated applications of a function to a value

// Start = map ((+)1) [1,2,3]// [2,3,4] it applies a function to all the elements of the list with lambda
// Start = map (\x = x+4) [1,2,3]// [2,3,4] it applies a function to all the elements of the list

// Start = repeatn 3 [1,2,3]// [[1,2,3],[1,2,3],[1,2,3]] it repeats a list n times
// Start = repeat 3 // [3,3,4....inf] it returns an infinite list of repeated elements

// Start = scan (+) 10 [1,-1,0,3]// [10,11,10,10,13] it returns a list of successive reduced values from the left ? in other words it returns the list of the intermediate results of the foldl function

// Start = unzip [(1,2),(3,4)]// ([1,3],[2,4]) it returns a pair of lists, the first one contains the first elements of the pairs, the second one contains the second elements of the pairs

// Start = zip ([1,2,3],[4,5,6])// [(1,4),(2,5),(3,6)] it returns a list of pairs, the first element of the pair is the first element of the first list, the second element of the pair is the first element of the second list, and so on

// Start = zip2 [1,2,3] [4,5,6]// [(1,4),(2,5),(3,6)] it returns a list of pairs, the first element of the pair is the first element of the first list, the second element of the pair is the first element of the second list, and so on

// Start = diag3  [1,2] [7,8] [2,3]// [(1,7,2),(2,7,2),(1,7,3),(1,8,2),(2,7,3),(2,8,2),(1,8,3),(2,8,3)] it returns a list of triples, the first element of the triple is the first element of the first list, the second element of the triple is the first element of the second list, the third element of the triple is the first element of the third list, and so on

// Start = diag2 [1,2] [7,8]// [(1,7),(2,7),(1,8),(2,8)] it returns a list of pairs, the first element of the pair is the first element of the first list, the second element of the pair is the first element of the second list, and so on

// Start = and [True,True,False] // false it returns true if all the elements of the list are true
// Start = or [True,True,False] // true it returns true if at least one element of the list is true
// Start = any ((>)2) [1,2,3] // true it returns true if at least one element of the list satisfies the predicate
// Start = all ((>)2) [1,2,3] // false it returns true if all the elements of the list satisfy the predicate
// Start = isMember 2 [1,2,3] // true it returns true if the element is a member of the list
// Start = isAnyMember [12,4] [1,2,3]  // true it returns true if at least one element of the list is a member of the second list

// Start = removeDup  [1,2,3,1,2,3] // [1,2,3] it removes the duplicates from the list

// Start = removeMember 2 [1,2,2,2,3,3] // [1,2,2,3,3] it removes the first occurrence of the element from the list

// Start = removeMembers  [1,2,2,2,3,3] [2,3]  //  [1,2,2,3]  => removeMembers list membersToRemove it removes the first occurrence of the elements from the list

// Start = removeIndex 2 [1,2,3,4] // (1,[1,3,4]) it removes the element at the given position and returns a tuple with the removed element and the new list

// Start = limit [1,3,3,2,2,1]//3 Find the first element for which the next element is the same. E.g., limit [1,3,2,2,1,...] = 2

// Start =sum [1,2,3]//6 it returns the sum of the elements of the list

// Start =prod [1,2,6]//6 it returns the product of the elements of the list equivalent to foldl (*) 1 [1,2,6]

// Start = avg [1,2,3]//2 it returns the average of the elements of the list <==> sum/length