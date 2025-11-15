-- Week 1: Advanced Exercises
-- Focus: Tuples, complex recursion, combining multiple concepts
-- These are harder than the basic exercises!

{-
  INSTRUCTIONS:
  1. Read the README_ADVANCED.md for tuple syntax and worked examples
  2. Implement each function below
  3. Test with the test file (tests_advanced.hs)
  4. These are challenging - don't get discouraged!
-}

-- ============================================================================
-- Exercise 1: Working with Pairs
-- ============================================================================
-- Concepts: Tuples, pattern matching on pairs
-- Difficulty: Medium
-- Scala Equivalent: def swap[A,B](pair: (A,B)): (B,A) = pair match { case (a,b) => (b,a) }

{-
Problem:
Write a function that swaps the elements of a pair.

Examples:
  swapPair (1, 2)       --> (2, 1)
  swapPair ("hello", 5) --> (5, "hello")
  swapPair (True, 'a')  --> ('a', True)

Hint: Pattern match on (x, y) and return (y, x)

Type Signature: (a, b) -> (b, a)
-}

swapPair :: (a, b) -> (b, a)
swapPair (x, y) = (y ,x)


-- ============================================================================
-- Exercise 2: Zip - Combining Two Lists
-- ============================================================================
-- Concepts: Recursion on multiple lists simultaneously, creating pairs
-- Difficulty: Medium-Hard
-- Scala Equivalent: list1.zip(list2) (but implement manually)

{-
Problem:
Implement the zip function that combines two lists into a list of pairs.
Stop when either list runs out of elements.

Examples:
  zipLists [1,2,3] ['a','b','c']     --> [(1,'a'),(2,'b'),(3,'c')]
  zipLists [1,2] ['a','b','c','d']   --> [(1,'a'),(2,'b')]
  zipLists [1,2,3] []                --> []
  zipLists [] [1,2,3]                --> []

Hint: You need to pattern match on BOTH lists
      - If either is empty, return []
      - Otherwise: cons (x,y) onto the zipped tails

Type Signature: [a] -> [b] -> [(a, b)]
-}

zipLists :: [a] -> [b] -> [(a, b)]
zipLists _ [] = [] 
zipLists [] _ = []
zipLists (x : xs) (y : ys) = (x , y) : zipLists xs ys


-- ============================================================================
-- Exercise 3: Take N Elements
-- ============================================================================
-- Concepts: Guards + recursion, counting down
-- Difficulty: Medium
-- Scala Equivalent: list.take(n) (but implement manually)

{-
Problem:
Take the first n elements from a list. If n is greater than the list length,
return the whole list. If n <= 0, return empty list.

Examples:
  takeN 3 [1,2,3,4,5]  --> [1,2,3]
  takeN 2 ['a','b']    --> ['a','b']
  takeN 5 [1,2]        --> [1,2]
  takeN 0 [1,2,3]      --> []
  takeN (-1) [1,2,3]   --> []

Hint: Two base cases - n <= 0 OR empty list
      Recursive case: cons x onto (take (n-1) xs)

Type Signature: Int -> [a] -> [a]
-}

takeN :: Int -> [a] -> [a]
takeN _ [] = []
takeN n (x : xs) 
  | n <= 0 = []
  | otherwise = x : takeN (n-1) xs



-- ============================================================================
-- Exercise 4: Reverse a List
-- ============================================================================
-- Concepts: Recursion with append, building lists in reverse
-- Difficulty: Medium
-- Scala Equivalent: list.reverse (but implement manually)

{-
Problem:
Reverse a list using recursion.

Examples:
  reverseList [1,2,3]    --> [3,2,1]
  reverseList "hello"    --> "olleh"
  reverseList [1]        --> [1]
  reverseList []         --> []

Hint: Empty list reverses to []
      For (x:xs): reverse xs, then append x at the END
      Use ++ to append: reverseList xs ++ [x]

Note: This is inefficient (quadratic) - you'll learn better ways later

Type Signature: [a] -> [a]
-}

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x : xs) = reverseList(xs) ++ [x]


-- ============================================================================
-- Exercise 5: Drop N Elements
-- ============================================================================
-- Concepts: Guards + recursion, counting down without building
-- Difficulty: Medium
-- Scala Equivalent: list.drop(n) (but implement manually)

{-
Problem:
Drop the first n elements from a list and return the rest.
If n >= list length, return empty list.

Examples:
  dropN 2 [1,2,3,4,5]  --> [3,4,5]
  dropN 3 [1,2]        --> []
  dropN 0 [1,2,3]      --> [1,2,3]
  dropN 5 [1,2,3]      --> []

Hint: If n <= 0, return the whole list
      If list is empty, return []
      Otherwise: dropN (n-1) xs  (don't cons x!)

Type Signature: Int -> [a] -> [a]
-}

dropN :: Int -> [a] -> [a]
dropN _ [] = []
dropN n (x : xs) 
  | n <= 0 = x : xs
  | otherwise = drop (n-1) xs 


-- ============================================================================
-- Exercise 6: Unzip - Split Pairs Into Two Lists
-- ============================================================================
-- Concepts: Recursion on list of tuples, building two lists simultaneously
-- Difficulty: Hard
-- Scala Equivalent: list.unzip (but implement manually)

{-
Problem:
Split a list of pairs into two separate lists.

Examples:
  unzipPairs [(1,'a'),(2,'b'),(3,'c')]  --> ([1,2,3], ['a','b','c'])
  unzipPairs [(True, 0), (False, 1)]    --> ([True,False], [0,1])
  unzipPairs []                         --> ([], [])

Hint: Empty list unzips to ([], [])
      For ((x,y):rest):
        - Recursively unzip rest to get (xs, ys)
        - Return (x:xs, y:ys)

Type Signature: [(a, b)] -> ([a], [b])
-}

unzipPairs :: [(a, b)] -> ([a], [b])
unzipPairs [] = ([], [])
unzipPairs ((x, y) : rest) = (x : xs, y : ys)
  where (xs, ys) = unzipPairs rest


-- ============================================================================
-- Exercise 7: Map Over Pairs
-- ============================================================================
-- Concepts: Higher-order thinking, applying functions to tuple elements
-- Difficulty: Medium-Hard
-- Scala Equivalent: pair match { case (a,b) => (f(a), g(b)) }

{-
Problem:
Apply two functions to the elements of a pair.
The first function is applied to the first element,
the second function to the second element.

Examples:
  mapPair (+1) (*2) (5, 3)      --> (6, 6)
  mapPair not length (True, "hi") --> (False, 2)

Hint: Pattern match on the pair (x, y)
      Return (f x, g y)

Type Signature: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
-}

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair fun1 fun2 (x, y) = (fun1 x, fun2 y)


-- ============================================================================
-- BONUS Exercise 8: List Difference (Remove First Occurrence)
-- ============================================================================
-- Concepts: Recursion, equality checking, guards
-- Difficulty: Hard
-- Scala Equivalent: list.diff(List(elem)) for single element

{-
Problem:
Remove the first occurrence of an element from a list.
If the element doesn't exist, return the original list.

Examples:
  removeFirst 3 [1,2,3,4,3]  --> [1,2,4,3]
  removeFirst 'a' "banana"   --> "bnana"
  removeFirst 5 [1,2,3]      --> [1,2,3]
  removeFirst 1 []           --> []

Hint: Pattern match on the list
      Use guards to check if x == elem
      If yes: return xs (skip this element)
      If no: cons x onto removeFirst elem xs

Type Signature: Eq a => a -> [a] -> [a]
-}

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst el (x : xs) 
  | el == x = xs 
  | otherwise = x : removeFirst el xs 


-- ============================================================================
-- BONUS Exercise 9: Partition - Split by Predicate
-- ============================================================================
-- Concepts: Recursion, guards, building two lists simultaneously
-- Difficulty: Very Hard
-- Scala Equivalent: list.partition(p) (but implement manually)

{-
Problem:
Split a list into two lists: elements that satisfy a predicate and elements that don't.
Return a tuple (satisfied, notSatisfied).

Examples:
  partitionBy (> 5) [1,10,3,8,5,12]      --> ([10,8,12], [1,3,5])
  partitionBy even [1,2,3,4,5,6]         --> ([2,4,6], [1,3,5])
  partitionBy (== 'a') "banana"          --> ("aaa", "bnn")

Hint: Base case: empty list partitions to ([], [])
      For (x:xs):
        - Recursively partition xs to get (yeses, nos)
        - If predicate x is True: return (x:yeses, nos)
        - Otherwise: return (yeses, x:nos)

Type Signature: (a -> Bool) -> [a] -> ([a], [a])
-}

partitionBy :: (a -> Bool) -> [a] -> ([a], [a])
partitionBy fun [] = ([], [])
partitionBy fun (x : rest) 
  | fun x = (x : xs, ys) 
  | otherwise = (xs, x : ys) 
    where (xs, ys) = partitionBy fun rest


-- ============================================================================
-- TESTING YOUR SOLUTIONS
-- ============================================================================

{-
To test your functions:

1. Load the test file:
   :load tests_advanced.hs

2. Run all tests:
   runAllTests

3. Or test individual functions in GHCi:
   swapPair (1, 2)
   zipLists [1,2,3] ['a','b','c']
   takeN 3 [1,2,3,4,5]
-}
