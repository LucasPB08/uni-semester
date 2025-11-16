-- Week 2: Advanced Exercises
-- Focus: Folding, complex composition, combining multiple concepts
-- These are significantly harder than the basic exercises!

{-
  INSTRUCTIONS:
  1. Read README_ADVANCED.md for foldr/foldl explanations
  2. Implement each function below
  3. Test with tests_advanced.hs
  4. Challenge yourself - don't peek at hints too quickly!
-}

-- ============================================================================
-- Exercise 1: Sum Using Fold
-- ============================================================================
-- Concepts: foldr, basic fold pattern
-- Difficulty: Medium
-- Scala Equivalent: list.foldRight(0)(_ + _)

{-
Problem:
Implement sum using foldr. Don't use recursion directly.

Examples:
  sumList [1,2,3,4]   --> 10
  sumList []          --> 0
  sumList [5]         --> 5
  sumList [-1,1]      --> 0

Hint: foldr takes a combining function, an initial value, and a list
      For sum: combining function is (+), initial value is 0
      sumList = foldr (+) 0

Type Signature: [Int] -> Int
-}

sumList :: [Int] -> Int
sumList = foldr (+) 0


-- ============================================================================
-- Exercise 2: Length Using Fold
-- ============================================================================
-- Concepts: foldr with lambda, ignoring values
-- Difficulty: Medium
-- Scala Equivalent: list.foldRight(0)((_, acc) => acc + 1)

{-
Problem:
Implement length using foldr. Don't use recursion directly.

Examples:
  lengthList [1,2,3]      --> 3
  lengthList []           --> 0
  lengthList "hello"      --> 5
  lengthList [1]          --> 1

Hint: For each element, add 1 to the accumulator
      Don't care about the element value, just count
      lengthList = foldr (\x acc -> 1 + acc) 0
      Or: lengthList = foldr (\_ acc -> acc + 1) 0

Type Signature: [a] -> Int
-}

lengthList :: [a] -> Int
lengthList = foldr (\x y -> 1 + y) 0


-- ============================================================================
-- Exercise 3: Map Using Fold
-- ============================================================================
-- Concepts: foldr, building lists
-- Difficulty: Medium-Hard
-- Scala Equivalent: list.foldRight(Nil: List[B])((x, acc) => f(x) :: acc)

{-
Problem:
Implement map using foldr. Apply a function to each element.

Examples:
  mapList (*2) [1,2,3]       --> [2,4,6]
  mapList (+10) [1,2,3]      --> [11,12,13]
  mapList reverse ["ab","cd"] --> ["ba","dc"]
  mapList (^2) []            --> []

Hint: For each element x, apply f to it and cons onto accumulator
      mapList f = foldr (\x acc -> f x : acc) []

Type Signature: (a -> b) -> [a] -> [b]
-}

mapList :: (a -> b) -> [a] -> [b]
mapList fun = foldr (\el acc -> fun el : acc) []


-- ============================================================================
-- Exercise 4: Filter Using Fold
-- ============================================================================
-- Concepts: foldr, conditional accumulation
-- Difficulty: Medium-Hard
-- Scala Equivalent: list.foldRight(Nil)((x, acc) => if (p(x)) x :: acc else acc)

{-
Problem:
Implement filter using foldr. Keep only elements that satisfy a predicate.

Examples:
  filterList even [1,2,3,4]       --> [2,4]
  filterList (> 5) [3,6,2,8,1]    --> [6,8]
  filterList (/= ' ') "a b c"     --> "abc"
  filterList (const True) [1,2,3] --> [1,2,3]

Hint: For each element, check predicate
      If true, cons onto accumulator; if false, skip it
      filterList pred = foldr (\x acc -> if pred x then x : acc else acc) []

Type Signature: (a -> Bool) -> [a] -> [a]
-}

filterList :: (a -> Bool) -> [a] -> [a]
filterList pred = foldr (\el acc -> if pred el then el : acc else acc) []


-- ============================================================================
-- Exercise 5: Reverse Using foldl
-- ============================================================================
-- Concepts: foldl, flip, building in reverse
-- Difficulty: Hard
-- Scala Equivalent: list.foldLeft(Nil)((acc, x) => x :: acc)

{-
Problem:
Implement reverse using foldl. Don't use foldr or manual recursion.

Examples:
  reverseList [1,2,3]     --> [3,2,1]
  reverseList "hello"     --> "olleh"
  reverseList []          --> []
  reverseList [1]         --> [1]

Hint: Use foldl to cons elements in reverse order
      reverseList = foldl (flip (:)) []

      Why flip? Because:
      - (:) has type: a -> [a] -> [a]  (element first, list second)
      - foldl gives us: acc -> element
      - flip (:) gives: [a] -> a -> [a] (list first, element second)

Type Signature: [a] -> [a]
-}

reverseList :: [a] -> [a]
reverseList = foldl (\acc el -> el : acc) []


-- ============================================================================
-- Exercise 6: Maximum Using Fold
-- ============================================================================
-- Concepts: foldl, handling non-empty lists
-- Difficulty: Medium
-- Scala Equivalent: list.tail.foldLeft(list.head)(math.max)

{-
Problem:
Find the maximum element in a non-empty list using foldl.
You can assume the list is non-empty.

Examples:
  maximumList [3,7,2,9,1]     --> 9
  maximumList [5]             --> 5
  maximumList [-1,-5,-3]      --> -1
  maximumList [1,2,3,4,5]     --> 5

Hint: Use foldl with max function
      Start with the first element as initial value
      maximumList (x:xs) = foldl max x xs

Type Signature: [Int] -> Int
-}

maximumList :: [Int] -> Int
maximumList (x:xs) = foldl max x xs


-- ============================================================================
-- Exercise 7: Concatenate Lists Using Fold
-- ============================================================================
-- Concepts: foldr, list concatenation
-- Difficulty: Medium
-- Scala Equivalent: list.foldRight(Nil)(_ ++ _)

{-
Problem:
Flatten a list of lists into a single list using foldr.

Examples:
  concatLists [[1,2], [3,4], [5]]     --> [1,2,3,4,5]
  concatLists [[], [1], [], [2,3]]    --> [1,2,3]
  concatLists ["hello", "world"]      --> "helloworld"
  concatLists []                      --> []

Hint: Use foldr with (++) as the combining function
      concatLists = foldr (++) []

Type Signature: [[a]] -> [a]
-}

concatLists :: [[a]] -> [a]
concatLists = undefined


-- ============================================================================
-- Exercise 8: Function Pipeline - Composition Only
-- ============================================================================
-- Concepts: Function composition (.), point-free style
-- Difficulty: Medium-Hard
-- Scala Equivalent: Multiple andThen/compose calls

{-
Problem:
Create a function that:
1. Filters out odd numbers
2. Squares each remaining number
3. Sums the results

Use ONLY function composition (.) - no explicit parameters!

Examples:
  sumOfSquaresOfEvens [1,2,3,4,5]   --> 20  ([2,4] -> [4,16] -> 20)
  sumOfSquaresOfEvens [1,3,5]       --> 0   (no evens)
  sumOfSquaresOfEvens [2,4,6]       --> 56  ([2,4,6] -> [4,16,36] -> 56)
  sumOfSquaresOfEvens []            --> 0

Hint: Build the pipeline right-to-left
      sumOfSquaresOfEvens = sum . map (^2) . filter even

Type Signature: [Int] -> Int
-}

sumOfSquaresOfEvens :: [Int] -> Int
sumOfSquaresOfEvens = undefined


-- ============================================================================
-- Exercise 9: Any - Check if Predicate Holds for Any Element
-- ============================================================================
-- Concepts: foldr, short-circuit evaluation
-- Difficulty: Medium
-- Scala Equivalent: list.foldRight(false)((x, acc) => p(x) || acc)

{-
Problem:
Check if ANY element in a list satisfies a predicate.
Use foldr (which can short-circuit with lazy evaluation).

Examples:
  anyList even [1,3,5,4,7]     --> True  (4 is even)
  anyList (> 10) [1,2,3]       --> False
  anyList (== 'a') "hello"     --> False
  anyList (== 'a') "have"      --> True

Hint: Use foldr with (||)
      anyList pred = foldr (\x acc -> pred x || acc) False

Type Signature: (a -> Bool) -> [a] -> Bool
-}

anyList :: (a -> Bool) -> [a] -> Bool
anyList = undefined


-- ============================================================================
-- Exercise 10: All - Check if Predicate Holds for All Elements
-- ============================================================================
-- Concepts: foldr, short-circuit evaluation
-- Difficulty: Medium
-- Scala Equivalent: list.foldRight(true)((x, acc) => p(x) && acc)

{-
Problem:
Check if ALL elements in a list satisfy a predicate.
Use foldr.

Examples:
  allList even [2,4,6,8]       --> True
  allList even [2,4,5,8]       --> False
  allList (> 0) [1,2,3]        --> True
  allList (> 0) [1,-2,3]       --> False

Hint: Use foldr with (&&)
      allList pred = foldr (\x acc -> pred x && acc) True

Type Signature: (a -> Bool) -> [a] -> Bool
-}

allList :: (a -> Bool) -> [a] -> Bool
allList = undefined


-- ============================================================================
-- BONUS Exercise 11: takeWhile Using Fold
-- ============================================================================
-- Concepts: foldr, early termination
-- Difficulty: Hard
-- Scala Equivalent: Manual implementation with foldRight

{-
Problem:
Implement takeWhile using foldr.
Take elements from the front of the list while they satisfy a predicate.
Stop at the first element that doesn't satisfy it.

Examples:
  takeWhileList (< 5) [1,2,3,6,4,2]   --> [1,2,3]
  takeWhileList even [2,4,6,1,8]      --> [2,4,6]
  takeWhileList (/= ' ') "hello world" --> "hello"
  takeWhileList (const True) [1,2,3]  --> [1,2,3]

Hint: For each element, check predicate
      If true AND accumulator is building, cons it
      If false, return empty list (stop building)
      This is tricky! Think about what the accumulator represents.

      takeWhileList pred = foldr (\x acc -> if pred x then x : acc else []) []

Type Signature: (a -> Bool) -> [a] -> [a]
-}

takeWhileList :: (a -> Bool) -> [a] -> [a]
takeWhileList = undefined


-- ============================================================================
-- BONUS Exercise 12: zipWith Using Recursion + Higher-Order
-- ============================================================================
-- Concepts: Recursion, higher-order functions, combining two lists
-- Difficulty: Hard
-- Scala Equivalent: list1.zip(list2).map { case (a, b) => f(a, b) }

{-
Problem:
Implement zipWith - combine two lists element-wise with a function.
Stop when either list runs out.

Examples:
  zipWithList (+) [1,2,3] [10,20,30]       --> [11,22,33]
  zipWithList (*) [1,2,3] [4,5,6]          --> [4,10,18]
  zipWithList (,) [1,2] ['a','b','c']      --> [(1,'a'),(2,'b')]
  zipWithList max [1,5,3] [4,2,6]          --> [4,5,6]

Hint: Use pattern matching on both lists
      zipWithList f (x:xs) (y:ys) = f x y : zipWithList f xs ys
      zipWithList _ _ _ = []

Type Signature: (a -> b -> c) -> [a] -> [b] -> [c]
-}

zipWithList :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithList = undefined


-- ============================================================================
-- BONUS Exercise 13: Complex Pipeline with $ and .
-- ============================================================================
-- Concepts: ($), (.), complex composition
-- Difficulty: Medium-Hard
-- Scala Equivalent: Chained method calls

{-
Problem:
Create a function that processes a list of words:
1. Filter words longer than 3 characters
2. Convert each word to uppercase (use map and a helper function)
3. Sort them (use Data.List.sort - you can import it or assume it exists)
4. Concatenate into a single string with spaces

For this exercise, you can assume you have:
  toUpperStr :: String -> String  (converts string to uppercase)
  sort :: [String] -> [String]    (sorts a list)

Build the function using composition and $.

Examples:
  processWords ["hi", "hello", "world", "bye"]
    --> "HELLO WORLD"
    -- Steps: filter -> ["hello","world","bye"]
    --        uppercase -> ["HELLO","WORLD","BYE"]
    --        sort -> ["BYE","HELLO","WORLD"]
    --        intercalate -> "BYE HELLO WORLD"

Hint: Use composition for the pipeline
      You'll need intercalate " " from Data.List (joins with separator)

      processWords = intercalate " " . sort . map toUpperStr . filter (\w -> length w > 3)

Type Signature: [String] -> String
-}

import Data.Char (toUpper)
import Data.List (sort, intercalate)

toUpperStr :: String -> String
toUpperStr = map toUpper

processWords :: [String] -> String
processWords = undefined


-- ============================================================================
-- BONUS Exercise 14: Fold with Complex Accumulator
-- ============================================================================
-- Concepts: foldl, tuple accumulator, tracking multiple values
-- Difficulty: Very Hard
-- Scala Equivalent: foldLeft with tuple accumulator

{-
Problem:
Count both the number of even and odd integers in a list.
Return a tuple (evenCount, oddCount).
Use foldl with a tuple accumulator.

Examples:
  countEvenOdd [1,2,3,4,5,6]     --> (3, 3)
  countEvenOdd [2,4,6]           --> (3, 0)
  countEvenOdd [1,3,5]           --> (0, 3)
  countEvenOdd []                --> (0, 0)

Hint: Use foldl with initial value (0, 0)
      For each element:
        - If even, increment first count
        - If odd, increment second count

      countEvenOdd = foldl (\(evens, odds) x ->
        if even x then (evens + 1, odds) else (evens, odds + 1)) (0, 0)

Type Signature: [Int] -> (Int, Int)
-}

countEvenOdd :: [Int] -> (Int, Int)
countEvenOdd = undefined


-- ============================================================================
-- BONUS Exercise 15: Implementing groupBy
-- ============================================================================
-- Concepts: foldr, as-patterns, complex recursion
-- Difficulty: Very Hard
-- Scala Equivalent: list.groupBy(f).values.toList

{-
Problem:
Group consecutive equal elements together.
Similar to Data.List.group but implement it yourself.

Examples:
  groupConsecutive [1,1,2,2,2,3,1,1]   --> [[1,1],[2,2,2],[3],[1,1]]
  groupConsecutive "aabbcca"           --> ["aa","bb","cc","a"]
  groupConsecutive [1,2,3]             --> [[1],[2],[3]]
  groupConsecutive []                  --> []

Hint: This is very challenging! Use manual recursion with as-patterns.
      For each element, check if it matches the head of the first group
      If yes, add to that group; if no, start a new group

      Pattern:
      groupConsecutive [] = []
      groupConsecutive (x:xs) = ... use as-pattern to build groups ...

Type Signature: Eq a => [a] -> [[a]]
-}

groupConsecutive :: Eq a => [a] -> [[a]]
groupConsecutive = undefined


-- ============================================================================
-- TESTING YOUR SOLUTIONS
-- ============================================================================

{-
To test your functions:

1. Load this file:
   :load exercises_advanced.hs

2. Test individual functions:
   sumList [1,2,3,4]
   mapList (*2) [1,2,3]

3. Run all tests:
   :load tests_advanced.hs
   runAllTests

4. If you get stuck:
   - Re-read the hints
   - Check README_ADVANCED.md for patterns
   - Try the examples manually in GHCi
   - Break down the problem into smaller steps
-}
