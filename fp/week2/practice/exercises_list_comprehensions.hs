module Exercises_List_Comprehensions
where
-- Week 2: List Comprehensions
-- Focus: Generators, guards, multiple generators, pattern matching in generators
-- Test your functions in GHCi by loading this file with `:load exercises_list_comprehensions.hs`

{-
  INSTRUCTIONS:
  1. Read the worked examples in README_LIST_COMPREHENSIONS.md first
  2. Implement each function using LIST COMPREHENSIONS (not map/filter)
  3. Test with the provided examples in GHCi
  4. Run tests with `:load tests_list_comprehensions.hs` then `runAllTests`
-}

-- ============================================================================
-- Exercise 1: Double Evens
-- ============================================================================
-- Concepts: Guards for filtering, transformation
-- Difficulty: Easy
-- Scala Equivalent: list.filter(_ % 2 == 0).map(_ * 2)

{-
Problem:
Double all even numbers in a list. Remove odd numbers.

Examples:
  doubleEvens [1,2,3,4,5,6]  --> [4,8,12]
  doubleEvens [1,3,5]        --> []
  doubleEvens [2,4,6]        --> [4,8,12]
  doubleEvens []             --> []

Type Signature: [Int] -> [Int]
-}

doubleEvens :: [Int] -> [Int]
doubleEvens list = [x * 2 | x <- list, even x]


-- ============================================================================
-- Exercise 2: Squares Up To N
-- ============================================================================
-- Concepts: Range generation, transformation
-- Difficulty: Easy
-- Scala Equivalent: (1 to n).map(x => x * x).toList

{-
Problem:
Generate a list of squares from 1 to n.

Examples:
  squaresUpTo 5   --> [1,4,9,16,25]
  squaresUpTo 3   --> [1,4,9]
  squaresUpTo 1   --> [1]
  squaresUpTo 0   --> []

Type Signature: Int -> [Int]
-}

squaresUpTo :: Int -> [Int]
squaresUpTo n = [x^2 | x <- [1..n] ]


-- ============================================================================
-- Exercise 3: Factors of N
-- ============================================================================
-- Concepts: Guards with mod, range generation
-- Difficulty: Easy-Medium
-- Scala Equivalent: (1 to n).filter(n % _ == 0).toList

{-
Problem:
Find all factors of a positive integer n.
A factor is a number that divides n evenly (no remainder).

Examples:
  factors 12  --> [1,2,3,4,6,12]
  factors 7   --> [1,7]
  factors 1   --> [1]
  factors 24  --> [1,2,3,4,6,8,12,24]

Hint: x is a factor of n if n `mod` x == 0

Type Signature: Int -> [Int]
-}

factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0] 


-- ============================================================================
-- Exercise 4: Coordinate Grid
-- ============================================================================
-- Concepts: Multiple generators, tuple creation
-- Difficulty: Medium
-- Scala Equivalent: for { r <- 0 until rows; c <- 0 until cols } yield (r, c)

{-
Problem:
Generate all coordinate pairs for a grid of given rows and columns.
Rows and columns are 0-indexed.

Examples:
  grid 2 3  --> [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
  grid 3 2  --> [(0,0),(0,1),(1,0),(1,1),(2,0),(2,1)]
  grid 1 1  --> [(0,0)]
  grid 0 5  --> []

Type Signature: Int -> Int -> [(Int, Int)]
-}

grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x,y) | x <- [0..n-1], y <- [0..m-1]]


-- ============================================================================
-- Exercise 5: Pairs That Sum To N
-- ============================================================================
-- Concepts: Multiple generators with guard, avoiding duplicates
-- Difficulty: Medium
-- Scala Equivalent: for { x <- 1 to n; y <- x to n if x + y == n } yield (x, y)

{-
Problem:
Find all pairs (x, y) of positive integers where x + y = n and x <= y.
The constraint x <= y avoids duplicate pairs like (2,3) and (3,2).

Examples:
  pairsSumTo 5   --> [(1,4),(2,3)]
  pairsSumTo 10  --> [(1,9),(2,8),(3,7),(4,6),(5,5)]
  pairsSumTo 3   --> [(1,2)]
  pairsSumTo 2   --> [(1,1)]


Type Signature: Int -> [(Int, Int)]
-}

pairsSumTo :: Int -> [(Int, Int)]
pairsSumTo n = [(x, y) | x <- [1..n], y <- [x..n], x + y == n]


-- ============================================================================
-- Exercise 6: Concat (Flatten)
-- ============================================================================
-- Concepts: Nested generators, flattening structure
-- Difficulty: Medium
-- Scala Equivalent: lists.flatten

{-
Problem:
Flatten a list of lists into a single list using list comprehensions.

Examples:
  concat' [[1,2], [3,4,5], [6]]  --> [1,2,3,4,5,6]
  concat' [[], [1], [], [2,3]]  --> [1,2,3]
  concat' []                    --> []
  concat' [[1,2,3]]             --> [1,2,3]

Type Signature: [[a]] -> [a]
-}

concat' :: [[a]] -> [a]
concat' list = [y | x <- list, y <- x]


-- ============================================================================
-- Exercise 7: Pythagorean Triples
-- ============================================================================
-- Concepts: Multiple generators with complex guard, mathematical constraint
-- Difficulty: Medium-Hard
-- Scala Equivalent: for { a <- 1 to n; b <- a to n; c <- b to n if a*a + b*b == c*c } yield (a,b,c)

{-
Problem:
Find all Pythagorean triples (a, b, c) where a^2 + b^2 = c^2, and all values <= n.
To avoid duplicates, ensure a <= b <= c.

Examples:
  pyths 5   --> [(3,4,5)]
  pyths 10  --> [(3,4,5),(6,8,10)]
  pyths 15  --> [(3,4,5),(5,12,13),(6,8,10),(9,12,15)]
  pyths 2   --> []

Type Signature: Int -> [(Int, Int, Int)]
-}

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a,b,c) | a <- [1..n], b <- [a..n], c <- [b..n], a^2 + b^2 == c^2]


-- ============================================================================
-- Exercise 8: Perfect Numbers
-- ============================================================================
-- Concepts: Using helper functions in guards, mathematical definition
-- Difficulty: Medium-Hard
-- Scala Equivalent: (1 to n).filter(x => (1 until x).filter(x % _ == 0).sum == x)

{-
Problem:
Find all perfect numbers up to n.
A perfect number equals the sum of its proper divisors (factors excluding itself).

Examples:
  perfects 30    --> [6,28]
  perfects 500   --> [6,28,496]
  perfects 5     --> []

Example: 6 is perfect because 1 + 2 + 3 = 6
         28 is perfect because 1 + 2 + 4 + 7 + 14 = 28

Type Signature: Int -> [Int]
-}

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n-1], mod n x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (divisors x) == x]




-- ============================================================================
-- Exercise 9: Positions
-- ============================================================================
-- Concepts: Pattern matching with zip, finding indices
-- Difficulty: Medium
-- Scala Equivalent: list.zipWithIndex.filter(_._1 == target).map(_._2)

{-
Problem:
Find all positions (indices) of a value in a list.
Indices are 0-based.

Examples:
  positions 3 [1,2,3,4,3,5,3]  --> [2,4,6]
  positions 'a' "banana"       --> [1,3,5]
  positions 5 [1,2,3,4]        --> []
  positions 1 [1,1,1]          --> [0,1,2]

Type Signature: Eq a => a -> [a] -> [Int]
-}

positions :: Eq a => a -> [a] -> [Int]
positions el list = [idx | (x, idx) <- zip list [0..length list], x == el]


-- ============================================================================
-- Exercise 10: Scalar Product (Dot Product)
-- ============================================================================
-- Concepts: Pattern matching on pairs from zip
-- Difficulty: Medium
-- Scala Equivalent: xs.zip(ys).map { case (x, y) => x * y }.sum

{-
Problem:
Calculate the scalar product (dot product) of two lists.
Multiply corresponding elements and sum the results.

Examples:
  scalarProduct [1,2,3] [4,5,6]  --> 32  (1*4 + 2*5 + 3*6)
  scalarProduct [1,0,1] [1,1,1]  --> 2   (1*1 + 0*1 + 1*1)
  scalarProduct [] []            --> 0
  scalarProduct [2,3] [4,5]      --> 23  (2*4 + 3*5)


Type Signature: [Int] -> [Int] -> Int
-}

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct l1 l2 = sum [x * y | (x, y) <- zip l1 l2]


-- ============================================================================
-- BONUS Exercise 11: Quicksort (List Comprehension Version)
-- ============================================================================
-- Concepts: Classic functional quicksort, partition with comprehensions
-- Difficulty: Medium
-- Scala Equivalent: Functional quicksort

{-
Problem:
Implement quicksort using list comprehensions for partitioning.
This is the classic "beautiful" Haskell quicksort.

Algorithm:
1. Empty list is sorted
2. Take first element as pivot
3. smaller = all elements <= pivot
4. larger = all elements > pivot
5. Recursively sort and concatenate

Examples:
  qsort [3,1,4,1,5,9,2]  --> [1,1,2,3,4,5,9]
  qsort "haskell"        --> "aehklls"
  qsort []               --> []
  qsort [1]              --> [1]

Type Signature: Ord a => [a] -> [a]
-}

qsort :: Ord a => [a] -> [a]
qsort = undefined


-- ============================================================================
-- BONUS Exercise 12: Replicate Elements
-- ============================================================================
-- Concepts: Multiple generators, using element in range
-- Difficulty: Medium
-- Scala Equivalent: list.flatMap(x => List.fill(n)(x))

{-
Problem:
Replicate each element n times using list comprehensions.

Examples:
  replicateEach 3 [1,2,3]   --> [1,1,1,2,2,2,3,3,3]
  replicateEach 2 "ab"      --> "aabb"
  replicateEach 0 [1,2,3]   --> []
  replicateEach 1 [5,6]     --> [5,6]

Type Signature: Int -> [a] -> [a]
-}

helper :: Int -> a -> [a]
helper 0 el = []
helper factor el = el : helper (factor-1) el 


replicateEach :: Int -> [a] -> [a]
replicateEach factor list = [el | x <- list, el <- helper factor x]


-- ============================================================================
-- TESTING YOUR SOLUTIONS
-- ============================================================================

{-
To test your functions:

1. Open GHCi:
   ghci

2. Load this file:
   :load exercises_list_comprehensions.hs

3. Test individual functions:
   doubleEvens [1,2,3,4,5,6]
   factors 12
   pyths 15

4. Load and run all tests:
   :load tests_list_comprehensions.hs
   runAllTests

5. If you make changes, reload:
   :reload
-}
