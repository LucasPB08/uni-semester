-- Week 4: Monads and Lazy Evaluation Exercises
-- ===========================================
--
-- Instructions:
-- 1. Replace 'undefined' with your implementation
-- 2. Test in GHCi: :load exercises.hs
-- 3. Run tests: :load tests.hs then main
--
-- Remember:
-- - >>= chains operations where the function returns a wrapped value
-- - Do-notation is syntactic sugar for >>=
-- - Lazy evaluation allows infinite data structures

module Exercises where

import Control.Monad (guard)
import Data.Char (toUpper, isDigit, digitToInt)
-- ============================================================================
-- PART 1: Maybe Monad
-- ============================================================================

-- Exercise 1: Safe List Operations
-- --------------------------------
-- Concepts: Maybe monad, pattern matching, safe operations
-- Difficulty: Easy

-- | Safely get the last element of a list
-- Examples:
--   safeLast [1,2,3] = Just 3
--   safeLast []      = Nothing
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [a] = Just a
safeLast (x:xs) = safeLast xs

-- | Safely get the element at index n (0-indexed)
-- Examples:
--   safeIndex [1,2,3] 1 = Just 2
--   safeIndex [1,2,3] 5 = Nothing
--   safeIndex [] 0      = Nothing
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:xs) 0 = Just x
safeIndex (x:xs) n = safeIndex xs (n-1)


-- Exercise 2: Safe Arithmetic
-- ---------------------------
-- Concepts: Maybe monad, error handling
-- Difficulty: Easy

-- | Safe division (returns Nothing for division by zero)
-- Examples:
--   safeDiv 10 2 = Just 5
--   safeDiv 10 0 = Nothing
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just $ div x y

-- | Safe square root (returns Nothing for negative numbers)
-- Examples:
--   safeSqrt 16.0 = Just 4.0
--   safeSqrt (-1) = Nothing
safeSqrt :: Double -> Maybe Double
safeSqrt x
  | x < 0 = Nothing
  | otherwise = Just $ sqrt x


-- Exercise 3: Chaining Maybe Operations
-- -------------------------------------
-- Concepts: >>= operator, Maybe chaining
-- Difficulty: Medium

-- | Calculate (a / b) / c safely
-- If any division fails, the whole computation fails
-- Examples:
--   chainedDiv 20 2 2 = Just 5
--   chainedDiv 20 0 2 = Nothing
--   chainedDiv 20 2 0 = Nothing
chainedDiv :: Int -> Int -> Int -> Maybe Int
chainedDiv a b c = safeDiv a b >>= \x -> safeDiv x c

chainedDiv a b c = do
  x <- safeDiv a b
  safeDiv x c

-- | Get the head of the tail of the tail (third element, 0-indexed as index 2)
-- Use safeTail and safeHead (define these helper functions or use pattern matching)
-- Examples:
--   thirdElement [1,2,3,4] = Just 3
--   thirdElement [1,2]     = Nothing
--   thirdElement []        = Nothing
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeHead [] = Nothing
safeHead (x:xs) = Just x

thirdElement :: [a] -> Maybe a
thirdElement list = safeTail list >>= \x -> safeTail x >>= \x -> safeHead x 
thirdElement list = do
  x <- safeTail list 
  y <- safeTail x 
  safeHead y

-- Exercise 4: Maybe with Do-Notation
-- ----------------------------------
-- Concepts: do-notation with Maybe, monadic composition
-- Difficulty: Medium

-- | Parse a simple arithmetic expression "a+b" where a and b are single digits
-- Examples:
--   parseAndAdd "3+5" = Just 8
--   parseAndAdd "9+1" = Just 10
--   parseAndAdd "a+b" = Nothing (not digits)
--   parseAndAdd "12"  = Nothing (wrong format)
--   parseAndAdd ""    = Nothing
-- Hint: Use pattern matching on the string and isDigit, digitToInt from Data.Char
parseAndAdd :: String -> Maybe Int
parseAndAdd string = do 
  firstDigit <- safeHead string
  symbol <- safeTail string >>= safeHead
  secondDigit <- safeTail string >>= safeTail >>= safeHead
  guard (symbol == '+')
  guard (isDigit firstDigit && isDigit secondDigit)
  Just (digitToInt firstDigit + digitToInt secondDigit)



-- Exercise 5: User Lookup Chain
-- -----------------------------
-- Concepts: Real-world Maybe chaining, do-notation
-- Difficulty: Medium

type StudentId = Int
type CourseName = String
type Grade = Char

-- Sample data (pretend database)
students :: [(StudentId, String)]
students = [(1, "Alice"), (2, "Bob"), (3, "Charlie")]

enrollments :: [(String, [CourseName])]
enrollments = [("Alice", ["FP", "Security"]), ("Bob", ["FP"]), ("Charlie", [])]

grades :: [(String, CourseName, Grade)]
grades = [("Alice", "FP", 'A'), ("Alice", "Security", 'B'), ("Bob", "FP", 'C')]

-- | Look up a student name by ID
-- Examples:
--   lookupStudent 1 = Just "Alice"
--   lookupStudent 99 = Nothing
lookupStudent :: StudentId -> Maybe String
lookupStudent sid = lookup sid students

-- | Look up courses for a student name
-- Examples:
--   lookupCourses "Alice" = Just ["FP", "Security"]
--   lookupCourses "Unknown" = Nothing
lookupCourses :: String -> Maybe [CourseName]
lookupCourses name = lookup name enrollments

-- | Look up grade for a student in a specific course
-- Examples:
--   lookupGrade "Alice" "FP" = Just 'A'
--   lookupGrade "Alice" "AI" = Nothing
lookupGrade :: String -> CourseName -> Maybe Grade
lookupGrade name course =
  case [g | (n, c, g) <- grades, n == name, c == course] of
    [g] -> Just g
    _   -> Nothing

-- | Get a student's grade in their first enrolled course
-- Chain: StudentId -> Name -> Courses -> First Course -> Grade
-- Examples:
--   getFirstCourseGrade 1 = Just 'A'  (Alice's FP grade)
--   getFirstCourseGrade 2 = Just 'C'  (Bob's FP grade)
--   getFirstCourseGrade 3 = Nothing   (Charlie has no courses)
--   getFirstCourseGrade 99 = Nothing  (student doesn't exist)
-- Hint: You'll need safeHead for the list of courses
getFirstCourseGrade :: StudentId -> Maybe Grade
getFirstCourseGrade sid = do
  name <- lookupStudent sid 
  courses <- lookupCourses name 
  firstCourse <- safeHead courses 
  lookupGrade name firstCourse


-- ============================================================================
-- PART 2: List Monad
-- ============================================================================

-- Exercise 6: Cartesian Products
-- ------------------------------
-- Concepts: List monad, do-notation for combinations
-- Difficulty: Easy

-- | Generate all pairs from two lists
-- Examples:
--   allPairs [1,2] ['a','b'] = [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
--   allPairs [] [1,2] = []
allPairs :: [a] -> [b] -> [(a,b)]
allPairs l1 l2 = [(x, y) | x <- l1, y <- l2]
allPairs l1 l2 = do 
  x <- l1 
  y <- l2 
  return (x, y)

-- | Generate all triples from three lists
-- Examples:
--   allTriples [1,2] ['a'] [True,False] = [(1,'a',True),(1,'a',False),(2,'a',True),(2,'a',False)]
allTriples :: [a] -> [b] -> [c] -> [(a,b,c)]
allTriples l1 l2 l3 = do
  x <- l1
  y <- l2
  z <- l3
  return (x,y,z)


-- Exercise 7: List Monad with Guards
-- ----------------------------------
-- Concepts: guard, filtering in list monad
-- Difficulty: Medium

-- | Find all pairs (x,y) where x is from xs, y is from ys, and x + y == target
-- Examples:
--   pairsThatSum [1,2,3] [4,5,6] 7 = [(1,6),(2,5),(3,4)]
--   pairsThatSum [1,2] [1,2] 3 = [(1,2),(2,1)]
pairsThatSum :: [Int] -> [Int] -> Int -> [(Int, Int)]
pairsThatSum l1 l2 target = do
  x <- l1 
  y <- l2 
  guard (x + y == target)
  return (x , y)

pairsThatSum l1 l2 target = [(x, y) | x <- l1, y <- l2, x + y == target]


-- | Find all factors of n (pairs of numbers that multiply to n)
-- Examples:
--   factors 12 = [(1,12),(2,6),(3,4)]
--   factors 7 = [(1,7)]
--   factors 1 = [(1,1)]
-- Hint: Only include pairs where first <= second to avoid duplicates
factors :: Int -> [(Int, Int)]
factors n = do 
  x <- [1..n]
  y <- [x..n]
  guard (x * y == n)
  return (x, y)


-- Exercise 8: Chess Knight Moves
-- ------------------------------
-- Concepts: List monad for exploring possibilities
-- Difficulty: Medium

type Position = (Int, Int)

-- | Generate all valid positions a knight can move to from current position
-- A knight moves in an "L" shape: 2 squares in one direction, 1 in perpendicular
-- Board is 8x8, positions are (1,1) to (8,8)
-- Examples:
--   knightMoves (1,1) = [(2,3),(3,2)]
--   knightMoves (4,4) = [(2,3),(2,5),(3,2),(3,6),(5,2),(5,6),(6,3),(6,5)]
--   (order may vary, just needs all valid moves)
knightMoves :: Position -> [Position]
knightMoves (x, y) = do 
  funs <- [((+1), (+2)), ((+1), (\x -> x-2)), ((\x -> x-1), (+2)), ((\x -> x-1), (\x -> x-2))]
  let foo = fst funs
  let bar = snd funs
  let firstPosition = (foo x, bar y)
  let secondPosition = (bar x, foo y)
  [(m, n) | (m, n) <- [firstPosition, secondPosition], m >= 1, m <= 8, n >= 1, n <= 8]



-- | Can the knight reach the target position in exactly n moves?
-- Examples:
--   canReachIn 1 (1,1) (2,3) = True
--   canReachIn 1 (1,1) (3,3) = False
--   canReachIn 2 (1,1) (1,1) = True (can return to start)
-- Hint: Use the list monad to explore all paths
canReachIn :: Int -> Position -> Position -> Bool
canReachIn 0 p1 p2 = p1 == p2
canReachIn 1 p1 p2 = elem p1 $ knightMoves p2 
canReachIn n p1 p2 = let moves = knightMoves p2 in 
  or [canReachIn (n-1) p1 move | move <- moves ]

-- Exercise 9: Permutations
-- ------------------------
-- Concepts: List monad, recursion, nondeterminism
-- Difficulty: Hard

-- | Generate all permutations of a list
-- Examples:
--   perms [1,2] = [[1,2],[2,1]]
--   perms [1,2,3] = [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
--   perms "" = [""]
--   length (perms [1,2,3,4]) = 24
-- Hint: For each element, choose it as first, then permute the rest
perms :: [a] -> [[a]]
perms [] = [[]]
perms list = undefined



-- ============================================================================
-- PART 3: Lazy Evaluation
-- ============================================================================

-- Exercise 10: Infinite Sequences
-- -------------------------------
-- Concepts: Lazy evaluation, infinite lists
-- Difficulty: Easy

-- | Infinite list of powers of 2: [1, 2, 4, 8, 16, ...]
-- Examples:
--   take 5 powersOfTwo = [1,2,4,8,16]
--   powersOfTwo !! 10 = 1024
powersOfTwo :: [Integer]
powersOfTwo = map (\el -> 2^el) [0..]

-- | Infinite list of triangular numbers: [1, 3, 6, 10, 15, ...]
-- The nth triangular number is 1 + 2 + ... + n
-- Examples:
--   take 5 triangulars = [1,3,6,10,15]
--   triangulars !! 99 = 5050
-- Hint: Use scanl or zipWith
triangulars :: [Integer]
triangulars = map (\x -> sum [1..x]) [1..]


-- Exercise 11: Fibonacci Variations
-- ---------------------------------
-- Concepts: Self-referential definitions, lazy evaluation
-- Difficulty: Medium

-- | Infinite list of Fibonacci numbers: [0, 1, 1, 2, 3, 5, 8, ...]
-- Use the elegant self-referential definition with zipWith
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- | Infinite list of Lucas numbers: [2, 1, 3, 4, 7, 11, ...]
-- Same recurrence as Fibonacci (each is sum of previous two) but starts with 2, 1
-- Examples:
--   take 7 lucas = [2,1,3,4,7,11,18]
lucas :: [Integer]
lucas = 2 : 1 : zipWith (+) lucas (tail lucas)


-- Exercise 12: Prime Generation
-- -----------------------------
-- Concepts: Lazy evaluation, sieve algorithm
-- Difficulty: Medium

-- | Infinite list of prime numbers using the Sieve of Eratosthenes
-- Examples:
--   take 10 primes = [2,3,5,7,11,13,17,19,23,29]
--   primes !! 100 = 547
primes :: [Int]
primes = 2 : [num | num <- [3..], not (any (\x -> num `mod` x == 0) [2..num-1])]


-- | Check if a number is prime (using the infinite primes list)
-- Examples:
--   isPrime 17 = True
--   isPrime 18 = False
--   isPrime 2 = True
--   isPrime 1 = False
isPrime :: Int -> Bool
isPrime n = n == head (dropWhile (< n) primes)


-- Exercise 13: Collatz Sequence
-- -----------------------------
-- Concepts: iterate, takeWhile, lazy sequences
-- Difficulty: Medium

-- | Generate the Collatz sequence starting from n
-- Rules: if even, divide by 2; if odd, multiply by 3 and add 1
-- Sequence ends when reaching 1
-- Examples:
--   collatz 6 = [6,3,10,5,16,8,4,2,1]
--   collatz 1 = [1]
--   collatz 27 has 112 elements
-- Hint: Use iterate and takeWhile (or a recursive definition)
collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n 
  | even n = n : collatz (div n 2)
  | otherwise = n : collatz (n*3+1)


-- | Length of Collatz sequence (number of steps to reach 1)
-- Examples:
--   collatzLength 6 = 9
--   collatzLength 1 = 1
--   collatzLength 27 = 112
collatzLength :: Integer -> Int
collatzLength n = length (collatz n)


-- Exercise 14: Interleaving Streams
-- ---------------------------------
-- Concepts: Lazy evaluation, combining infinite lists
-- Difficulty: Medium

-- | Interleave two lists: take alternating elements
-- Examples:
--   take 8 (interleave [1,3..] [2,4..]) = [1,2,3,4,5,6,7,8]
--   interleave "abc" "123" = "a1b2c3"
--   interleave [1,2] [3,4,5,6] = [1,3,2,4,5,6]
interleave :: [a] -> [a] -> [a]
interleave [] [] = []
interleave l1 [] = l1
interleave [] l2 = l2 
interleave (x:xs) (y:ys) = x : y : interleave xs ys


-- | Merge two sorted infinite lists into one sorted list
-- Examples:
--   take 10 (merge [1,3..] [2,4..]) = [1,2,3,4,5,6,7,8,9,10]
--   take 10 (merge [1,4,7,10..] [2,5,8,11..]) = [1,2,4,5,7,8,10,11,...]
-- Assumes both input lists are sorted and infinite
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys) 
  | x == y = x : y : merge xs ys 
  | x < y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys


-- ============================================================================
-- PART 4: Combining Monads and Laziness
-- ============================================================================

-- Exercise 15: Safe Operations on Infinite Structures
-- ---------------------------------------------------
-- Concepts: Combining Maybe with lazy evaluation
-- Difficulty: Hard

-- | Find the first element in an infinite list that satisfies the predicate
-- wrapped in Maybe (to handle edge cases with finite lists)
-- Examples:
--   findFirst (> 100) [1..] = Just 101
--   findFirst even [1,3..] = Nothing (infinite odd list - be careful!)
--   findFirst (> 5) [1..10] = Just 6
--   findFirst (> 100) [1..10] = Nothing
-- Note: For truly infinite lists with no match, this won't terminate
-- That's expected - we're not solving the halting problem!
findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst _ [] = Nothing
findFirst f (x:xs) 
  | f x = Just x 
  | otherwise = findFirst f xs 

-- | Safely get the nth prime number (0-indexed)
-- Examples:
--   nthPrime 0 = Just 2
--   nthPrime 4 = Just 11
--   nthPrime (-1) = Nothing


nthPrime :: Int -> Maybe Int
nthPrime n 
  | n < 0 = Nothing
  | otherwise = safeIndex primes n 


-- Exercise 16: Hamming Numbers (Bonus - Hard)
-- -------------------------------------------
-- Concepts: Self-referential definitions, merge, lazy evaluation
-- Difficulty: Hard

-- | Infinite list of Hamming numbers (also called regular numbers)
-- Hamming numbers are numbers of the form 2^i * 3^j * 5^k where i,j,k >= 0
-- In other words: numbers whose only prime factors are 2, 3, and 5
-- The sequence starts: [1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, ...]
-- Examples:
--   take 15 hamming = [1,2,3,4,5,6,8,9,10,12,15,16,18,20,24]
--   hamming !! 1000 = 51200000
-- Hint: Define hamming in terms of itself:
--   hamming = 1 : merge (map (*2) hamming) (merge (map (*3) hamming) (map (*5) hamming))
-- But you need to remove duplicates (use a version of merge that removes dups)
hamming :: [Integer]
hamming = undefined
