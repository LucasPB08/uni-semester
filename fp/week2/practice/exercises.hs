-- Week 2: Advanced Patterns and First-Class Functions
-- Focus: As-patterns (@) and functions as first-class values
-- Test your functions in GHCi by loading this file with `:load exercises.hs`

{-
  INSTRUCTIONS:
  1. Read the worked examples in README.md first
  2. Implement each function below
  3. Test with the provided examples in GHCi
  4. Run tests with `:load tests.hs` then `runAllTests`
-}

-- ============================================================================
-- Exercise 1: As-Patterns - List Tails
-- ============================================================================
-- Concepts: As-patterns (@), recursion, list construction
-- Difficulty: Medium
-- Scala Equivalent: Similar to list.tails but manual implementation

{-
Problem:
Generate all tails of a list. A tail is a suffix of the list.
Use as-patterns to avoid rebuilding the list.

Examples:
  tails [1,2,3]     --> [[1,2,3], [2,3], [3], []]
  tails "abc"       --> ["abc", "bc", "c", ""]
  tails []          --> [[]]
  tails [1]         --> [[1], []]

Hint: Use as-pattern all@(x:xs) to capture the whole list
      - Cons 'all' onto recursive call on 'xs'
      - Base case: tails [] = [[]]

Type Signature: [a] -> [[a]]
-}

tails :: [a] -> [[a]]
tails [] = [[]]
tails all@(x : xs) = all : tails xs


-- ============================================================================
-- Exercise 2: First-Class Functions - Apply Twice
-- ============================================================================
-- Concepts: Functions as arguments, function application
-- Difficulty: Easy
-- Scala Equivalent: def applyTwice[A](f: A => A)(x: A): A = f(f(x))

{-
Problem:
Apply a function to a value twice.

Examples:
  applyTwice (+3) 5      --> 11   (5+3=8, 8+3=11)
  applyTwice (*2) 3      --> 12   (3*2=6, 6*2=12)
  applyTwice tail [1,2,3,4] --> [3,4]  (tail twice)
  applyTwice reverse [1,2]  --> [1,2]  (reverse twice = original)

Hint: Apply f to x, then apply f to the result
      applyTwice f x = f (f x)

Type Signature: (a -> a) -> a -> a
-}

applyTwice :: (a -> a) -> a -> a
applyTwice f a = f (f a)


-- ============================================================================
-- Exercise 3: Function Composition - Manual Implementation
-- ============================================================================
-- Concepts: Function composition, higher-order functions
-- Difficulty: Medium
-- Scala Equivalent: (f: B => C).compose(g: A => B): A => C

{-
Problem:
Implement function composition manually (like the . operator).
(compose f g) x should equal f (g x)

Examples:
  let double x = x * 2
      plusOne x = x + 1
  in compose plusOne double 5   --> 11  (5*2=10, 10+1=11)

  let exclaim s = s ++ "!"
      shout s = map toUpper s
  in compose exclaim shout "hello"  --> "HELLO!"  (if toUpper imported)

Hint: compose f g creates a NEW function that takes x
      This new function applies g to x, then f to the result
      Use lambda: \x -> f (g x)

Type Signature: (b -> c) -> (a -> b) -> (a -> c)
-}

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose fun1 fun2 x = fun1 (fun2 x)


-- ============================================================================
-- Exercise 4: Argument Flipping
-- ============================================================================
-- Concepts: Functions returning functions, argument reordering
-- Difficulty: Medium
-- Scala Equivalent: def flip[A,B,C](f: (A, B) => C): (B, A) => C

{-
Problem:
Create a function that flips the order of arguments for a two-argument function.

Examples:
  let subtract x y = x - y
  in flipArgs subtract 5 10  --> 5  (normally 10-5, but flipped to 5-10... wait)

  Actually: subtract 10 5 = 10 - 5 = 5
            flipArgs subtract 5 10 = subtract 10 5 = 10 - 5 = 5

  let divide x y = x / y
  in flipArgs divide 2 10  --> 5  (10 / 2, arguments flipped)

  flipArgs (:) [2,3] 1  --> [1,2,3]  (normally 1:[2,3], flipped)

Hint: flipArgs f creates a new function that takes arguments in reverse order
      flipArgs f = \x y -> f y x

Type Signature: (a -> b -> c) -> (b -> a -> c)
-}

flipArgs :: (a -> b -> c) -> (b -> a -> c)
flipArgs fun a b = fun b a


-- ============================================================================
-- Exercise 5: Apply N Times
-- ============================================================================
-- Concepts: Recursion + higher-order functions, guards
-- Difficulty: Medium-Hard
-- Scala Equivalent: Recursive application of function n times

{-
Problem:
Apply a function n times to a value.
If n <= 0, return the value unchanged.

Examples:
  applyNTimes 3 (+1) 5     --> 8    (5+1+1+1)
  applyNTimes 4 (*2) 1     --> 16   (1*2*2*2*2)
  applyNTimes 2 tail [1,2,3,4] --> [3,4]
  applyNTimes 0 (*10) 5    --> 5    (no applications)
  applyNTimes (-1) (+1) 5  --> 5    (negative = no applications)

Hint: Use guards for base case n <= 0
      Recursive case: apply f once, then apply (n-1) more times
      applyNTimes n f x = applyNTimes (n-1) f (f x)

Type Signature: Int -> (a -> a) -> a -> a
-}

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 fun x = x
applyNTimes n fun x 
  | n <= 0 = x
  | otherwise = applyNTimes (n-1) fun (fun x)


-- ============================================================================
-- Exercise 6: Function Builder - Multiplier
-- ============================================================================
-- Concepts: Functions returning functions, closures
-- Difficulty: Medium
-- Scala Equivalent: def buildMultiplier(n: Int): Int => Int = x => x * n

{-
Problem:
Create a function that builds a "multiply by n" function.

Examples:
  let times3 = buildMultiplier 3
  in times3 5  --> 15

  let times10 = buildMultiplier 10
  in times10 7  --> 70

  buildMultiplier 0 100  --> 0

Hint: Return a lambda function that multiplies its argument by n
      buildMultiplier n = \x -> x * n

Type Signature: Int -> (Int -> Int)
-}

buildMultiplier :: Int -> (Int -> Int)
buildMultiplier n x = n * x


-- ============================================================================
-- Exercise 7: As-Pattern - Duplicate Elements
-- ============================================================================
-- Concepts: As-patterns, recursion
-- Difficulty: Medium
-- Scala Equivalent: list.flatMap(x => List(x, x))

{-
Problem:
Duplicate each element in a list.
Use as-patterns where helpful.

Examples:
  duplicateEach [1,2,3]     --> [1,1,2,2,3,3]
  duplicateEach "ab"        --> "aabb"
  duplicateEach []          --> []
  duplicateEach [5]         --> [5,5]

Hint: Pattern match with (x:xs)
      Cons x twice, then recurse: x : x : duplicateEach xs

Type Signature: [a] -> [a]
-}

duplicateEach :: [a] -> [a]
duplicateEach [] = []
duplicateEach (x:xs) = x : x : duplicateEach xs


-- ============================================================================
-- BONUS Exercise 8: Function List Pipeline
-- ============================================================================
-- Concepts: Folding functions, composition
-- Difficulty: Hard
-- Scala Equivalent: functions.foldLeft(identity)((acc, f) => f compose acc)

{-
Problem:
Given a list of functions, compose them all into a single function.
Apply them right-to-left (like . operator).

Examples:
  let fs = [(+1), (*2), (+10)]
  in pipelineFunctions fs 5  --> 31
  -- Evaluation: ((5 + 10) * 2) + 1 = (15 * 2) + 1 = 30 + 1 = 31

  let fs = [tail, tail]
  in pipelineFunctions fs [1,2,3,4]  --> [3,4]

Hint: Use foldr to compose functions right-to-left
      Start with identity function: id
      Fold with composition: foldr (.) id functions

Type Signature: [a -> a] -> (a -> a)
-}

pipelineFunctions :: [a -> a] -> (a -> a)
pipelineFunctions = foldr (.) id


-- ============================================================================
-- BONUS Exercise 9: Custom Filter Builder
-- ============================================================================
-- Concepts: Functions returning functions, partial application
-- Difficulty: Hard
-- Scala Equivalent: def buildFilter[A](pred: A => Bool): List[A] => List[A]

{-
Problem:
Create a function that takes a predicate and returns a filter function.

Examples:
  let keepEvens = buildFilter even
  in keepEvens [1,2,3,4,5,6]  --> [2,4,6]

  let keepPositive = buildFilter (> 0)
  in keepPositive [-1, 2, -3, 4]  --> [2, 4]

  let keepLong = buildFilter (\s -> length s > 3)
  in keepLong ["hi", "hello", "yo", "world"]  --> ["hello", "world"]

Hint: Return a function that filters a list using the given predicate
      buildFilter pred = \list -> filter pred list
      Or point-free: buildFilter pred = filter pred
      Or even more point-free: buildFilter = filter

Type Signature: (a -> Bool) -> ([a] -> [a])
-}

buildFilter :: (a -> Bool) -> ([a] -> [a])
buildFilter = filter


-- ============================================================================
-- BONUS Exercise 10: Partial Application - Range Checker
-- ============================================================================
-- Concepts: Partial application, functions returning functions
-- Difficulty: Medium
-- Scala Equivalent: def inRange(low: Int)(high: Int)(x: Int): Boolean

{-
Problem:
Create a function that checks if a value is within a range [low, high] (inclusive).
Return a function that performs the check.

Examples:
  let isDigit = inRange 0 9
  in isDigit 5  --> True

  let isTeen = inRange 13 19
  in isTeen 15  --> True

  inRange 10 20 25  --> False
  inRange 10 20 15  --> True

Hint: The function should take low and high, return a function that takes x
      inRange low high = \x -> x >= low && x <= high

Type Signature: Int -> Int -> (Int -> Bool)
                Or equivalently: Int -> Int -> Int -> Bool (due to currying)
-}

inRange :: Int -> Int -> Int -> Bool
inRange lower upper el 
 | lower <= el && el <= upper = True
 | otherwise = False


-- ============================================================================
-- TESTING YOUR SOLUTIONS
-- ============================================================================

{-
To test your functions:

1. Open GHCi:
   ghci

2. Load this file:
   :load exercises.hs

3. Test individual functions:
   tails [1,2,3]
   applyTwice (+1) 5
   compose (+1) (*2) 5

4. Load and run all tests:
   :load tests.hs
   runAllTests

5. If you make changes, reload:
   :reload
-}
