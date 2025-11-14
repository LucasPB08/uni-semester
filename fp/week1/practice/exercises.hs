import Data.Text.Lazy.Read (double)
-- Week 1: Haskell Fundamentals
-- Focus: Haskell syntax coming from Scala background
-- Test your functions in GHCi by loading this file with `:load exercises.hs`

{-
  INSTRUCTIONS:
  1. Read the worked examples in README.md first
  2. Implement each function below
  3. Test with the provided examples in GHCi
  4. Compare to the patterns in README examples

  NOTE: You already completed exercises 1 and 2 - nice work!
-}

-- ============================================================================
-- Exercise 1: Hello Functions
-- ============================================================================
-- Concepts: Basic function definition, integer arithmetic
-- Difficulty: Easy

{-
Problem:
Write a function that takes two integers and returns their sum.

Examples:
  add 3 5     --> 8
  add 10 20   --> 30
  add (-5) 15 --> 10

Type Signature: Int -> Int -> Int
-}

add :: Int -> Int -> Int
add x y = x + y  -- Replace 'undefined' with your implementation


-- ============================================================================
-- Exercise 2: Boolean Logic
-- ============================================================================
-- Concepts: Boolean operations, function definition
-- Difficulty: Easy

{-
Problem:
Write a function that takes two boolean values and returns True only if
BOTH of them are True (this is the logical AND operation).

Examples:
  both True True   --> True
  both True False  --> False
  both False False --> False

Type Signature: Bool -> Bool -> Bool
-}

both :: Bool -> Bool -> Bool
both x y = x && y 


-- ============================================================================
-- Exercise 3: Simple Conditions
-- ============================================================================
-- Concepts: Guards, comparisons
-- Difficulty: Easy
-- Scala Equivalent: if/else chain or pattern matching with guards

{-
Problem:
Write a function that takes an integer and returns "positive" if it's greater
than 0, "negative" if it's less than 0, and "zero" if it's exactly 0.

Examples:
  describeNumber 5    --> "positive"
  describeNumber (-3) --> "negative"
  describeNumber 0    --> "zero"

Hint: Similar to the `signum` example in README.md - use guards with |

Type Signature: Int -> String
-}

describeNumber :: Int -> String
describeNumber n 
  | n > 0 = "positive"
  | n < 0 = "negative"
  | otherwise = "zero"


-- ============================================================================
-- Exercise 4: List Recursion - Length
-- ============================================================================
-- Concepts: Pattern matching, recursion, lists
-- Difficulty: Medium
-- Scala Equivalent: list match { case Nil => 0; case _ :: xs => 1 + length(xs) }

{-
Problem:
Implement list length using recursion (don't use built-in 'length').

Think structurally:
- Empty list [] has length 0 (base case)
- List (x:xs) has length 1 + length of xs (recursive case)

Examples:
  countElements []           --> 0
  countElements [1,2,3]      --> 3
  countElements "hello"      --> 5  (strings are lists of Char)

Hint: This is VERY similar to the `sumList` example in README.md
      Pattern match on [] and (x:xs)

Type Signature: [a] -> Int
-}

countElements :: [a] -> Int
countElements [] = 0
countElements (x:xs) = 1 + countElements xs 


-- ============================================================================
-- Exercise 5: List Construction - Range
-- ============================================================================
-- Concepts: Recursion, guards, list building with ++
-- Difficulty: Medium
-- Scala Equivalent: (1 to n).toList (but implement manually with recursion)

{-
Problem:
Create a list [1, 2, 3, ..., n] using recursion.

Examples:
  makeRange 5  --> [1,2,3,4,5]
  makeRange 1  --> [1]
  makeRange 0  --> []

Hint: Check the `range` example in README.md
      - Use guards to handle n <= 0 (base case)
      - Recursively build range(n-1), then append [n] using ++

Note: There are more efficient ways (see README), but this is good practice

Type Signature: Int -> [Int]
-}

makeRange :: Int -> [Int]
makeRange n 
  | n <= 0 = []
  | otherwise = makeRange (n-1) ++ [n]


-- ============================================================================
-- Exercise 6: List Transformation - Map Pattern
-- ============================================================================
-- Concepts: Recursion, pattern matching, transformation
-- Difficulty: Medium
-- Scala Equivalent: list.map(_ * 2) (but implement manually)

{-
Problem:
Transform a list by doubling each element (manual map implementation).

Examples:
  doubleAll [1,2,3]    --> [2,4,6]
  doubleAll [10]       --> [20]
  doubleAll []         --> []

Hint: Similar to keepEvens in README, but simpler (no guards needed)
      - Base case: doubleAll [] = []
      - Recursive: cons (x*2) onto doubleAll xs

Type Signature: [Int] -> [Int]
-}

doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x : xs) = (2*x) : doubleAll xs


-- ============================================================================
-- BONUS Exercise 7: Filter Pattern - Guards + Recursion
-- ============================================================================
-- Concepts: Pattern matching, guards, recursion
-- Difficulty: Medium-Hard
-- Scala Equivalent: list.filter(_ > 0) (but implement manually)

{-
Problem:
Filter a list to keep only positive integers (manual filter implementation).

Examples:
  keepPositive [1,-2,3,-4,5]  --> [1,3,5]
  keepPositive [-1,-2,-3]     --> []
  keepPositive [1,2,3]        --> [1,2,3]

Hint: This is EXACTLY like the `keepEvens` example in README.md!
      - Pattern match on [] and (x:xs)
      - Use guards to check if x > 0
      - If true: cons x onto recursive result
      - If false: just return recursive result (skip x)

Type Signature: [Int] -> [Int]
-}

keepPositive :: [Int] -> [Int]
keepPositive [] = []
keepPositive (x : xs)  
  | x <= 0 = keepPositive(xs)
  | otherwise = x : keepPositive(xs)


-- ============================================================================
-- TESTING YOUR SOLUTIONS
-- ============================================================================

{-
To test your functions:

1. Open GHCi (Glasgow Haskell Compiler Interactive)
2. Load this file:
   :load exercises.hs

3. Test individual functions:
   add 3 5
   countElements [1,2,3,4]
   doubleAll [1,2,3]

4. If you make changes, reload:
   :reload

5. Check the type of your function:
   :type add
-}
