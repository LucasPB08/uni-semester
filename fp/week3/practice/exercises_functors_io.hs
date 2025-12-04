module Exercises_Functors_IO where

-- Week 3: Functors and IO
-- Focus: Functor typeclass, fmap, IO actions, do-notation
-- Test your functions in GHCi by loading this file with `:load exercises_functors_io.hs`

import Data.Char (toUpper, toLower, isDigit)

{-
  INSTRUCTIONS:
  1. Read the worked examples in README_FUNCTORS_IO.md first
  2. Implement each function below
  3. Test Functor exercises in GHCi normally
  4. For IO exercises, you need to RUN them (e.g., type `greet` in GHCi)
  5. Run tests with `:load tests_functors_io.hs` then `runAllTests`
-}


-- ============================================================================
-- PART 1: FUNCTORS
-- ============================================================================

-- ============================================================================
-- Data Types for Exercises
-- ============================================================================

-- A box holding a single value
data Box a = Box a
  deriving (Show, Eq)

-- A pair of values of the same type
data Pair a = Pair a a
  deriving (Show, Eq)

-- Either-like type: Success with value, or Failure with error message
data Result a = Failure String | Success a
  deriving (Show, Eq)

-- Binary tree (same as your previous exercises)
data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)


-- ============================================================================
-- Exercise 1: Functor Instance for Box
-- ============================================================================
-- Concepts: Writing Functor instances, applying function inside container
-- Difficulty: Easy
-- Scala Equivalent: case class Box[A](value: A) with map method

{-
Problem:
Write a Functor instance for Box.
Apply the function to the value inside the box.

Examples:
  fmap (*2) (Box 5)        --> Box 10
  fmap show (Box 42)       --> Box "42"
  fmap length (Box "hello") --> Box 5

Hint: Box contains exactly one value, apply f to it.
-}

instance Functor Box where
  fmap f (Box a)= Box (f a)


-- ============================================================================
-- Exercise 2: Functor Instance for Pair
-- ============================================================================
-- Concepts: Functor with multiple values
-- Difficulty: Easy
-- Scala Equivalent: case class Pair[A](fst: A, snd: A) with map method

{-
Problem:
Write a Functor instance for Pair.
Apply the function to BOTH values in the pair.

Examples:
  fmap (*2) (Pair 3 5)           --> Pair 6 10
  fmap toUpper (Pair 'a' 'b')    --> Pair 'A' 'B'
  fmap length (Pair "hi" "hello") --> Pair 2 5

-}

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b) 


-- ============================================================================
-- Exercise 3: Functor Instance for Result
-- ============================================================================
-- Concepts: Functor that may not have a value (like Either)
-- Difficulty: Medium
-- Scala Equivalent: sealed trait Result[A] { def map[B](f: A => B): Result[B] }

{-
Problem:
Write a Functor instance for Result.
- For Success: apply the function to the value
- For Failure: keep the error message, don't apply function

Examples:
  fmap (*2) (Success 5)           --> Success 10
  fmap (*2) (Failure "no value")  --> Failure "no value"
  fmap show (Success 42)          --> Success "42"
  fmap length (Failure "error")   --> Failure "error"

Hint: Only transform Success values, Failure passes through unchanged.
      This is like Either where Left holds errors.
-}

instance Functor Result where
  fmap f (Success a) = Success (f a)
  fmap _ (Failure a) = Failure a 


-- ============================================================================
-- Exercise 4: Functor Instance for Tree
-- ============================================================================
-- Concepts: Functor for recursive data types
-- Difficulty: Medium
-- Scala Equivalent: Your treeMap function as a Functor instance

{-
Problem:
Write a Functor instance for Tree.
This is exactly your treeMap function!

Examples:
  fmap (*2) Empty                                    --> Empty
  fmap (*2) (Node 5 Empty Empty)                     --> Node 10 Empty Empty
  fmap (*2) (Node 5 (Node 3 Empty Empty) Empty)      --> Node 10 (Node 6 Empty Empty) Empty
  fmap length (Node "hi" Empty Empty)                --> Node 2 Empty Empty

Hint: Empty stays Empty, Node applies f to value and recurses on subtrees.
-}

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node el left right) = Node (f el) (fmap f left) (fmap f right)


-- ============================================================================
-- Exercise 5: Using fmap and <$>
-- ============================================================================
-- Concepts: Practical use of fmap and <$> operator
-- Difficulty: Easy
-- Scala Equivalent: Using .map on various containers

{-
Problem:
Implement these functions using fmap or <$> (they're the same).
Do NOT use pattern matching - just fmap!

Examples:
  incrementAll [1,2,3]           --> [2,3,4]
  incrementAll []                --> []

  doubleIfPresent (Just 5)       --> Just 10
  doubleIfPresent Nothing        --> Nothing

  uppercaseResult (Success "hi") --> Success "HI"
  uppercaseResult (Failure "x")  --> Failure "x"
-}

-- Increment all numbers in a list using fmap
incrementAll :: [Int] -> [Int]
incrementAll = fmap (+1)

-- Double the value if present using fmap
doubleIfPresent :: Maybe Int -> Maybe Int
doubleIfPresent = fmap (*2)

-- Uppercase the string in a Result using fmap
uppercaseResult :: Result String -> Result String
uppercaseResult = fmap (map toUpper)


-- ============================================================================
-- Exercise 6: Functor Laws Verification
-- ============================================================================
-- Concepts: Understanding Functor laws
-- Difficulty: Medium

{-
Problem:
These functions check if the Functor laws hold for specific values.
Implement them to return True if the law holds.

Law 1 (Identity): fmap id x == x
Law 2 (Composition): fmap (g . f) x == fmap g (fmap f x)
-}

-- Check identity law for a Box
checkIdentityBox :: Eq a => Box a -> Bool
checkIdentityBox box = fmap id box == box  -- Should be: fmap id box == box

-- Check composition law for a Maybe
-- f = (+1), g = (*2)
checkCompositionMaybe :: Maybe Int -> Bool
checkCompositionMaybe mx = fmap ((*2) . (+1)) mx == fmap (*2) (fmap (+1) mx) 
  -- Should be: fmap ((*2) . (+1)) mx == fmap (*2) (fmap (+1) mx)


-- ============================================================================
-- PART 2: IO
-- ============================================================================

-- ============================================================================
-- Exercise 7: Basic IO - Greeting
-- ============================================================================
-- Concepts: putStrLn, getLine, do-notation, string concatenation
-- Difficulty: Easy
-- Scala Equivalent: println + readLine

{-
Problem:
Write an IO action that:
1. Prints "What is your name?"
2. Reads a line of input
3. Prints "Hello, <name>!"

Example interaction:
  > greet
  What is your name?
  Alice              <- user types this
  Hello, Alice!

-}

greet :: IO ()
greet = do 
  putStrLn "What is your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")


-- ============================================================================
-- Exercise 8: IO with Pure Functions
-- ============================================================================
-- Concepts: Combining IO and pure code, let in do-blocks
-- Difficulty: Easy-Medium
-- Scala Equivalent: Reading input, transforming, printing

{-
Problem:
Write an IO action that:
1. Prints "Enter some text:"
2. Reads a line
3. Converts it to uppercase (pure function!)
4. Prints the uppercase version

Example interaction:
  > shoutBack
  Enter some text:
  hello world        <- user types this
  HELLO WORLD

-}
helper = map toUpper

shoutBack :: IO ()
shoutBack = do
  putStrLn "Enter some text:"
  input <- getLine
  let transformed = helper input
  putStrLn transformed


-- ============================================================================
-- Exercise 9: IO Returning a Value
-- ============================================================================
-- Concepts: IO actions that return values, return
-- Difficulty: Medium
-- Scala Equivalent: Function that reads and returns processed input

{-
Problem:
Write an IO action that:
1. Prints "Enter a number:"
2. Reads a line
3. Parses it as an Int (use: read line :: Int)
4. Returns the number doubled

Example:
  > result <- askAndDouble
  Enter a number:
  21                 <- user types this
  > result
  42

-}

askAndDouble :: IO Int
askAndDouble = do
  putStrLn "Enter a number"
  number <- getLine
  let x = read number :: Int
  return (x*2)




-- ============================================================================
-- Exercise 10: Multiple IO Actions
-- ============================================================================
-- Concepts: Sequencing multiple IO actions, accumulating results
-- Difficulty: Medium
-- Scala Equivalent: Multiple readLine calls combined

{-
Problem:
Write an IO action that:
1. Prints "Enter first name:"
2. Reads first name
3. Prints "Enter last name:"
4. Reads last name
5. Returns the full name as "First Last"

Example:
  > name <- askFullName
  Enter first name:
  John               <- user types this
  Enter last name:
  Doe                <- user types this
  > name
  "John Doe"

-}

askFullName :: IO String
askFullName = do
  putStrLn "Enter first name:"
  firstName <- getLine
  putStrLn "Enter last name:"
  lastName <- getLine
  return (firstName ++ lastName)


-- ============================================================================
-- Exercise 11: Conditional IO
-- ============================================================================
-- Concepts: if-then-else in IO, branching based on input
-- Difficulty: Medium
-- Scala Equivalent: if/else with side effects

{-
Problem:
Write an IO action that:
1. Prints "Do you want to continue? (yes/no)"
2. Reads the answer
3. If "yes": prints "Continuing..." and returns True
4. Otherwise: prints "Stopping." and returns False

Example:
  > continue <- askContinue
  Do you want to continue? (yes/no)
  yes                <- user types this
  Continuing...
  > continue
  True

Hint: Use if-then-else inside do-notation
      Both branches must have the same type (IO Bool)
-}

askContinue :: IO Bool
askContinue = do
  putStrLn "Do you want to continue? (yes/no)"
  continue <- getLine
  if continue == "yes" then
    do 
    putStrLn "Continuing..."
    return True
  else
    do
    putStrLn "Stopping."
    return False



-- ============================================================================
-- Exercise 12: IO with List Processing
-- ============================================================================
-- Concepts: Reading multiple values, pure list processing
-- Difficulty: Medium-Hard
-- Scala Equivalent: Reading comma-separated input, processing as list

{-
Problem:
Write an IO action that:
1. Prints "Enter numbers separated by spaces:"
2. Reads a line
3. Parses it into a list of integers
4. Returns the sum

Example:
  > total <- sumNumbers
  Enter numbers separated by spaces:
  10 20 30           <- user types this
  > total
  60


-}
helper3 = sum . map (\el -> read el :: Int) . words 

sumNumbers :: IO Int
sumNumbers = do
  putStrLn "Enter numbers separated by spaces:"
  numbers <- getLine
  let total = helper3 numbers
  return total


-- ============================================================================
-- BONUS Exercise 13: Recursive IO
-- ============================================================================
-- Concepts: Recursive IO actions, building up results
-- Difficulty: Hard
-- Scala Equivalent: Loop with accumulator using recursion

{-
Problem:
Write an IO action that repeatedly asks for input until the user types "done".
Return all the inputs as a list.

Example:
  > items <- collectUntilDone
  Enter item (or 'done' to finish):
  apple              <- user types this
  Enter item (or 'done' to finish):
  banana             <- user types this
  Enter item (or 'done' to finish):
  done               <- user types this
  > items
  ["apple","banana"]

Hint:
  - Use recursion: if input is "done", return []
  - Otherwise, recursively collect more and cons current input
  - Pattern: do { line <- getLine; if line == "done" then ... else ... }
-}

collectUntilDone :: IO [String]
collectUntilDone = undefined


-- ============================================================================
-- BONUS Exercise 14: Combining Functor and IO
-- ============================================================================
-- Concepts: fmap on IO, transforming IO results
-- Difficulty: Medium

{-
Problem:
IO is also a Functor! You can fmap over IO actions.

fmap f ioAction = do
  result <- ioAction
  return (f result)

Implement these using fmap on IO (NOT do-notation):
-}

-- Read a line and return its length (use fmap, not do-notation)
-- Hint: fmap length getLine
getLineLength :: IO Int
getLineLength = undefined

-- Read a line and return it uppercased (use fmap, not do-notation)
-- Hint: fmap (map toUpper) getLine
getUpperLine :: IO String
getUpperLine = undefined


-- ============================================================================
-- TESTING YOUR SOLUTIONS
-- ============================================================================

{-
To test FUNCTOR exercises (pure functions):
  ghci
  :load exercises_functors_io.hs
  fmap (*2) (Box 5)
  fmap show (Success 42)

To test IO exercises (need to RUN them):
  ghci
  :load exercises_functors_io.hs
  greet              -- type input when prompted
  shoutBack          -- type input when prompted
  result <- askAndDouble  -- then check: result

To run all tests:
  :load tests_functors_io.hs
  runAllTests
-}
