# Week 2: Advanced Patterns and First-Class Functions

## Goals
- Master as-patterns (@) for naming patterns
- Understand functions as first-class values
- Practice function composition and point-free style
- Learn partial application in depth
- Build and return functions

## Context: Building on Week 1

You've mastered basic recursion, pattern matching, and guards. Now we'll focus on:
- **As-patterns (@)** - Naming both the whole and the parts
- **Functions as values** - Passing, returning, and composing functions
- **Partial application** - Currying in practice
- **Point-free style** - Defining functions without naming arguments

---

## GHCi Setup Reminder

```bash
ghci
:load exercises.hs    # or :l exercises.hs
:reload               # or :r (after making changes)
:type expr            # or :t (check type of expression)
```

---

## New Syntax: As-Patterns (@)

### What Are As-Patterns?

As-patterns let you **name both the whole value AND its parts** in a single pattern.

**Haskell:**
```haskell
-- Without as-pattern (inefficient - builds list twice)
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = (x:xs) ++ (x:xs)  -- (x:xs) written twice!

-- With as-pattern (efficient - name the whole list once)
duplicate :: [a] -> [a]
duplicate [] = []
duplicate all@(x:xs) = all ++ all    -- 'all' refers to entire (x:xs)

-- Test in GHCi:
-- duplicate [1,2,3]  --> [1,2,3,1,2,3]
```

**Scala equivalent:**
```scala
def duplicate[A](list: List[A]): List[A] = list match {
  case Nil => Nil
  case all @ (_ :: _) => all ++ all  // Scala also has @ for as-patterns!
}
```

**Syntax:**
```haskell
name@pattern
```
- `name` becomes bound to the entire value
- `pattern` is destructured as usual

---

## First-Class Functions

### Functions as Values

In Haskell, **functions are values** like integers or strings. You can:
- Pass them as arguments
- Return them from functions
- Store them in data structures

**Haskell:**
```haskell
-- Function as argument
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Test:
-- applyTwice (+1) 5        --> 7  (applies +1 twice: (5+1)+1)
-- applyTwice (*2) 3        --> 12 (applies *2 twice: (3*2)*2)
-- applyTwice reverse [1,2] --> [1,2]
```

**Scala equivalent:**
```scala
def applyTwice[A](f: A => A, x: A): A = f(f(x))

// Or with function type:
def applyTwice[A](f: A => A)(x: A): A = f(f(x))
```

---

### Partial Application (Currying in Practice)

**Every Haskell function takes one argument and returns a function.**

**Haskell:**
```haskell
add :: Int -> Int -> Int
add x y = x + y

-- These are ALL equivalent:
add 3 5          -- 8
(add 3) 5        -- 8, explicitly showing partial application
let add3 = add 3 -- add3 is a function that adds 3
    in add3 5    -- 8

-- Practical use:
addTen = add 10
doubleAll = map (*2)      -- Partially apply * to get "times 2" function
keepBig = filter (> 100)  -- Partially apply > to get "greater than 100"
```

**Scala equivalent:**
```scala
def add(x: Int)(y: Int): Int = x + y  // Curried form

val addTen = add(10) _
val doubleAll = (list: List[Int]) => list.map(_ * 2)
```

**Key insight:** In Haskell, `add 3` is NOT a function call with one argument missing. It's a **new function** that adds 3 to its argument.

---

### Function Composition (.)

Combine functions to create new functions.

**Haskell:**
```haskell
-- The (.) operator composes functions
-- (f . g) x  =  f (g x)

-- Composition operator type:
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)

-- Example:
double :: Int -> Int
double x = x * 2

plusOne :: Int -> Int
plusOne x = x + 1

doubleThenPlusOne :: Int -> Int
doubleThenPlusOne = plusOne . double  -- Read right-to-left: double, THEN plusOne

-- Test:
-- doubleThenPlusOne 5  --> 11  (5*2 = 10, then 10+1 = 11)

-- More practical example:
import Data.Char (toUpper)

shout :: String -> String
shout = map toUpper  -- Converts to uppercase

exclaim :: String -> String
exclaim s = s ++ "!"

shoutExclaim :: String -> String
shoutExclaim = exclaim . shout  -- First shout (uppercase), then exclaim (add !)

-- Test:
-- shoutExclaim "hello"  --> "HELLO!"
```

**Scala equivalent:**
```scala
val double: Int => Int = x => x * 2
val plusOne: Int => Int = x => x + 1

val doubleThenPlusOne = double andThen plusOne
// Or: val doubleThenPlusOne = plusOne compose double
```

**Reading composition:**
- `f . g` means "do g first, then f"
- Read **right to left**
- `(h . g . f) x` = `h(g(f(x)))`

---

### Point-Free Style

Defining functions **without naming their arguments**.

**Haskell:**
```haskell
-- Pointful style (naming argument 'x'):
sum' :: [Int] -> Int
sum' xs = foldr (+) 0 xs

-- Point-free style (no 'xs'):
sum' :: [Int] -> Int
sum' = foldr (+) 0

-- Another example:
-- Pointful:
doubleAll :: [Int] -> [Int]
doubleAll xs = map (*2) xs

-- Point-free:
doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

-- Why? Because partial application!
-- map (*2) is already a function [Int] -> [Int]
-- No need to write xs twice
```

**Scala equivalent:**
```scala
// Scala doesn't make this as natural, but you can do:
val doubleAll: List[Int] => List[Int] = _ map (_ * 2)
```

**When to use point-free:**
- ✅ When it makes code clearer
- ✅ When using composition
- ❌ When it makes code harder to read
- ❌ When you need to name arguments for clarity

---

### Functions Returning Functions

**Haskell:**
```haskell
-- A function that builds a "greater than" checker
greaterThan :: Int -> (Int -> Bool)
greaterThan threshold = \x -> x > threshold
-- Or equivalently: greaterThan threshold x = x > threshold

-- Usage:
biggerThan10 = greaterThan 10
biggerThan100 = greaterThan 100

-- Test:
-- biggerThan10 15   --> True
-- biggerThan100 50  --> False

-- More complex: function that builds an adder
makeAdder :: Int -> (Int -> Int)
makeAdder n = \x -> x + n

add5 = makeAdder 5
add100 = makeAdder 100

-- Test:
-- add5 3    --> 8
-- add100 20 --> 120
```

**Scala equivalent:**
```scala
def greaterThan(threshold: Int): Int => Boolean =
  x => x > threshold

val biggerThan10 = greaterThan(10)
```

---

## Worked Examples

### Example 1: As-Pattern for Efficiency

**Problem:** Insert an element at the beginning if the list starts with a specific value.

**Haskell implementation:**
```haskell
-- Without as-pattern (inefficient):
prependIfStarts :: Eq a => a -> a -> [a] -> [a]
prependIfStarts val toCheck (x:xs)
  | x == toCheck = val : (x:xs)  -- Rebuilding (x:xs)!
  | otherwise    = x:xs
prependIfStarts _ _ [] = []

-- With as-pattern (efficient):
prependIfStarts :: Eq a => a -> a -> [a] -> [a]
prependIfStarts val toCheck all@(x:xs)
  | x == toCheck = val : all     -- Use 'all' instead of rebuilding
  | otherwise    = all
prependIfStarts _ _ [] = []

-- Test:
-- prependIfStarts 0 1 [1,2,3]  --> [0,1,2,3]
-- prependIfStarts 0 5 [1,2,3]  --> [1,2,3]
```

---

### Example 2: Function Composition Chain

**Problem:** Process a string by trimming, converting to uppercase, and adding exclamation.

**Haskell implementation:**
```haskell
import Data.Char (toUpper)

-- Individual transformations:
trimSpaces :: String -> String
trimSpaces = filter (/= ' ')

uppercase :: String -> String
uppercase = map toUpper

addExclaim :: String -> String
addExclaim s = s ++ "!!!"

-- Composed version (right-to-left execution):
processString :: String -> String
processString = addExclaim . uppercase . trimSpaces

-- Test:
-- processString "hello world"  --> "HELLOWORLD!!!"
```

**Step-by-step execution:**
```
processString "hello world"
= (addExclaim . uppercase . trimSpaces) "hello world"
= addExclaim (uppercase (trimSpaces "hello world"))
= addExclaim (uppercase "helloworld")
= addExclaim "HELLOWORLD"
= "HELLOWORLD!!!"
```

---

### Example 3: Building Custom Filters

**Problem:** Create a function that builds filter predicates.

**Haskell implementation:**
```haskell
-- Function that creates range checkers
inRange :: Int -> Int -> (Int -> Bool)
inRange low high = \x -> x >= low && x <= high

-- Create specific range checkers:
isDigit = inRange 0 9
isTeen = inRange 13 19
isAdult = inRange 18 100

-- Use with filter:
-- filter isDigit [5, 15, 8, 20, 3]    --> [5, 8, 3]
-- filter isTeen [10, 15, 18, 25, 13]  --> [15, 18, 13]
```

---

### Example 4: Partial Application in Practice

**Problem:** Use partial application to create specialized functions.

**Haskell implementation:**
```haskell
-- Generic replicate function (built-in, but let's see the type)
-- replicate :: Int -> a -> [a]

-- Specialized versions via partial application:
threeTimes = replicate 3
tenTimes = replicate 10

-- Test:
-- threeTimes 'a'  --> "aaa"
-- tenTimes 5      --> [5,5,5,5,5,5,5,5,5,5]

-- Another example with map:
doubleList = map (*2)
squareList = map (\x -> x * x)
negatelist = map negate

-- Test:
-- doubleList [1,2,3]  --> [2,4,6]
-- squareList [1,2,3]  --> [1,4,9]
```

---

## Key Takeaways for This Week

### As-Patterns (@)
- Syntax: `name@pattern`
- Use when you need both the whole value and its parts
- Avoids rebuilding data structures
- Common pattern: `all@(x:xs)` for lists

### First-Class Functions
- Functions are values (can be passed, returned, stored)
- Use parentheses for function types: `(Int -> Bool)`
- Lambda syntax: `\x -> expression`

### Partial Application
- All functions are curried automatically
- `add 3` creates a new function
- Useful for creating specialized functions

### Function Composition (.)
- Syntax: `f . g` means "g then f" (right-to-left)
- Type: `(b -> c) -> (a -> b) -> (a -> c)`
- Creates pipelines of transformations

### Point-Free Style
- Omit the last argument when it's redundant
- Clearer when using composition
- Don't overuse - clarity matters more

---

## Common Patterns

### 1. As-Pattern for Whole + Parts
```haskell
process all@(x:xs) = ... all ... x ... xs ...
```

### 2. Function Builder
```haskell
buildChecker :: threshold -> (input -> Bool)
buildChecker t = \x -> x > t
```

### 3. Composition Chain
```haskell
pipeline = f3 . f2 . f1  -- Execute: f1, then f2, then f3
```

### 4. Partial Application for Specialization
```haskell
specificFunc = genericFunc arg1  -- Creates new function
```

---

## Exercise Overview

Now that you've seen worked examples, try these exercises:

1. **tails** - Use as-patterns to build list of all tails
2. **applyTwice** - Apply function twice (first-class functions)
3. **compose** - Implement function composition manually
4. **flipArgs** - Flip function argument order
5. **applyNTimes** - Apply function n times (recursion + higher-order)
6. **buildMultiplier** - Return functions that multiply by n
7. **pipelineFunctions** - Compose multiple functions from a list

**Strategy:**
- For as-patterns: think "do I need both the whole AND the parts?"
- For functions: remember they're just values
- For composition: read right-to-left
- Test incrementally in GHCi!

---

## Common Pitfalls

❌ **As-pattern syntax**: `@` comes AFTER the name: `all@(x:xs)`, not `(x:xs)@all`
❌ **Composition order**: `f . g` applies `g` first (right-to-left)
❌ **Over-using point-free**: Don't sacrifice clarity
❌ **Forgetting parentheses** in function types: `Int -> Bool -> Bool` vs `(Int -> Bool) -> Bool`

✅ **Do use as-patterns** when you need the whole value
✅ **Do practice partial application** - it's very powerful
✅ **Do compose functions** for transformation pipelines
✅ **Do name intermediate functions** when it aids understanding

---

## Next Steps

1. Work through `exercises.hs`
2. Test each function in GHCi
3. Experiment with point-free versions
4. Show me your solutions for review!
