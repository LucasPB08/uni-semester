# Week 2: Advanced Exercises

## Overview
These exercises build on the fundamentals and introduce:
- **Folding functions** (`foldr`, `foldl`) - the most important higher-order pattern
- **Function application operator** (`$`) - reduce parentheses
- **Complex composition chains** - building transformation pipelines
- **Combining multiple concepts** - as-patterns + higher-order functions
- **Real-world patterns** - practical functional programming techniques

**Difficulty:** Medium-Hard to Very Hard - prepare to be challenged!

---

## New Concepts

### 1. The Fold Functions: `foldr` and `foldl`

Folds are **the most powerful pattern in functional programming**. Almost every list operation can be expressed as a fold!

#### What is Folding?

**Folding** is reducing a list to a single value by combining elements with a function.

Think of it like this:
- You have a list: `[1, 2, 3, 4]`
- You have a combining function: `(+)`
- You have a starting value: `0`
- Fold combines them: `0 + 1 + 2 + 3 + 4 = 10`

---

### `foldr` - Fold Right

**Type signature:**
```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
--       ^^^^^^^^^^^^^^    ^    ^^^    ^
--       combining fn    init  list  result
```

**How it works:**
```haskell
foldr f z [x1, x2, x3]
= f x1 (f x2 (f x3 z))
--      └─────┘  └─┘
--      recurse  base case
```

**Visualizing `foldr (+) 0 [1,2,3,4]`:**
```
foldr (+) 0 [1,2,3,4]
= 1 + (foldr (+) 0 [2,3,4])
= 1 + (2 + (foldr (+) 0 [3,4]))
= 1 + (2 + (3 + (foldr (+) 0 [4])))
= 1 + (2 + (3 + (4 + (foldr (+) 0 []))))
= 1 + (2 + (3 + (4 + 0)))
= 1 + (2 + (3 + 4))
= 1 + (2 + 7)
= 1 + 9
= 10
```

**Examples:**

```haskell
-- Sum a list
sum' :: [Int] -> Int
sum' = foldr (+) 0

-- Test:
-- sum' [1,2,3,4]  --> 10

-- Product of a list
product' :: [Int] -> Int
product' = foldr (*) 1

-- Test:
-- product' [2,3,4]  --> 24

-- Length of a list
length' :: [a] -> Int
length' = foldr (\x acc -> 1 + acc) 0
-- Or: length' = foldr (\_ acc -> acc + 1) 0

-- Test:
-- length' [1,2,3]  --> 3

-- Map using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- Test:
-- map' (*2) [1,2,3]  --> [2,4,6]

-- Filter using foldr
filter' :: (a -> Bool) -> [a] -> [a]
filter' pred = foldr (\x acc -> if pred x then x : acc else acc) []

-- Test:
-- filter' even [1,2,3,4]  --> [2,4]
```

---

### `foldl` - Fold Left

**Type signature:**
```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
--       ^^^^^^^^^^^^^^    ^    ^^^    ^
--       combining fn    init  list  result
```

**How it works:**
```haskell
foldl f z [x1, x2, x3]
= f (f (f z x1) x2) x3
--      └──┘
--      start from left
```

**Visualizing `foldl (+) 0 [1,2,3,4]`:**
```
foldl (+) 0 [1,2,3,4]
= foldl (+) (0 + 1) [2,3,4]
= foldl (+) ((0 + 1) + 2) [3,4]
= foldl (+) (((0 + 1) + 2) + 3) [4]
= foldl (+) ((((0 + 1) + 2) + 3) + 4) []
= ((((0 + 1) + 2) + 3) + 4)
= (((1 + 2) + 3) + 4)
= ((3 + 3) + 4)
= (6 + 4)
= 10
```

---

### When to Use `foldr` vs `foldl`?

**Use `foldr` when:**
- Building a new data structure (list, tree, etc.)
- Working with infinite lists (foldr is lazy!)
- The operation is right-associative
- Default choice for most cases

**Use `foldl` when:**
- Computing a single value (sum, product, count)
- The operation is left-associative
- You want strict evaluation (use `foldl'` from `Data.List`)

**Scala comparison:**
```scala
// Scala's foldLeft is like Haskell's foldl
list.foldLeft(0)(_ + _)

// Scala's foldRight is like Haskell's foldr
list.foldRight(0)(_ + _)
```

---

### 2. Function Application Operator: `$`

**The `$` operator** applies a function to an argument, but with **very low precedence**.

**Type signature:**
```haskell
($) :: (a -> b) -> a -> b
f $ x = f x
```

**Why use it?** To avoid parentheses!

```haskell
-- Without $: lots of parentheses
head (reverse (filter even (map (*2) [1,2,3,4])))

-- With $: cleaner
head $ reverse $ filter even $ map (*2) [1,2,3,4]

-- Another example:
sum (map (*2) (filter even [1,2,3,4,5]))

-- With $:
sum $ map (*2) $ filter even [1,2,3,4,5]
```

**How to read `$`:** "Apply the function on the left to everything on the right"

```haskell
f $ g $ h x
= f (g (h x))
```

**Scala equivalent:**
Scala doesn't have this - you'd just use parentheses or intermediate variables.

---

### 3. Complex Composition Chains

You can chain multiple functions using `.` and `$` for readable transformation pipelines:

```haskell
-- Process numbers: filter evens, double them, sum them
processNumbers :: [Int] -> Int
processNumbers = sum . map (*2) . filter even

-- Test:
-- processNumbers [1,2,3,4,5,6]  --> 24
-- (evens: [2,4,6], doubled: [4,8,12], sum: 24)

-- Process strings: trim spaces, uppercase, add exclamation
import Data.Char (toUpper)

processString :: String -> String
processString = (++ "!!!") . map toUpper . filter (/= ' ')

-- Test:
-- processString "hello world"  --> "HELLOWORLD!!!"
```

**Pattern:**
```haskell
pipeline = step3 . step2 . step1
-- Reads: do step1, then step2, then step3
```

---

### 4. Worked Examples

#### Example 1: Implementing `concat` with `foldr`

**Problem:** Flatten a list of lists into a single list.

```haskell
concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- How it works:
-- concat' [[1,2], [3,4], [5]]
-- = [1,2] ++ ([3,4] ++ ([5] ++ []))
-- = [1,2] ++ ([3,4] ++ [5])
-- = [1,2] ++ [3,4,5]
-- = [1,2,3,4,5]

-- Test:
-- concat' [[1,2], [3,4], [5,6]]  --> [1,2,3,4,5,6]
```

---

#### Example 2: Reverse with `foldl`

**Problem:** Reverse a list using a fold.

```haskell
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []
-- flip (:) means: \acc x -> x : acc

-- How it works:
-- reverse' [1,2,3]
-- = foldl (flip (:)) [] [1,2,3]
-- = foldl (flip (:)) (1 : []) [2,3]     -- = [1]
-- = foldl (flip (:)) (2 : [1]) [3]      -- = [2,1]
-- = foldl (flip (:)) (3 : [2,1]) []     -- = [3,2,1]
-- = [3,2,1]

-- Test:
-- reverse' [1,2,3,4]  --> [4,3,2,1]
```

**Note:** `flip` is a built-in function that flips argument order:
```haskell
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x
```

---

#### Example 3: Pipeline with `$` and `.`

**Problem:** Count how many even numbers are in a list after doubling them.

```haskell
-- Without pipeline (lots of parens):
countEvenAfterDouble :: [Int] -> Int
countEvenAfterDouble xs = length (filter even (map (*2) xs))

-- With $ (less parens):
countEvenAfterDouble xs = length $ filter even $ map (*2) xs

-- With . (point-free):
countEvenAfterDouble :: [Int] -> Int
countEvenAfterDouble = length . filter even . map (*2)

-- Test:
-- countEvenAfterDouble [1,2,3,4,5]  --> 5
-- (doubled: [2,4,6,8,10], all even, count: 5)
```

---

#### Example 4: Using Folds for Complex Operations

**Problem:** Find the maximum element in a list (without using `maximum`).

```haskell
maximum' :: [Int] -> Int
maximum' [] = error "empty list"
maximum' (x:xs) = foldl max x xs
-- Start with first element, compare with rest using max

-- Test:
-- maximum' [3,7,2,9,1]  --> 9

-- Alternative with foldr1 (built-in, no need for initial value):
maximum'' :: [Int] -> Int
maximum'' = foldr1 max

-- foldr1 uses the last element as the initial value
```

---

## Key Patterns

### Pattern 1: Building Lists with `foldr`
```haskell
-- Template:
buildList = foldr (\x acc -> ... x ... : acc) []

-- Examples:
map' f = foldr (\x acc -> f x : acc) []
filter' p = foldr (\x acc -> if p x then x : acc else acc) []
```

### Pattern 2: Accumulating Values with `foldl`
```haskell
-- Template:
accumulate = foldl (\acc x -> ... acc ... x ...) initialValue

-- Examples:
sum' = foldl (+) 0
product' = foldl (*) 1
reverse' = foldl (flip (:)) []
```

### Pattern 3: Function Pipelines
```haskell
-- Template:
pipeline = finalStep . middleStep . firstStep

-- Or with $:
result = finalStep $ middleStep $ firstStep $ input
```

### Pattern 4: Combining `foldr` and Composition
```haskell
-- Template:
complexOperation = foldr operation initialValue . transformation

-- Example:
sumOfSquares :: [Int] -> Int
sumOfSquares = foldr (+) 0 . map (^2)
-- First square all numbers, then sum them
```

---

## Common Haskell Idioms

### 1. Point-Free Function Composition
```haskell
-- Instead of:
f x = g (h (i x))

-- Write:
f = g . h . i
```

### 2. Using `$` to Avoid Parentheses
```haskell
-- Instead of:
result = f (g (h x))

-- Write:
result = f $ g $ h x
```

### 3. Combining `.` and `$`
```haskell
-- For pipelines with final argument:
result = f . g . h $ x
-- Equivalent to: result = f (g (h x))
```

---

## Exercise Strategy

These advanced exercises will challenge you to:
1. **Implement common functions using folds** - understand the fold pattern
2. **Build complex pipelines** - chain transformations
3. **Combine concepts** - use as-patterns, folds, and composition together
4. **Think functionally** - solve problems without loops or mutation

**Tips:**
- For fold exercises, think: "What's my combining function? What's my initial value?"
- For pipelines, work backwards: "What's my final result? What transformations get me there?"
- Test incrementally - don't write everything at once!
- Use `:type` in GHCi to understand function types

---

## Testing

Use `tests_advanced.hs` to verify your solutions:

```bash
ghci
:load tests_advanced.hs
runAllTests
```

Good luck! These exercises will solidify your understanding of functional programming patterns.
