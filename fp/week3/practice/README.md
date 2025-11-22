# Week 3: Advanced Recursion and Type System Fundamentals

## Goals
- Master complex recursion patterns (multiple recursive calls)
- Work with tree structures and nested data
- Understand polymorphic types and type inference
- Learn basic type classes (Eq, Ord, Show)
- Practice with custom algebraic data types

## Context: Completing Foundations

You've mastered basic recursion, pattern matching, and first-class functions. Week 3 completes the **Foundations** phase by covering:
- **Multiple recursive calls** - Functions that recurse more than once
- **Tree structures** - Recursive data types beyond lists
- **Polymorphic types** - Generic functions that work with any type
- **Type classes** - Interfaces for operations like equality and comparison

---

## GHCi Setup Reminder

```bash
ghci
:load exercises.hs    # or :l exercises.hs
:reload               # or :r (after making changes)
:type expr            # or :t (check type of expression)
:info TypeClass       # or :i (get info about type classes)
```

---

## Multiple Recursive Calls

### What Are Multiple Recursive Calls?

Some recursive functions need to call themselves **more than once** in the recursive case. This is common when working with tree-like structures or when a problem naturally splits into multiple subproblems.

### Example: Fibonacci Numbers

**Haskell:**
```haskell
-- Fibonacci: each number is the sum of the previous two
fib :: Int -> Int
fib 0 = 0                    -- base case 1
fib 1 = 1                    -- base case 2
fib n = fib (n-1) + fib (n-2)  -- TWO recursive calls!

-- Test in GHCi:
-- fib 5  --> 5  (0,1,1,2,3,5)
-- fib 7  --> 13
```

**Scala equivalent:**
```scala
def fib(n: Int): Int = n match {
  case 0 => 0
  case 1 => 1
  case _ => fib(n-1) + fib(n-2)
}
```

**Key insight:** Each recursive case calls `fib` twice, creating a tree of recursive calls.

---

## Custom Algebraic Data Types

### Defining Custom Types

Algebraic data types let you define your own types with multiple constructors.

**Haskell:**
```haskell
-- A simple type with multiple constructors (sum type)
data TrafficLight = Red | Yellow | Green
  deriving (Show, Eq)  -- Auto-generate Show and Eq instances

-- Pattern matching on custom types:
lightDuration :: TrafficLight -> Int
lightDuration Red    = 60
lightDuration Yellow = 5
lightDuration Green  = 45

-- Test:
-- lightDuration Red     --> 60
-- lightDuration Yellow  --> 5
```

**Scala equivalent:**
```scala
sealed trait TrafficLight
case object Red extends TrafficLight
case object Yellow extends TrafficLight
case object Green extends TrafficLight

def lightDuration(light: TrafficLight): Int = light match {
  case Red    => 60
  case Yellow => 5
  case Green  => 45
}
```

---

### Product Types (Records)

Types that combine multiple values together.

**Haskell:**
```haskell
-- A product type with fields
data Point = Point Double Double
  deriving (Show, Eq)

-- Pattern matching to extract fields:
distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) =
  sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- Creating values:
origin = Point 0.0 0.0
point1 = Point 3.0 4.0

-- Test:
-- distance origin point1  --> 5.0
```

**Scala equivalent:**
```scala
case class Point(x: Double, y: Double)

def distance(p1: Point, p2: Point): Double = {
  val Point(x1, y1) = p1
  val Point(x2, y2) = p2
  math.sqrt(math.pow(x2 - x1, 2) + math.pow(y2 - y1, 2))
}
```

---

### Recursive Data Types: Binary Trees

The most important pattern: types that contain themselves.

**Haskell:**
```haskell
-- A binary tree that holds values of type 'a'
data Tree a = Empty
            | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

-- Examples:
-- Empty
-- Node 5 Empty Empty               -- leaf with value 5
-- Node 3 (Node 1 Empty Empty)
--        (Node 5 Empty Empty)      -- tree with 3 nodes

-- Count nodes in tree
treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ left right) = 1 + treeSize left + treeSize right
-- TWO recursive calls: one for left subtree, one for right

-- Test:
-- treeSize Empty  --> 0
-- treeSize (Node 5 Empty Empty)  --> 1
-- treeSize (Node 3 (Node 1 Empty Empty) (Node 5 Empty Empty))  --> 3
```

**Scala equivalent:**
```scala
sealed trait Tree[+A]
case object Empty extends Tree[Nothing]
case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

def treeSize[A](tree: Tree[A]): Int = tree match {
  case Empty => 0
  case Node(_, left, right) => 1 + treeSize(left) + treeSize(right)
}
```

**Key insight:** Recursive data types require recursive functions!

---

## Polymorphic Types and Type Variables

### Type Variables (Generics)

**Haskell:**
```haskell
-- Type variable 'a' means "works with ANY type"
first :: [a] -> Maybe a
first []    = Nothing
first (x:xs) = Just x

-- Works with ANY list type:
-- first [1,2,3]         --> Just 1    (Int)
-- first "hello"         --> Just 'h'  (Char)
-- first [True, False]   --> Just True (Bool)
-- first []              --> Nothing
```

**Scala equivalent:**
```scala
def first[A](list: List[A]): Option[A] = list match {
  case Nil => None
  case x :: _ => Some(x)
}
```

**Type variables** (lowercase like `a`, `b`) work like generic type parameters in Scala.

---

### Maybe Type (Option in Scala)

The `Maybe` type represents optional values - either `Just value` or `Nothing`.

**Haskell:**
```haskell
-- Maybe is defined as:
-- data Maybe a = Nothing | Just a

-- Safe division (avoiding division by zero)
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- Test:
-- safeDivide 10 2  --> Just 5.0
-- safeDivide 10 0  --> Nothing
```

**Scala equivalent:**
```scala
def safeDivide(x: Double, y: Double): Option[Double] =
  if (y == 0) None else Some(x / y)
```

---

## Type Classes (Interfaces)

### Understanding Type Classes

Type classes are like interfaces in OOP - they define a set of operations that types can implement.

**Common type classes:**
- `Eq` - Types that can be compared for equality (`==`, `/=`)
- `Ord` - Types that can be ordered (`<`, `>`, `<=`, `>=`)
- `Show` - Types that can be converted to strings
- `Num` - Numeric types that support arithmetic

### Using Type Class Constraints

**Haskell:**
```haskell
-- Function that works with ANY type that has equality
elemCount :: Eq a => a -> [a] -> Int
elemCount _ [] = 0
elemCount target (x:xs)
  | x == target = 1 + elemCount target xs  -- requires Eq for ==
  | otherwise   = elemCount target xs

-- The Eq a => part says: "type 'a' must support equality comparison"

-- Test:
-- elemCount 3 [1,2,3,3,4]      --> 2
-- elemCount 'a' "banana"       --> 3
-- elemCount "hi" ["hi","bye"]  --> 1
```

**Scala equivalent:**
```scala
def elemCount[A](target: A, list: List[A]): Int = list match {
  case Nil => 0
  case x :: xs if x == target => 1 + elemCount(target, xs)
  case _ :: xs => elemCount(target, xs)
}
```

**Key insight:** `Eq a =>` is a **constraint** that says "a must support equality."

---

### Deriving Type Classes

You can automatically generate type class instances with `deriving`.

**Haskell:**
```haskell
data Color = Red | Green | Blue
  deriving (Show, Eq, Ord)

-- Now you can:
-- show Red           --> "Red"    (Show)
-- Red == Green       --> False    (Eq)
-- Red < Green        --> True     (Ord - based on constructor order)
```

---

## Worked Examples

### Example 1: Multiple Recursive Calls - Merge Lists

**Problem:** Merge two sorted lists into one sorted list.

**Haskell implementation:**
```haskell
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys                    -- base: first empty
merge xs [] = xs                    -- base: second empty
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)  -- x smaller, keep it
  | otherwise = y : merge (x:xs) ys  -- y smaller, keep it

-- Test:
-- merge [1,3,5] [2,4,6]  --> [1,2,3,4,5,6]
-- merge [1,5,9] [2,3]    --> [1,2,3,5,9]
-- merge [] [1,2]         --> [1,2]
```

**Explanation:**
- Two base cases: when either list is empty
- Compare heads: take smaller one, recurse with remaining elements
- This is the core of **merge sort**!

---

### Example 2: Binary Tree Operations

**Problem:** Sum all values in a binary tree of integers.

**Haskell implementation:**
```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving Show

sumTree :: Num a => Tree a -> a
sumTree Empty = 0
sumTree (Node val left right) = val + sumTree left + sumTree right

-- Example tree:
--      5
--     / \
--    3   8
--   /
--  1
exampleTree = Node 5 (Node 3 (Node 1 Empty Empty) Empty)
                     (Node 8 Empty Empty)

-- Test:
-- sumTree exampleTree  --> 17  (5+3+1+8)
-- sumTree Empty        --> 0
```

**Explanation:**
- Empty tree has sum 0 (base case)
- Node: add current value + sum of left subtree + sum of right subtree
- **Two recursive calls**: one for left, one for right

---

### Example 3: Maybe Type for Error Handling

**Problem:** Safely get the nth element from a list (returns Nothing if out of bounds).

**Haskell implementation:**
```haskell
safeNth :: [a] -> Int -> Maybe a
safeNth [] _ = Nothing               -- empty list - out of bounds
safeNth (x:xs) 0 = Just x            -- found it at index 0
safeNth (x:xs) n
  | n < 0     = Nothing              -- negative index - invalid
  | otherwise = safeNth xs (n-1)     -- recurse, decrement index

-- Test:
-- safeNth [1,2,3] 0   --> Just 1
-- safeNth [1,2,3] 2   --> Just 3
-- safeNth [1,2,3] 5   --> Nothing
-- safeNth [1,2,3] (-1) --> Nothing
```

**Explanation:**
- Returns `Maybe a` instead of crashing on invalid index
- `Nothing` represents failure (like Scala's `None`)
- `Just value` represents success (like Scala's `Some(value)`)

---

### Example 4: Using Type Classes - Generic Maximum

**Problem:** Find maximum element in a non-empty list (works for any ordered type).

**Haskell implementation:**
```haskell
-- Returns Maybe because list might be empty
maximum' :: Ord a => [a] -> Maybe a
maximum' [] = Nothing
maximum' [x] = Just x                -- single element
maximum' (x:xs) = case maximum' xs of
  Nothing  -> Just x                 -- shouldn't happen if xs non-empty
  Just maxRest -> Just (max x maxRest)  -- compare x with max of rest

-- Test:
-- maximum' [3,1,4,1,5]  --> Just 5
-- maximum' [10]         --> Just 10
-- maximum' []           --> Nothing
-- maximum' "hello"      --> Just 'o'  (works with Char!)
```

**Explanation:**
- `Ord a =>` constraint allows use of `max` function
- Works with **any** ordered type: Int, Double, Char, String, etc.
- Returns `Maybe` to handle empty list safely

---

## Key Takeaways for This Week

### Multiple Recursive Calls
- Some problems need multiple recursive calls
- Common in tree structures (left subtree + right subtree)
- Examples: Fibonacci, tree traversal, merge sort

### Algebraic Data Types
- **Sum types**: Multiple alternatives (`Red | Green | Blue`)
- **Product types**: Combine values (`Point x y`)
- **Recursive types**: Contain themselves (`Tree a = Empty | Node a (Tree a) (Tree a)`)

### Pattern Matching Custom Types
- Match on constructors: `Red`, `Node val left right`
- Extract fields in pattern: `Point x y`
- Always handle all cases (exhaustive patterns)

### Polymorphic Types
- Type variables (lowercase): `a`, `b`, `c`
- Work like Scala generics: `[A]`
- Enable code reuse across different types

### Maybe Type
- Represents optional values: `Maybe a = Nothing | Just a`
- Like Scala's `Option[A] = None | Some(A)`
- Use for error handling without exceptions

### Type Classes
- `Eq a =>` - equality comparison required
- `Ord a =>` - ordering comparison required
- `Num a =>` - numeric operations required
- `Show a =>` - string conversion required
- Like Scala's type class pattern or context bounds

---

## Exercise Overview

Now that you've seen worked examples, try these exercises:

1. **quicksort** - Multiple recursive calls (divide and conquer)
2. **treeDepth** - Recursive tree operations
3. **treeMap** - Transform values in tree (polymorphic)
4. **flattenTree** - Convert tree to list
5. **lookup** - Search in association list with Maybe
6. **filterMaybe** - Filter list keeping only Just values
7. **compareLength** - Use Ord type class
8. **treeContains** - Use Eq type class with trees

**Strategy:**
- For multiple recursion: identify the subproblems
- For trees: think about Empty vs Node cases
- For Maybe: think about failure cases
- For type classes: let the types guide you

---

## Common Patterns

### 1. Multiple Recursive Calls
```haskell
func (Node val left right) =
  val `combine` func left `combine` func right
```

### 2. Tree Pattern Matching
```haskell
func Empty = baseCase
func (Node val left right) = recursiveCase
```

### 3. Maybe Pattern Matching
```haskell
func x = case maybeValue of
  Nothing -> handleFailure
  Just val -> handleSuccess val
```

### 4. Type Class Constraint
```haskell
func :: TypeClass a => a -> result
func x = ... -- can use TypeClass operations on x
```

---

## Common Pitfalls

❌ **Forgetting base cases** in multiple recursion (need one for each subtree/subproblem)
❌ **Not handling Empty** in tree functions
❌ **Forgetting type class constraints** when using ==, <, show, etc.
❌ **Confusing Maybe with lists** - `Just x` is not `[x]`
❌ **Wrong tree structure** - `Node val left right`, not `Node left val right`

✅ **Do handle all constructors** in pattern matching
✅ **Do use type variables** to make functions generic
✅ **Do use Maybe** for operations that might fail
✅ **Do add type class constraints** when needed
✅ **Do think recursively** for tree structures

---

## Next Steps

1. Work through `exercises.hs`
2. Test each function in GHCi with the provided examples
3. Run tests with `:load tests.hs` then `runAllTests`
4. Experiment with different tree shapes
5. Show me your solutions for review!

After completing Week 3, you'll be ready for **Weeks 4-5: Core Concepts** (higher-order functions, list comprehensions, advanced type classes).
