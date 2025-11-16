# Currying in Haskell

## What is Currying?

**Currying** is the technique of transforming a function that takes multiple arguments into a sequence of functions that each take a single argument.

In Haskell, **ALL functions are curried by default**. What looks like a function taking multiple arguments is actually a function that takes one argument and returns another function.

---

## Type Signatures: What They Really Mean

### Example 1: Simple Addition

```haskell
add :: Int -> Int -> Int
add x y = x + y
```

**How to read this signature:**

```haskell
add :: Int -> Int -> Int
--     ^^^    ^^^^^^^^
--     input  RETURN VALUE (which is itself a function!)
```

With explicit parentheses (right-associative):

```haskell
add :: Int -> (Int -> Int)
--     ^^^    ^^^^^^^^^^^
--     |      Returns a function that takes an Int and returns an Int
--     Takes an Int
```

**What this means:**
- `add` takes an `Int`
- `add` returns a function of type `Int -> Int`
- That returned function takes an `Int` and returns an `Int`

### In Practice:

```haskell
add :: Int -> Int -> Int
add x y = x + y

-- These are ALL equivalent:
add 3 5           --> 8
(add 3) 5         --> 8  (explicitly showing intermediate function)
let add3 = add 3  -- add3 is a NEW function of type Int -> Int
    in add3 5     --> 8
```

---

## Example 2: Three Parameters

```haskell
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
```

**With parentheses:**

```haskell
addThree :: Int -> (Int -> (Int -> Int))
```

**Step-by-step evaluation:**

```haskell
addThree 1 2 3

-- Step 1: Apply 1
= (addThree 1) 2 3
-- addThree 1 returns a function of type Int -> Int -> Int

-- Step 2: Apply 2
= ((addThree 1) 2) 3
-- (addThree 1) 2 returns a function of type Int -> Int

-- Step 3: Apply 3
= (((addThree 1) 2) 3)
-- Returns 6
```

---

## The Confusing Part: Parameters in Implementation

### The Question:
"If the type signature is `(a -> b) -> (a -> c)`, why can I write `f x` in the implementation?"

### The Answer:

```haskell
-- Type signature says: returns a function (a -> c)
compose :: (b -> c) -> (a -> b) -> (a -> c)
--                                  ^^^^^^^^
--                                  This is a function type!
--                                  It needs an 'a' to produce a 'c'

-- Because of currying, these signatures are IDENTICAL:
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose :: (b -> c) -> (a -> b) -> a -> c    -- Just removed outer parens!
--                                  ^    ^
--                                  |    |
--                            input  output of the returned function
```

**So these implementations are all equivalent:**

```haskell
-- Version 1: Return a lambda explicitly
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- Version 2: Just spell out all parameters (most common)
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- Version 3: Using where
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = newFun where newFun x = f (g x)
```

---

## Practical Example: Partial Application

Currying enables **partial application** - calling a function with fewer arguments than it expects, creating a new specialized function.

### Example: Building Specialized Functions

```haskell
-- Generic multiplication
multiply :: Int -> Int -> Int
multiply x y = x * y

-- Partially apply to create specialized functions
double :: Int -> Int
double = multiply 2    -- Partially apply multiply with first arg = 2

triple :: Int -> Int
triple = multiply 3

timesHundred :: Int -> Int
timesHundred = multiply 100

-- Usage:
double 5         --> 10
triple 4         --> 12
timesHundred 3   --> 300
```

### Example: With Filter and Map

```haskell
-- filter has type: (a -> Bool) -> [a] -> [a]

-- Partially apply filter with a predicate
keepEvens :: [Int] -> [Int]
keepEvens = filter even    -- Only provided first arg!

keepBig :: [Int] -> [Int]
keepBig = filter (> 100)

-- map has type: (a -> b) -> [a] -> [b]

-- Partially apply map with a function
doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

uppercaseAll :: [String] -> [String]
uppercaseAll = map (map toUpper)  -- map on outer list, map on each string
```

---

## Scala Comparison

### Scala (Not Curried by Default):

```scala
// Standard form (not curried)
def add(x: Int, y: Int): Int = x + y

// Can't do partial application easily:
val add3 = add(3, _)  // Need placeholder syntax

// Curried form (explicit)
def addCurried(x: Int)(y: Int): Int = x + y

// Now partial application works:
val add3 = addCurried(3)
add3(5)  // --> 8
```

### Haskell (Curried by Default):

```haskell
-- Already curried automatically!
add :: Int -> Int -> Int
add x y = x + y

-- Partial application is natural:
add3 = add 3
add3 5  --> 8
```

---

## Common Currying Patterns

### Pattern 1: Building Comparators

```haskell
greaterThan :: Int -> Int -> Bool
greaterThan threshold value = value > threshold

-- Partially apply to build specialized comparators:
isAdult :: Int -> Bool
isAdult = greaterThan 18

isBig :: Int -> Bool
isBig = greaterThan 1000

-- Usage:
isAdult 25   --> True
isAdult 15   --> False
isBig 500    --> False
```

### Pattern 2: Configuration Functions

```haskell
replicate :: Int -> a -> [a]
-- replicate 3 'a' --> "aaa"

-- Partially apply the count:
threeTimes :: a -> [a]
threeTimes = replicate 3

tenTimes :: a -> [a]
tenTimes = replicate 10

-- Usage:
threeTimes 'x'   --> "xxx"
tenTimes 5       --> [5,5,5,5,5,5,5,5,5,5]
```

### Pattern 3: Function Composition

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
-- Composition operator is curried!

-- Partially apply composition:
addThenDouble :: Int -> Int
addThenDouble = (*2) . (+10)
-- Reads: "add 10, THEN multiply by 2"

-- Usage:
addThenDouble 5   --> 30  (5+10=15, 15*2=30)
```

---

## Reading Type Signatures with Currying in Mind

### Rule: The Last Type is the Return Value, Everything Else is Input

```haskell
map :: (a -> b) -> [a] -> [b]
--     ^^^^^^^^^    ^^^    ^^^
--     input 1      in2    RETURN

filter :: (a -> Bool) -> [a] -> [a]
--        ^^^^^^^^^^^    ^^^    ^^^
--        input 1        in2    RETURN

foldr :: (a -> b -> b) -> b -> [a] -> b
--       ^^^^^^^^^^^^^    ^    ^^^    ^
--       input 1          in2  in3    RETURN
```

### But Also: Every Arrow Returns a Function!

```haskell
map :: (a -> b) -> [a] -> [b]

-- Can also be read as:
map :: (a -> b) -> ([a] -> [b])
--     ^^^^^^^^^^^  ^^^^^^^^^^^^
--     Take this    Return this function

-- So:
doubleAll = map (*2)
-- doubleAll has type: [Int] -> [Int]
-- It's the function returned by partially applying map!
```

---

## Key Takeaways

1. **All Haskell functions are curried** - they take one argument at a time
2. **Type signatures are right-associative**: `a -> b -> c` means `a -> (b -> c)`
3. **Implementation parameters don't need to match signature exactly** because of currying:
   - Signature: `f :: a -> b -> c`
   - Implementation: Can write `f x y = ...` (all params) or `f x = \y -> ...` (lambda)
4. **Partial application is automatic** - just provide fewer arguments
5. **The last type is always the final return value** - everything before it is an input

---

## Mental Model

Think of a multi-parameter function as a **chain of single-parameter functions**:

```haskell
add :: Int -> Int -> Int
add x y = x + y

-- Mental model:
-- add is a function that:
--   1. Takes an Int (x)
--   2. Returns a function that:
--      - Takes an Int (y)
--      - Returns an Int (x + y)
```

This is why partial application works so naturally - you're just stopping the chain partway!
