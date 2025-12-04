# Function Composition and Point-Free Style

## Function Composition (.)

### What is Composition?

Function composition combines two functions into a new function. The output of the second function becomes the input of the first.

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)
```

**Key insight:** Read `.` as "after" — `f . g` means "f after g".

---

### Basic Example

```haskell
double :: Int -> Int
double x = x * 2

addOne :: Int -> Int
addOne x = x + 1

-- Compose them:
doubleThenAddOne :: Int -> Int
doubleThenAddOne = addOne . double

-- Evaluation:
-- doubleThenAddOne 5
-- = (addOne . double) 5
-- = addOne (double 5)
-- = addOne 10
-- = 11
```

**Reading order:** Right to left!
- `addOne . double` means: first `double`, then `addOne`

---

### Scala Comparison

```scala
// Scala
val double: Int => Int = _ * 2
val addOne: Int => Int = _ + 1

// compose: f.compose(g) = f(g(x)) — same as Haskell's (.)
val doubleThenAddOne = addOne.compose(double)

// andThen: f.andThen(g) = g(f(x)) — reversed order
val doubleThenAddOne = double.andThen(addOne)
```

Haskell's `.` is like Scala's `compose` (right-to-left).

---

### Chaining Multiple Functions

```haskell
-- Process pipeline: trim -> uppercase -> add exclamation
import Data.Char (toUpper)

process :: String -> String
process = (++ "!") . map toUpper . filter (/= ' ')

-- Evaluation of process "hello world":
-- 1. filter (/= ' ') "hello world" = "helloworld"
-- 2. map toUpper "helloworld" = "HELLOWORLD"
-- 3. (++ "!") "HELLOWORLD" = "HELLOWORLD!"
```

**Reading:** Right to left = execution order left to right in pipeline.

---

## Point-Free Style

### What is Point-Free?

Point-free style defines functions **without explicitly naming their arguments**. The "points" are the arguments.

```haskell
-- Pointful (with arguments):
sum' :: [Int] -> Int
sum' xs = foldr (+) 0 xs

-- Point-free (no arguments):
sum' :: [Int] -> Int
sum' = foldr (+) 0
```

---

### Why Does This Work?

Because of **partial application**!

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr (+) :: Num a => a -> [a] -> a      -- partially applied with (+)
foldr (+) 0 :: Num a => [a] -> a         -- partially applied with 0
```

`foldr (+) 0` is already a function `[a] -> a`. No need to write `xs` on both sides!

---

### Common Patterns

#### Pattern 1: Removing the Last Argument

```haskell
-- Pointful:
doubleAll xs = map (*2) xs
keepEvens xs = filter even xs
sumSquares xs = sum (map (^2) xs)

-- Point-free:
doubleAll = map (*2)
keepEvens = filter even
sumSquares = sum . map (^2)
```

#### Pattern 2: Composition Chains

```haskell
-- Pointful:
process x = f (g (h x))

-- Point-free:
process = f . g . h
```

#### Pattern 3: With Operators

```haskell
-- Pointful:
addTen x = x + 10
multiplyByTwo x = x * 2

-- Point-free (using sections):
addTen = (+ 10)
multiplyByTwo = (* 2)
```

---

### When to Use Point-Free

**Use when:**
- Simple composition chains: `f . g . h`
- Single transformation: `map f`, `filter p`
- It makes intent clearer

**Avoid when:**
- Multiple arguments needed in complex ways
- It becomes hard to read
- You need to name arguments for clarity

```haskell
-- Good point-free:
processNames = map toUpper . unwords . sort

-- Bad point-free (too complex):
foo = (. (++)) . flip . (flip .) . (.)
-- Just write it with arguments!
```

---

## The ($) Operator

### What is ($)?

`$` is function application with **lowest precedence**. It's used to avoid parentheses.

```haskell
($) :: (a -> b) -> a -> b
f $ x = f x
```

---

### Removing Parentheses

```haskell
-- With parentheses:
sum (map (*2) (filter even [1..10]))

-- With $:
sum $ map (*2) $ filter even [1..10]

-- Reading: apply sum to (apply map (*2) to (apply filter even to [1..10]))
```

`$` says: "evaluate everything to the right first, then apply the function on the left."

---

### Combining (.) and ($)

```haskell
-- Common pattern: composition chain applied to an argument
result = f . g . h $ x

-- Equivalent to:
result = (f . g . h) x
result = f (g (h x))
```

**Rule of thumb:**
- Use `.` to build the pipeline
- Use `$` to apply it to data

```haskell
-- Build pipeline, then apply
processData = clean . transform . validate
result = processData $ inputData

-- Or inline:
result = clean . transform . validate $ inputData
```

---

## Quick Reference

| What | Syntax | Meaning |
|------|--------|---------|
| Compose | `f . g` | `\x -> f (g x)` |
| Apply | `f $ x` | `f x` (low precedence) |
| Section | `(+ 10)` | `\x -> x + 10` |
| Section | `(10 +)` | `\x -> 10 + x` |

### Operator Precedence

```haskell
-- Highest to lowest:
-- Function application (whitespace): f x
-- .  (composition): infixr 9
-- $  (application): infixr 0  (lowest)

-- So:
f . g $ x   =   (f . g) $ x   =   (f . g) x
```

---

## Examples

### Example 1: Processing Pipeline

```haskell
-- Count words longer than 3 characters
countLongWords :: String -> Int
countLongWords = length . filter ((> 3) . length) . words

-- Test:
-- countLongWords "the quick brown fox" = 2  (quick, brown)
```

### Example 2: Composition with Multiple Arguments

```haskell
-- Point-free with multiple args is trickier:
-- add x y = x + y

-- To make point-free:
add = (+)  -- (+) is already a function of two args

-- But for more complex cases, sometimes pointful is clearer
```

### Example 3: Practical Data Transformation

```haskell
import Data.Char (toLower)
import Data.List (sort, nub)

-- Get unique sorted lowercase words
uniqueSortedWords :: String -> [String]
uniqueSortedWords = sort . nub . map (map toLower) . words

-- Test:
-- uniqueSortedWords "Hello hello WORLD world" = ["hello", "world"]
```

---

## Key Takeaways

1. **Composition (.)** combines functions right-to-left: `f . g` = "g then f"
2. **Point-free** omits arguments when partial application makes them redundant
3. **($)** is low-precedence application — use to avoid parentheses
4. **Don't overdo it** — clarity beats cleverness
5. **Read composition chains** right-to-left for execution order
