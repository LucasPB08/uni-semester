# Week 2: List Comprehensions

## Goals
- Master list comprehension syntax
- Understand generators, guards, and multiple generators
- Know when to use list comprehensions vs map/filter

## Context

List comprehensions are a concise way to create lists by **filtering and transforming** elements. They're syntactic sugar for combinations of `map` and `filter`, but often read more naturally.

**Syntax:**
```haskell
[expression | pattern <- list, condition1, condition2, ...]
```

---

## Basic Syntax (with Scala Comparisons)

### 1. Simple Transformation (Like Map)

**Haskell:**
```haskell
[x * 2 | x <- [1,2,3,4,5]]
-- Result: [2,4,6,8,10]

-- Reading: "x times 2, for each x drawn from [1..5]"
```

**Scala equivalent:**
```scala
for (x <- List(1,2,3,4,5)) yield x * 2
// or: List(1,2,3,4,5).map(_ * 2)
```

**Equivalent with map:**
```haskell
map (*2) [1,2,3,4,5]
```

---

### 2. Filtering with Guards

**Haskell:**
```haskell
[x | x <- [1..10], x > 5]
-- Result: [6,7,8,9,10]

-- Reading: "give me x, for each x from 1 to 10, where x > 5"
```

**Scala equivalent:**
```scala
for (x <- 1 to 10 if x > 5) yield x
// or: (1 to 10).filter(_ > 5).toList
```

**Equivalent with filter:**
```haskell
filter (> 5) [1..10]
```

---

### 3. Filter + Transform (The Sweet Spot)

This is where list comprehensions really shine - doing both at once!

**Haskell:**
```haskell
[x * 2 | x <- [1..10], even x]
-- Result: [4,8,12,16,20]

-- Reading: "double x, for each x from 1 to 10, where x is even"
```

**Scala equivalent:**
```scala
for (x <- 1 to 10 if x % 2 == 0) yield x * 2
```

**Equivalent with map/filter:**
```haskell
map (*2) (filter even [1..10])
```

---

### 4. Multiple Guards

You can have multiple conditions - they act like AND.

**Haskell:**
```haskell
[x | x <- [1..20], x > 5, x < 15, even x]
-- Result: [6,8,10,12,14]

-- Reading: "x from 1..20, where x > 5 AND x < 15 AND x is even"
```

**Scala equivalent:**
```scala
for (x <- 1 to 20 if x > 5 if x < 15 if x % 2 == 0) yield x
```

---

## Multiple Generators (Nested Loops)

### Cartesian Product

**Haskell:**
```haskell
[(x, y) | x <- [1,2,3], y <- ['a','b']]
-- Result: [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]
```

Think of it as nested loops:
```
for x in [1,2,3]:
    for y in ['a','b']:
        yield (x, y)
```

**Scala equivalent:**
```scala
for {
  x <- List(1,2,3)
  y <- List('a','b')
} yield (x, y)
```

---

### Dependent Generators

Later generators can use earlier variables!

**Haskell:**
```haskell
[(x, y) | x <- [1..3], y <- [1..x]]
-- Result: [(1,1),(2,1),(2,2),(3,1),(3,2),(3,3)]

-- For x=1: y goes from 1 to 1 -> (1,1)
-- For x=2: y goes from 1 to 2 -> (2,1), (2,2)
-- For x=3: y goes from 1 to 3 -> (3,1), (3,2), (3,3)
```

**Scala equivalent:**
```scala
for {
  x <- 1 to 3
  y <- 1 to x
} yield (x, y)
```

---

## Pattern Matching in Generators

You can destructure directly in the generator!

**Haskell:**
```haskell
-- Sum the pairs
[x + y | (x, y) <- [(1,2), (3,4), (5,6)]]
-- Result: [3,7,11]

-- Get first elements
[x | (x, _) <- [(1,'a'), (2,'b'), (3,'c')]]
-- Result: [1,2,3]
```

**Scala equivalent:**
```scala
for ((x, y) <- List((1,2), (3,4), (5,6))) yield x + y
```

---

## Worked Examples

### Example 1: Quicksort with List Comprehensions

The classic functional quicksort - beautifully clear with comprehensions:

```haskell
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
  where
    smaller = [y | y <- xs, y <= x]  -- all elements <= pivot
    larger  = [y | y <- xs, y > x]   -- all elements > pivot

-- Test:
-- quicksort [3,1,4,1,5,9,2,6]  --> [1,1,2,3,4,5,6,9]
```

**Reading:** "smaller is all y from xs where y <= x"

Compare to the filter version:
```haskell
smaller = filter (<= x) xs
larger  = filter (> x) xs
```

Both work! List comprehensions are often clearer when you want to name the element.

---

### Example 2: All Pairs

Generate all coordinate pairs in a grid:

```haskell
grid :: Int -> Int -> [(Int, Int)]
grid rows cols = [(r, c) | r <- [0..rows-1], c <- [0..cols-1]]

-- Test:
-- grid 2 3  --> [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
```

---

### Example 3: Pythagorean Triples

Find all Pythagorean triples up to a limit:

```haskell
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a, b, c) | a <- [1..n],
                       b <- [a..n],      -- b >= a to avoid duplicates
                       c <- [b..n],      -- c >= b
                       a*a + b*b == c*c] -- Pythagorean condition

-- Test:
-- pyths 15  --> [(3,4,5),(5,12,13),(6,8,10),(9,12,15)]
```

---

### Example 4: Flatten a List of Lists

```haskell
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

-- Test:
-- concat' [[1,2], [3,4,5], [6]]  --> [1,2,3,4,5,6]

-- Reading: "for each list xs in xss, for each element x in xs, give me x"
```

---

### Example 5: Prime Factors

Find all factors of a number:

```haskell
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- Test:
-- factors 12  --> [1,2,3,4,6,12]
-- factors 7   --> [1,7]
```

Check if a number is prime:

```haskell
isPrime :: Int -> Bool
isPrime n = factors n == [1, n]

-- Test:
-- isPrime 7   --> True
-- isPrime 12  --> False
```

Get all primes up to n:

```haskell
primes :: Int -> [Int]
primes n = [x | x <- [2..n], isPrime x]

-- Test:
-- primes 20  --> [2,3,5,7,11,13,17,19]
```

---

## Quick Reference

| What you want | List comprehension | Traditional |
|---------------|-------------------|-------------|
| Transform all | `[f x \| x <- xs]` | `map f xs` |
| Keep some | `[x \| x <- xs, p x]` | `filter p xs` |
| Transform + filter | `[f x \| x <- xs, p x]` | `map f (filter p xs)` |
| Pairs/products | `[(x,y) \| x <- xs, y <- ys]` | `concatMap (\x -> map (x,) ys) xs` |
| Flatten | `[x \| xs <- xss, x <- xs]` | `concat xss` |

---

## When to Use List Comprehensions?

**Use when:**
- Filtering AND transforming together
- Multiple generators (nested loops/products)
- The code reads more like English
- You want to name the element (x) in conditions

**Prefer map/filter when:**
- Simple transformation only: `map f xs` > `[f x | x <- xs]`
- Simple filter only: `filter p xs` > `[x | x <- xs, p x]`
- Using function composition: `map f . filter p` is idiomatic

---

## Key Takeaways

1. **Syntax**: `[expr | x <- list, guard1, guard2, ...]`
2. **Guards** are conditions that filter - multiple guards = AND
3. **Multiple generators** create nested loops (Cartesian products)
4. **Dependent generators** - later generators can use earlier variables
5. **Pattern matching** works in generators: `(x, y) <- pairs`
6. **Trade-off**: More readable for complex cases, but map/filter for simple ones

---

## Common Pitfalls

- **Don't forget the condition is a guard, not a filter call**:
  - `[x | x <- xs, even x]` (guards)
  - NOT `[x | x <- xs, filter even xs]`

- **Generator order matters for dependent generators**:
  - `[... | x <- ..., y <- f x]` works
  - `[... | y <- f x, x <- ...]` fails (x not in scope)

- **Don't overuse** - sometimes map/filter is cleaner

---

## Next Steps

1. Work through `exercises_list_comprehensions.hs`
2. Test in GHCi
3. Run tests: `:load tests_list_comprehensions.hs` then `runAllTests`
4. Try rewriting your week 3 quicksort using list comprehensions!
