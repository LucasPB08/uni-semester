# List Comprehensions in Haskell

## Quick Overview

List comprehensions are a concise way to create lists by **filtering and transforming** elements from existing lists. Think of them as a declarative "recipe" for building new lists.

**Syntax:**
```haskell
[expression | pattern <- list, condition1, condition2, ...]
```

## Basic Examples

### 1. Simple Transformation (Map)

**Haskell:**
```haskell
[x * 2 | x <- [1,2,3,4,5]]
-- Result: [2,4,6,8,10]
```

**Scala equivalent:**
```scala
(1 to 5).map(_ * 2).toList
// or: for (x <- 1 to 5) yield x * 2
```

**Reading it:** "double x, for each x drawn from [1,2,3,4,5]"

---

### 2. Filtering (Filter)

**Haskell:**
```haskell
[x | x <- [1..10], x > 5]
-- Result: [6,7,8,9,10]
```

**Scala equivalent:**
```scala
(1 to 10).filter(_ > 5).toList
```

**Reading it:** "give me x, for each x from 1 to 10, where x > 5"

---

### 3. Filter + Transform

**Haskell:**
```haskell
[x * 2 | x <- [1..10], even x]
-- Result: [4,8,12,16,20]
```

**Scala equivalent:**
```scala
(1 to 10).filter(_ % 2 == 0).map(_ * 2).toList
```

**Reading it:** "double x, for each x from 1 to 10, where x is even"

---

## Quicksort Example

**Your foldr version:**
```haskell
quicksort (x:xs) =
  let halves = foldr (\el acc -> if el <= x then (el : (fst acc), snd acc)
                                 else (fst acc, el : snd acc)) ([], []) xs
  in quicksort (fst halves) ++ [x] ++ quicksort (snd halves)
```

**List comprehension version:**
```haskell
quicksort [] = []
quicksort (x:xs) =
  let smaller = [y | y <- xs, y <= x]   -- filter elements <= pivot
      larger  = [y | y <- xs, y > x]    -- filter elements > pivot
  in quicksort smaller ++ [x] ++ quicksort larger
```

**Why it's clearer:**
- Reads like English: "get all y from xs where y <= x"
- No manual tuple manipulation
- No fst/snd accessors
- Separates the two partitions clearly

---

## Multiple Generators (Cartesian Product)

**Haskell:**
```haskell
[(x, y) | x <- [1,2,3], y <- ['a','b']]
-- Result: [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]
```

**Scala equivalent:**
```scala
for {
  x <- List(1,2,3)
  y <- List('a','b')
} yield (x, y)
```

---

## Pattern Matching in Generators

**Haskell:**
```haskell
[x + y | (x, y) <- [(1,2), (3,4), (5,6)]]
-- Result: [3,7,11]
```

Destructures tuples directly in the generator!

---

## Common Patterns

### Map
```haskell
[f x | x <- list]           -- map f list
```

### Filter
```haskell
[x | x <- list, condition]  -- filter condition list
```

### Filter + Map
```haskell
[f x | x <- list, condition]  -- map f (filter condition list)
```

---

## Quick Reference

| What you want | List comprehension | Traditional |
|---------------|-------------------|-------------|
| Double all | `[x*2 \| x <- xs]` | `map (*2) xs` |
| Keep evens | `[x \| x <- xs, even x]` | `filter even xs` |
| Double evens | `[x*2 \| x <- xs, even x]` | `map (*2) (filter even xs)` |

---

## When to Use List Comprehensions?

✅ **Use when:**
- Filtering and/or transforming at once
- Multiple generators (nested loops)
- Code reads more naturally

✅ **Don't use when:**
- Simple map: `map f xs` is clearer than `[f x | x <- xs]`
- Simple filter: `filter p xs` is clearer than `[x | x <- xs, p x]`
- Complex logic: named functions are better

---

## Scala Connection

Haskell list comprehensions ≈ Scala's for-comprehensions:

```scala
// Scala
for {
  x <- List(1,2,3,4,5)
  if x % 2 == 0
} yield x * 2
```

```haskell
-- Haskell
[x * 2 | x <- [1,2,3,4,5], even x]
```

Both are **syntactic sugar** for monadic operations (map/filter/flatMap).
