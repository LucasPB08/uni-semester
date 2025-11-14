# Week 1: Advanced Exercises

## Overview
These exercises build on the fundamentals and introduce:
- **Tuples** (pairs and triples)
- **Multi-list recursion** (processing 2+ lists simultaneously)
- **Complex patterns** (combining guards, pattern matching, recursion)
- **Building multiple results** (returning pairs of lists)

**Difficulty:** Medium to Hard - these will challenge you!

---

## New Concepts: Tuples

### What Are Tuples?

Tuples are **fixed-size collections of heterogeneous values** (different types allowed).

**Haskell:**
```haskell
-- Pairs (2-tuples)
(1, "hello")        :: (Int, String)
(True, 'a', 3.14)   :: (Bool, Char, Double)  -- Triple

-- Functions on pairs
fst :: (a, b) -> a     -- Get first element
snd :: (a, b) -> b     -- Get second element
```

**Scala equivalent:**
```scala
val pair: (Int, String) = (1, "hello")
val triple: (Boolean, Char, Double) = (true, 'a', 3.14)

pair._1  // First element
pair._2  // Second element
```

**Key differences:**
- Haskell uses `fst` and `snd` (only for pairs!)
- Scala uses `._1`, `._2`, `._3`, etc.
- Tuples have **fixed sizes** known at compile time

---

## Pattern Matching on Tuples

**Haskell:**
```haskell
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)   -- Pattern match on pair, return new pair
```

**Scala equivalent:**
```scala
def swap[A, B](pair: (A, B)): (B, A) = pair match {
  case (x, y) => (y, x)
}
```

---

## Worked Examples

### Example 1: Zip Two Lists

**Problem:** Combine two lists element-by-element into pairs.

**Haskell implementation:**
```haskell
zipLists :: [a] -> [b] -> [(a, b)]
zipLists [] _          = []   -- If first list empty, done
zipLists _ []          = []   -- If second list empty, done
zipLists (x:xs) (y:ys) = (x, y) : zipLists xs ys  -- Pair heads, recurse on tails

-- Test in GHCi:
-- zipLists [1,2,3] ['a','b','c']  --> [(1,'a'),(2,'b'),(3,'c')]
-- zipLists [1,2] ['a','b','c']    --> [(1,'a'),(2,'b')]  (stops at shortest)
```

**Explanation:**
- **Two base cases:** Either list being empty means we're done
- **Pattern match both lists:** `(x:xs)` and `(y:ys)`
- **Create pair:** `(x, y)` combines the heads
- **Recurse:** Call `zipLists xs ys` on the tails
- **Cons the pair:** Put `(x, y)` at the front of the result

**Scala comparison:**
```scala
def zipLists[A, B](list1: List[A], list2: List[B]): List[(A, B)] = (list1, list2) match {
  case (Nil, _) => Nil
  case (_, Nil) => Nil
  case (x :: xs, y :: ys) => (x, y) :: zipLists(xs, ys)
}

// Or simply: list1.zip(list2)
```

---

### Example 2: Take N Elements

**Problem:** Return the first n elements of a list.

**Haskell implementation:**
```haskell
takeN :: Int -> [a] -> [a]
takeN n _
  | n <= 0    = []          -- Base case: non-positive n
takeN _ []    = []          -- Base case: empty list
takeN n (x:xs) = x : takeN (n-1) xs  -- Cons x, take (n-1) from tail

-- Test in GHCi:
-- takeN 3 [1,2,3,4,5]  --> [1,2,3]
-- takeN 5 [1,2]        --> [1,2]  (stops at end of list)
-- takeN 0 [1,2,3]      --> []
```

**Explanation:**
- **Guard for n <= 0:** Return empty list immediately
- **Pattern for empty list:** Can't take from nothing
- **Recursive case:** Include `x`, then take `(n-1)` more from `xs`
- **Two counters:** `n` counts down, list gets shorter

**Scala comparison:**
```scala
def takeN[A](n: Int, list: List[A]): List[A] = (n, list) match {
  case (n, _) if n <= 0 => Nil
  case (_, Nil) => Nil
  case (n, x :: xs) => x :: takeN(n - 1, xs)
}

// Or simply: list.take(n)
```

---

### Example 3: Unzip Pairs Into Two Lists

**Problem:** Split a list of pairs into two separate lists.

**Haskell implementation:**
```haskell
unzipPairs :: [(a, b)] -> ([a], [b])
unzipPairs []         = ([], [])   -- Base: empty list gives two empty lists
unzipPairs ((x,y):rest) =          -- Pattern match on pair in list
  let (xs, ys) = unzipPairs rest   -- Recursively unzip the tail
  in (x:xs, y:ys)                  -- Cons x to first list, y to second

-- Test in GHCi:
-- unzipPairs [(1,'a'),(2,'b'),(3,'c')]  --> ([1,2,3], ['a','b','c'])
-- unzipPairs []                         --> ([], [])
```

**Explanation:**
- **Base case:** Empty list unzips to a pair of empty lists `([], [])`
- **Pattern:** `(x, y):rest` matches a list whose head is a pair
- **Let binding:** Recursively unzip `rest` to get `(xs, ys)`
- **Build result:** Cons `x` onto first list, `y` onto second list
- **Return tuple:** `(x:xs, y:ys)` is a pair of lists

**Alternative (without let):**
```haskell
unzipPairs :: [(a, b)] -> ([a], [b])
unzipPairs [] = ([], [])
unzipPairs ((x,y):rest) = (x : fst recursed, y : snd recursed)
  where recursed = unzipPairs rest
```

**Scala comparison:**
```scala
def unzipPairs[A, B](list: List[(A, B)]): (List[A], List[B]) = list match {
  case Nil => (Nil, Nil)
  case (x, y) :: rest =>
    val (xs, ys) = unzipPairs(rest)
    (x :: xs, y :: ys)
}

// Or simply: list.unzip
```

---

## Key Takeaways for Advanced Exercises

### Multiple Pattern Matching
```haskell
-- Match on two lists simultaneously
zipLists [] _          = []
zipLists _ []          = []
zipLists (x:xs) (y:ys) = ...
```

### Guards + Patterns
```haskell
-- Combine guards with pattern matching
takeN n _
  | n <= 0    = []    -- Guard first
takeN _ []    = []    -- Then patterns
takeN n (x:xs) = ...
```

### Building Multiple Results
```haskell
-- Return tuples of lists
unzipPairs :: [(a,b)] -> ([a], [b])
unzipPairs [] = ([], [])  -- Return pair of empty lists
unzipPairs ((x,y):rest) = (x:xs, y:ys)  -- Build two lists
  where (xs, ys) = unzipPairs rest
```

### Type Constraints
```haskell
-- Eq a => means type 'a' must support equality (==)
removeFirst :: Eq a => a -> [a] -> [a]
removeFirst elem [] = []
removeFirst elem (x:xs)
  | x == elem = xs        -- Can use == because of Eq constraint
  | otherwise = x : removeFirst elem xs
```

---

## Common Patterns You'll Use

### 1. Recursion on Two Lists (Zip Pattern)
```haskell
process [] _ = ...
process _ [] = ...
process (x:xs) (y:ys) = ... process xs ys ...
```

### 2. Counting Down + List (Take/Drop Pattern)
```haskell
process n _
  | n <= 0 = ...
process _ [] = ...
process n (x:xs) = ... process (n-1) xs ...
```

### 3. Building Two Results (Unzip/Partition Pattern)
```haskell
process [] = ([], [])
process (x:xs) =
  let (list1, list2) = process xs
  in (x:list1, ...:list2)
```

---

## Exercise Strategy

1. **Read the worked examples** above carefully
2. **Identify the pattern** your exercise follows
3. **Write the type signature** first (it's given, but understand it!)
4. **Think about base cases** (empty lists, n <= 0, etc.)
5. **Implement recursive case** using the pattern
6. **Test incrementally** - don't wait until all exercises are done

---

## Difficulty Guide

**Medium:**
- Exercise 1 (swapPair) - Simple tuple manipulation
- Exercise 3 (takeN) - Counting down pattern
- Exercise 5 (dropN) - Similar to takeN
- Exercise 7 (mapPair) - Function application on tuples

**Medium-Hard:**
- Exercise 2 (zipLists) - Two-list recursion
- Exercise 4 (reverseList) - Append at end (tricky!)
- Exercise 8 (removeFirst) - Equality checking with guards

**Hard:**
- Exercise 6 (unzipPairs) - Building two lists from pairs
- Exercise 9 (partitionBy) - Conditional splitting into two lists

---

## Testing

Use `tests_advanced.hs` to verify your solutions:

```bash
ghci
:load tests_advanced.hs
runAllTests
```

Good luck! These are challenging - take your time and refer back to the examples.
