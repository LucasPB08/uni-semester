# Week 1: Haskell Fundamentals

## Goals
- Learn Haskell-specific syntax (coming from Scala)
- Understand Haskell's approach to pattern matching and guards
- Practice recursion in pure functional style (no mutable vars!)
- Get comfortable with GHCi workflow

## Context: Haskell vs Scala
You already know FP from Scala, so this week focuses on **Haskell-specific syntax** and **pure functional thinking**. The main differences:
- **No mutation, ever** - Scala lets you use `var`, Haskell doesn't
- **Lazy by default** - Expressions aren't evaluated until needed
- **No null** - Use `Maybe` instead (like Scala's `Option`)
- **Whitespace-sensitive** - Indentation matters (like Python)
- **Pattern matching syntax** - Different from Scala's `match`

---

## GHCi Setup

### Starting GHCi
```bash
ghci
```

### Loading Your Exercise File
```haskell
:load exercises.hs    -- or :l exercises.hs
:reload               -- or :r (after making changes)
:type expr            -- or :t (check type of expression)
:quit                 -- or :q
```

---

## Haskell Syntax Guide (with Scala Comparisons)

### 1. Function Definitions

**Haskell:**
```haskell
add :: Int -> Int -> Int
add x y = x + y
```

**Scala equivalent:**
```scala
def add(x: Int, y: Int): Int = x + y
```

**Key differences:**
- Type signature is **separate** from implementation
- No parentheses around parameters
- All functions are curried by default (partial application is automatic)

### 2. Pattern Matching on Lists

**Haskell:**
```haskell
-- Pattern matching in function definition
listLength :: [a] -> Int
listLength []     = 0           -- empty list
listLength (x:xs) = 1 + listLength xs  -- head:tail
```

**Scala equivalent:**
```scala
def listLength[A](list: List[A]): Int = list match {
  case Nil => 0
  case x :: xs => 1 + listLength(xs)
}
```

**Key differences:**
- No `match` keyword - patterns go directly in function clauses
- Use `[]` for empty list (not `Nil`)
- Use `:` for cons (not `::`)
- Parentheses required around `(x:xs)` pattern

### 3. Guards (Conditional Logic)

**Haskell:**
```haskell
signum :: Int -> Int
signum n
  | n > 0     = 1
  | n < 0     = -1
  | otherwise = 0
```

**Scala equivalent:**
```scala
def signum(n: Int): Int = {
  if (n > 0) 1
  else if (n < 0) -1
  else 0
}
```

**Key differences:**
- Use `|` before each condition (guards)
- `otherwise` is like `else` (it's actually just `True`)
- No parentheses around conditions
- Guards are evaluated top-to-bottom

---

## Worked Examples

### Example 1: Sum of List (Manual Recursion)

**Problem:** Implement sum without using built-in functions.

**Haskell implementation:**
```haskell
-- Type signature: list of any numeric type 'a' -> returns 'a'
sumList :: Num a => [a] -> a
sumList []     = 0              -- base case: empty list sums to 0
sumList (x:xs) = x + sumList xs -- recursive case: add head to sum of tail

-- Test in GHCi:
-- sumList [1,2,3,4]  --> 10
-- sumList []         --> 0
```

**Explanation:**
- `Num a =>` is a **type constraint** (like Scala's context bound `A: Numeric`)
- Base case: empty list sums to 0
- Recursive case: take first element `x`, add it to sum of remaining elements `xs`
- No loop variables, no mutable accumulator - pure recursion

**Scala comparison:**
```scala
def sumList[A: Numeric](list: List[A]): A = list match {
  case Nil => 0
  case x :: xs => x + sumList(xs)
}
```

---

### Example 2: Filter with Guards

**Problem:** Keep only even numbers from a list.

**Haskell implementation:**
```haskell
keepEvens :: [Int] -> [Int]
keepEvens [] = []                          -- base case
keepEvens (x:xs)
  | even x    = x : keepEvens xs           -- if even, include in result
  | otherwise = keepEvens xs               -- if odd, skip it

-- Test in GHCi:
-- keepEvens [1,2,3,4,5,6]  --> [2,4,6]
-- keepEvens []             --> []
```

**Explanation:**
- Pattern match on list structure
- Use guards to check condition (`even` is a built-in function)
- If even: cons `x` onto the filtered tail with `x : keepEvens xs`
- If odd: just return the filtered tail
- The `:` operator is **cons** - it prepends an element to a list

**Scala comparison:**
```scala
def keepEvens(list: List[Int]): List[Int] = list match {
  case Nil => Nil
  case x :: xs if x % 2 == 0 => x :: keepEvens(xs)
  case _ :: xs => keepEvens(xs)
}
```

---

### Example 3: Building a List with Recursion

**Problem:** Create a list of first n natural numbers: `[1,2,3,...,n]`

**Haskell implementation:**
```haskell
range :: Int -> [Int]
range n
  | n <= 0    = []                  -- base case: non-positive returns empty
  | otherwise = range (n-1) ++ [n]  -- recursive: build (n-1) range, append n

-- Test in GHCi:
-- range 5   --> [1,2,3,4,5]
-- range 0   --> []
-- range (-3) --> []
```

**Explanation:**
- Guards handle edge cases (n <= 0)
- Recursively build range for `n-1`, then append `n` at the end
- `++` is list concatenation operator
- **Note:** This is inefficient (quadratic time) - you'll learn better approaches later

**Alternative (more efficient):**
```haskell
range' :: Int -> [Int]
range' n
  | n <= 0    = []
  | otherwise = 1 : range' (n-1)  -- cons 1 at front, increment recursively

-- Wait, this gives [1,1,1,1,1]! We need a helper:
rangeHelper :: Int -> Int -> [Int]
rangeHelper current n
  | current > n = []
  | otherwise   = current : rangeHelper (current+1) n

range'' :: Int -> [Int]
range'' n = rangeHelper 1 n
```

**Scala comparison:**
```scala
def range(n: Int): List[Int] = {
  if (n <= 0) Nil
  else (1 to n).toList  // Scala has built-in range
}
```

---

## Key Takeaways for This Week

### Pattern Matching
- **Multiple function clauses** instead of one `match` expression
- Match patterns directly in function definition
- Always handle all cases (compiler warns about non-exhaustive patterns)

### List Operations
- `[]` = empty list
- `x:xs` = head and tail (pattern)
- `:` = cons operator (prepend)
- `++` = concatenation

### Pure Recursion
- No loop counters or mutable state
- Think: "What's the base case? What's the recursive case?"
- Build results by combining recursive calls

### Type Signatures
- Separate from implementation
- `->` separates argument types and return type
- Type constraints with `=>`  (e.g., `Num a => [a] -> a`)

---

## Exercise Overview

Now that you've seen worked examples, try these similar exercises:

1. **add** - Basic two-parameter function ✓ (you completed this)
2. **both** - Boolean AND operation ✓ (you completed this)
3. **describeNumber** - Practice guards (like `signum` example)
4. **countElements** - Recursion on lists (like `sumList` example)
5. **makeRange** - Build a list (like `range` example)
6. **doubleAll** - Transform list elements (similar to filter pattern)
7. **keepPositive** - Combine guards + recursion (like `keepEvens` example)

**Strategy:**
- Look at the worked examples above
- Try to adapt the pattern to the new problem
- Don't just copy - make sure you understand WHY each part works

---

## Common Pitfalls (Coming from Scala)

❌ **Don't try to use `match`** - pattern matching is built into function definitions
❌ **Don't forget parentheses** around `(x:xs)` in patterns
❌ **Don't use `::` for cons** - that's Scala; Haskell uses `:`
❌ **Don't use `Nil`** - Haskell uses `[]`
❌ **Watch your indentation** - Haskell is whitespace-sensitive

✅ **Do use recursion** instead of thinking about loops
✅ **Do pattern match** in function clauses
✅ **Do use guards** for conditional logic
✅ **Do use `:` to cons** and `++` to concatenate

---

## Next Steps

1. Work through `exercises.hs`
2. Test each function in GHCi
3. When stuck, refer back to the worked examples
4. Show me your solutions for review!
