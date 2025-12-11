# Week 4: Monads and Lazy Evaluation

## Goals
- Understand what a Monad is and why it matters
- Use the Maybe monad for error handling
- Use the List monad for nondeterminism
- Deeply understand do-notation as syntactic sugar
- Master lazy evaluation with infinite structures
- Recognize common monadic patterns

## Prerequisites
You should be comfortable with:
- Functor and `fmap` (week 3)
- Basic IO and do-notation (week 3)
- Higher-order functions (week 2)

---

## Part 1: What is a Monad?

### The Functor Limitation

With Functor, you can apply a function `a -> b` inside a container:
```haskell
fmap :: Functor f => (a -> b) -> f a -> f b

fmap (+1) (Just 5)  -- Just 6
fmap (*2) [1,2,3]   -- [2,4,6]
```

But what if your function **also returns a wrapped value**?

```haskell
-- Function that might fail
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

-- This doesn't work!
fmap (safeDiv 10) (Just 5)  -- Just (Just 2) ... nested Maybe!
```

We get `Maybe (Maybe Int)` instead of `Maybe Int`. We need a way to "flatten" nested contexts.

---

### The Monad Typeclass

```haskell
class Applicative m => Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b   -- "bind"
  return :: a -> m a                    -- wrap pure value
```

**Reading `>>=` (bind):**
- Takes a wrapped value `m a`
- Takes a function `a -> m b` that produces a wrapped value
- Returns `m b` (flattened, not `m (m b)`)

**The key insight:** `>>=` lets you chain operations that each return wrapped values, handling the "unwrapping" for you.

---

### Monad Laws

Every Monad must satisfy three laws:

```haskell
-- 1. Left identity: return doesn't change bind behavior
return x >>= f  ==  f x

-- 2. Right identity: binding return changes nothing
m >>= return  ==  m

-- 3. Associativity: grouping doesn't matter
(m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)
```

Don't memorize these - just understand that monads must behave predictably.

---

### Scala Comparison

```scala
// Scala flatMap is Haskell >>=
val result = Some(5).flatMap(x => Some(x + 1))  // Some(6)

// This is the same as:
val result = for {
  x <- Some(5)
  y <- Some(x + 1)
} yield y
```

```haskell
-- Haskell equivalent
result = Just 5 >>= \x -> Just (x + 1)  -- Just 6

-- Or with do-notation:
result = do
  x <- Just 5
  return (x + 1)
```

**Key mapping:**
| Scala | Haskell |
|-------|---------|
| `flatMap` | `>>=` (bind) |
| `map` | `fmap` / `<$>` |
| `for comprehension` | do-notation |

---

## Part 2: The Maybe Monad

### Maybe for Error Handling

`Maybe` represents computations that might fail:

```haskell
instance Monad Maybe where
  Nothing >>= _  = Nothing   -- Failure propagates
  Just x  >>= f  = f x       -- Success continues

  return = Just
```

**The magic:** If any step fails (`Nothing`), the whole chain fails. No manual checking needed!

---

### Chaining Maybe Operations

```haskell
-- Safe operations
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

-- Chain them: get second element
safeSecond :: [a] -> Maybe a
safeSecond xs = safeTail xs >>= safeHead

-- Examples:
safeSecond [1,2,3]  -- Just 2
safeSecond [1]      -- Nothing (safeTail succeeds, safeHead fails)
safeSecond []       -- Nothing (safeTail fails immediately)
```

---

### Do-Notation with Maybe

Do-notation works with any Monad, not just IO!

```haskell
-- Get third element
safeThird :: [a] -> Maybe a
safeThird xs = do
  t1 <- safeTail xs      -- Drop first
  t2 <- safeTail t1      -- Drop second
  safeHead t2            -- Get head of remainder

-- Equivalent without do-notation:
safeThird xs =
  safeTail xs >>= \t1 ->
  safeTail t1 >>= \t2 ->
  safeHead t2
```

**Do-notation is just syntactic sugar for `>>=` chains!**

---

### Real Example: User Lookup

```haskell
type UserId = Int
type Username = String
type Email = String

-- Pretend these look up in a database
findUser :: UserId -> Maybe Username
findUser 1 = Just "alice"
findUser 2 = Just "bob"
findUser _ = Nothing

findEmail :: Username -> Maybe Email
findEmail "alice" = Just "alice@email.com"
findEmail "bob"   = Just "bob@email.com"
findEmail _       = Nothing

-- Chain lookups
getUserEmail :: UserId -> Maybe Email
getUserEmail uid = do
  username <- findUser uid
  findEmail username

-- Examples:
getUserEmail 1  -- Just "alice@email.com"
getUserEmail 3  -- Nothing (user not found)
```

---

## Part 3: The List Monad

### Lists for Nondeterminism

The List monad represents **multiple possible results**:

```haskell
instance Monad [] where
  xs >>= f = concat (map f xs)   -- Apply f to each, flatten
  return x = [x]                  -- Single-element list
```

**Intuition:** Each element represents a possible value. `>>=` tries all combinations.

---

### Basic List Monad Examples

```haskell
-- Apply function to each element, flatten results
[1,2,3] >>= \x -> [x, x*10]
-- [1,10,2,20,3,30]

-- How it works:
-- map (\x -> [x, x*10]) [1,2,3] = [[1,10], [2,20], [3,30]]
-- concat = [1,10,2,20,3,30]
```

---

### Do-Notation with Lists

Do-notation with lists gives you **list comprehensions**!

```haskell
-- All pairs from two lists
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do
  x <- xs
  y <- ys
  return (x, y)

pairs [1,2] ['a','b']
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

-- This is equivalent to:
pairs xs ys = [(x,y) | x <- xs, y <- ys]
```

---

### Filtering in the List Monad

Use `guard` to filter possibilities:

```haskell
import Control.Monad (guard)

-- guard :: Bool -> [()]
-- guard True  = [()]
-- guard False = []

-- Pythagorean triples up to n
triples :: Int -> [(Int, Int, Int)]
triples n = do
  a <- [1..n]
  b <- [a..n]              -- b >= a to avoid duplicates
  c <- [b..n]              -- c >= b
  guard (a*a + b*b == c*c) -- Filter
  return (a, b, c)

triples 20
-- [(3,4,5),(5,12,13),(6,8,10),(8,15,17),(9,12,15),(12,16,20)]
```

**How `guard` works:** Returns empty list on False (pruning that branch), or `[()]` on True (continuing).

---

### Scala Comparison

```scala
// Scala for comprehension with filter
val triples = for {
  a <- 1 to 20
  b <- a to 20
  c <- b to 20
  if a*a + b*b == c*c
} yield (a, b, c)
```

```haskell
-- Haskell equivalent
triples = do
  a <- [1..20]
  b <- [a..20]
  c <- [b..20]
  guard (a*a + b*b == c*c)
  return (a, b, c)
```

---

## Part 4: Do-Notation Desugaring

Understanding that do-notation is **just syntactic sugar** is crucial.

### The Rules

```haskell
-- Rule 1: Last expression (no binding)
do { e }  =  e

-- Rule 2: Binding with <-
do { x <- e; rest }  =  e >>= \x -> do { rest }

-- Rule 3: Expression without binding
do { e; rest }  =  e >> do { rest }

-- Rule 4: let in do
do { let x = e; rest }  =  let x = e in do { rest }
```

---

### Desugaring Example

```haskell
-- Original
example = do
  x <- getLine
  let y = map toUpper x
  putStrLn y
  return (length y)

-- Desugared step by step:
example = getLine >>= \x ->
          let y = map toUpper x in
          putStrLn y >>
          return (length y)
```

---

### The >> Operator

`>>` is bind that ignores the result:

```haskell
(>>) :: Monad m => m a -> m b -> m b
ma >> mb = ma >>= \_ -> mb

-- Example:
putStrLn "Hello" >> putStrLn "World"
-- Prints "Hello" then "World", ignoring ()
```

---

## Part 5: Lazy Evaluation

### What is Lazy Evaluation?

Haskell only evaluates expressions **when needed**. This enables:
- Infinite data structures
- Efficient computation (only compute what you use)
- Modular code (separate generation from selection)

---

### Infinite Lists

```haskell
-- Infinite list of ones
ones :: [Int]
ones = 1 : ones

-- Infinite list of natural numbers
nats :: [Int]
nats = [0..]  -- Same as: 0 : 1 : 2 : 3 : ...

-- Infinite list of squares
squares :: [Int]
squares = map (^2) [0..]  -- [0, 1, 4, 9, 16, ...]
```

**This doesn't loop forever** because Haskell only computes what you ask for:
```haskell
take 5 ones     -- [1,1,1,1,1]
take 5 nats     -- [0,1,2,3,4]
take 5 squares  -- [0,1,4,9,16]
```

---

### Generating Infinite Lists

```haskell
-- repeat: infinite copies
repeat :: a -> [a]
repeat x = x : repeat x

take 3 (repeat 'a')  -- "aaa"

-- iterate: apply function repeatedly
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

take 5 (iterate (*2) 1)  -- [1,2,4,8,16]

-- cycle: repeat a list forever
cycle :: [a] -> [a]
cycle xs = xs ++ cycle xs

take 7 (cycle [1,2,3])  -- [1,2,3,1,2,3,1]
```

---

### Fibonacci Sequence

Classic example of lazy infinite lists:

```haskell
-- Elegant but inefficient
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Efficient using infinite list (self-referential!)
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- How it works:
-- fibs       = 0 : 1 : 1 : 2 : 3 : 5 : ...
-- tail fibs  = 1 : 1 : 2 : 3 : 5 : 8 : ...
-- zipWith(+) = 1 : 2 : 3 : 5 : 8 : 13 : ...

take 10 fibs  -- [0,1,1,2,3,5,8,13,21,34]
fibs !! 30    -- 832040 (instant!)
```

---

### Sieve of Eratosthenes

Prime numbers using infinite lists:

```haskell
primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

take 10 primes  -- [2,3,5,7,11,13,17,19,23,29]
primes !! 100   -- 547 (101st prime)
```

---

### Take and Drop Patterns

Common patterns with infinite lists:

```haskell
-- Take first n elements
take 5 [1..]  -- [1,2,3,4,5]

-- Drop first n elements
drop 5 [1..]  -- Infinite list starting at 6

-- Take while condition holds
takeWhile (< 10) [1..]  -- [1,2,3,4,5,6,7,8,9]

-- Drop while condition holds
dropWhile (< 10) [1..]  -- Infinite list starting at 10

-- Combine: get elements 5-9
take 5 (drop 5 [1..])  -- [6,7,8,9,10]
```

---

### Scala Comparison

```scala
// Scala LazyList (formerly Stream)
val nats = LazyList.from(0)
nats.take(5).toList  // List(0, 1, 2, 3, 4)

val fibs: LazyList[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map { case (a, b) => a + b }
fibs.take(10).toList  // List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
```

**Key difference:** In Scala you explicitly choose `LazyList`. In Haskell, **everything is lazy by default**.

---

## Part 6: Combining Monads and Laziness

### Monadic Operations on Infinite Lists

```haskell
-- Filter with Maybe
import Data.Maybe (catMaybes)

safeDiv' :: Int -> Int -> Maybe Int
safeDiv' _ 0 = Nothing
safeDiv' x y = Just (x `div` y)

-- Try dividing 100 by each number, keep successes
take 10 $ catMaybes [safeDiv' 100 x | x <- [0..]]
-- [100,50,33,25,20,16,14,12,11,10]
```

---

### mapM and sequence

```haskell
-- sequence: turn list of monads into monad of list
sequence :: Monad m => [m a] -> m [a]

sequence [Just 1, Just 2, Just 3]  -- Just [1,2,3]
sequence [Just 1, Nothing, Just 3]  -- Nothing

-- mapM: map then sequence
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f xs = sequence (map f xs)

-- Example: safe division of list
mapM (safeDiv 10) [2,5,1]  -- Just [5,2,10]
mapM (safeDiv 10) [2,0,1]  -- Nothing
```

---

## Key Takeaways

### Monads
1. **Monad** = Functor + ability to chain operations that return wrapped values
2. **>>=** (bind) chains operations, handling unwrapping
3. **Do-notation** is syntactic sugar for `>>=` chains
4. **Maybe monad**: automatic error propagation
5. **List monad**: nondeterminism, all combinations

### Lazy Evaluation
1. **Lazy** = only evaluate when needed
2. **Infinite lists** are possible and useful
3. **take/drop/takeWhile** work with infinite lists
4. **Self-referential definitions** (like `fibs`) are powerful

### Patterns
- `return` wraps a value in a monad
- `>>` sequences, ignoring the first result
- `guard` filters in the list monad
- Use `catMaybes` to filter out `Nothing` values

---

## Common Pitfalls

**Confusing `>>=` and `fmap`:**
```haskell
fmap :: (a -> b)   -> f a -> f b    -- Function returns plain value
(>>=) :: m a -> (a -> m b) -> m b   -- Function returns wrapped value
```

**Forgetting `return` in do-blocks:**
```haskell
-- Wrong: last line is Int, but need Maybe Int
bad = do
  x <- Just 5
  x + 1

-- Right: wrap the result
good = do
  x <- Just 5
  return (x + 1)
```

**Forcing infinite lists:**
```haskell
length [1..]  -- Runs forever!
sum [1..]     -- Runs forever!
-- Use take/takeWhile first
```

**Not understanding guard:**
```haskell
-- guard False produces empty list, terminating that branch
-- guard True produces [()], allowing continuation
```

---

## Next Steps

1. Work through `exercises.hs`
2. Test your solutions: `:load tests.hs` then `main`
3. Practice both Maybe chains and List comprehensions
4. Experiment with infinite lists in GHCi
