# Week 3: Applicatives

## Goals
- Understand the Applicative typeclass and why it exists
- Use `pure` and `<*>` to work with values in contexts
- Apply multi-argument functions to Functor values
- See how Applicative sits between Functor and Monad

## Prerequisites
- Functor typeclass and `fmap`/`<$>`
- Custom data types and type classes

## Context

You've learned Functor: you can map a function over a container with `fmap`. But what if you have a **function inside a container** and want to apply it to a **value inside another container**?

```haskell
-- You know how to do this:
fmap (+1) (Just 5)  -- Just 6

-- But what about this?
Just (+1) ??? Just 5  -- How do we apply Just (+1) to Just 5?
```

**Applicative** solves exactly this problem.

---

## The Problem Functor Can't Solve

With Functor, you can apply a function to a wrapped value:

```haskell
fmap (*2) (Just 5)      -- Just 10
fmap (+) (Just 5)       -- Just (+5)  ... but now what?
```

Wait - `fmap (+) (Just 5)` gives us `Just (+5)` - a **function inside a Maybe**! How do we apply it to another Maybe?

```haskell
-- We have:
Just (+5) :: Maybe (Int -> Int)
Just 3    :: Maybe Int

-- We want:
Just 8    :: Maybe Int
```

`fmap` can't help here - it expects a **plain function**, not a wrapped one.

---

## Applicative Typeclass

```haskell
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

**Reading this:**
- `Functor f =>` means every Applicative must also be a Functor
- `pure` wraps a value in the minimal context
- `<*>` (pronounced "apply" or "ap") applies a wrapped function to a wrapped value

---

## The Two Key Operations

### pure: Lifting Values

`pure` puts a value into the "default" context:

```haskell
pure 5 :: Maybe Int      -- Just 5
pure 5 :: [Int]          -- [5]
pure 5 :: Either e Int   -- Right 5
pure 5 :: IO Int         -- IO action that returns 5
```

**Scala equivalent:**
```scala
Some(5)        // Option
List(5)        // List
Right(5)       // Either
IO.pure(5)     // Cats Effect IO
```

### <*>: Applying Wrapped Functions

`<*>` applies a function inside a context to a value inside a context:

```haskell
Just (+3) <*> Just 5     -- Just 8
Just (+3) <*> Nothing    -- Nothing
Nothing   <*> Just 5     -- Nothing

[(+1), (*2)] <*> [10, 20]  -- [11, 21, 20, 40]
```

**How list `<*>` works:** Apply each function to each value (all combinations).

---

## Why Applicative Matters: Multi-Argument Functions

The real power: applying **multi-argument functions** to multiple wrapped values.

```haskell
-- Regular function application:
(+) 3 5  -- 8

-- With Maybe values, use <$> and <*>:
(+) <$> Just 3 <*> Just 5  -- Just 8

-- Step by step:
-- (+) <$> Just 3        = Just (+3)      -- fmap gives us wrapped function
-- Just (+3) <*> Just 5  = Just 8         -- <*> applies it
```

**The pattern:**
```haskell
f <$> x <*> y <*> z
-- is equivalent to
pure f <*> x <*> y <*> z
```

This lets you write:
```haskell
-- 3-argument function with Maybe values:
(\x y z -> x + y + z) <$> Just 1 <*> Just 2 <*> Just 3  -- Just 6

-- If any is Nothing, result is Nothing:
(\x y z -> x + y + z) <$> Just 1 <*> Nothing <*> Just 3  -- Nothing
```

---

## Maybe Applicative

```haskell
instance Applicative Maybe where
  pure = Just

  Nothing <*> _       = Nothing
  _       <*> Nothing = Nothing
  Just f  <*> Just x  = Just (f x)
```

**Behavior:** If either side is Nothing, result is Nothing. Both must be Just.

```haskell
-- Examples:
pure (+) <*> Just 3 <*> Just 5     -- Just 8
(+) <$> Just 3 <*> Just 5          -- Just 8 (same thing)
(+) <$> Nothing <*> Just 5         -- Nothing
(+) <$> Just 3 <*> Nothing         -- Nothing

-- Practical: combining optional values
data Person = Person String Int deriving Show

makePerson :: Maybe String -> Maybe Int -> Maybe Person
makePerson mName mAge = Person <$> mName <*> mAge

makePerson (Just "Alice") (Just 30)  -- Just (Person "Alice" 30)
makePerson Nothing (Just 30)         -- Nothing
```

**Scala equivalent:**
```scala
// Scala with Cats
(Option("Alice"), Option(30)).mapN(Person.apply)

// Or manually:
for {
  name <- Some("Alice")
  age <- Some(30)
} yield Person(name, age)
```

---

## List Applicative

```haskell
instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]  -- All combinations!
```

**Behavior:** Every function applied to every value (Cartesian product).

```haskell
-- Examples:
pure (*2) <*> [1,2,3]           -- [2,4,6]
[(+1), (*2)] <*> [10, 20]       -- [11, 21, 20, 40]
                                 -- (+1) 10, (+1) 20, (*2) 10, (*2) 20

-- Multi-argument:
(+) <$> [1,2] <*> [10,20]       -- [11,21,12,22]
                                 -- 1+10, 1+20, 2+10, 2+20

-- Generating combinations:
(,) <$> [1,2] <*> ["a","b"]     -- [(1,"a"),(1,"b"),(2,"a"),(2,"b")]

-- All pairs from two lists:
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = (,) <$> xs <*> ys
```

**Scala equivalent:**
```scala
// For comprehension gives same result
for {
  f <- List((x: Int) => x + 1, (x: Int) => x * 2)
  x <- List(10, 20)
} yield f(x)  // List(11, 21, 20, 40)
```

---

## Either Applicative

```haskell
instance Applicative (Either e) where
  pure = Right

  Left e  <*> _       = Left e
  _       <*> Left e  = Left e
  Right f <*> Right x = Right (f x)
```

**Behavior:** First Left "wins" - computation short-circuits on error.

```haskell
(+) <$> Right 3 <*> Right 5           -- Right 8
(+) <$> Left "error" <*> Right 5      -- Left "error"
(+) <$> Right 3 <*> Left "oops"       -- Left "oops"
(+) <$> Left "first" <*> Left "second" -- Left "first" (first error)
```

---

## The Applicative Laws

Every Applicative instance must satisfy these laws:

### 1. Identity
```haskell
pure id <*> v == v

-- Example:
pure id <*> Just 5  ==  Just 5  -- True
```

### 2. Homomorphism
Applying a pure function to a pure value equals pure of the application:
```haskell
pure f <*> pure x == pure (f x)

-- Example:
pure (+1) <*> pure 5  ==  pure ((+1) 5)
Just (+1) <*> Just 5  ==  Just 6  -- True
```

### 3. Interchange
```haskell
u <*> pure y == pure ($ y) <*> u

-- Example:
Just (+3) <*> pure 5  ==  pure ($ 5) <*> Just (+3)
-- Just 8             ==  Just 8
```

### 4. Composition
```haskell
pure (.) <*> u <*> v <*> w == u <*> (v <*> w)
```

---

## Applicative vs Functor vs Monad

| Typeclass | What it does | Operator |
|-----------|--------------|----------|
| **Functor** | Map function over wrapped value | `fmap`, `<$>` |
| **Applicative** | Apply wrapped function to wrapped value | `pure`, `<*>` |
| **Monad** | Chain computations where next depends on previous | `return`, `>>=` |

**The hierarchy:**
```
Functor  ⊂  Applicative  ⊂  Monad
```

Every Monad is an Applicative, every Applicative is a Functor.

**When to use which:**
- **Functor**: Transform values inside a context
- **Applicative**: Combine independent computations
- **Monad**: Chain dependent computations (next step depends on previous result)

---

## Useful Applicative Functions

```haskell
-- Lift a binary function (from Control.Applicative)
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y

-- Examples:
liftA2 (+) (Just 3) (Just 5)      -- Just 8
liftA2 (,) [1,2] ["a","b"]        -- [(1,"a"),(1,"b"),(2,"a"),(2,"b")]

-- Sequence actions, keep left result
(<*) :: Applicative f => f a -> f b -> f a

-- Sequence actions, keep right result
(*>) :: Applicative f => f a -> f b -> f b

-- Examples:
Just 3 <* Just 5   -- Just 3
Just 3 *> Just 5   -- Just 5
Nothing <* Just 5  -- Nothing
```

---

## Practical Examples

### Validating Multiple Fields

```haskell
data User = User String Int String deriving Show

-- Each validation might fail
validateName :: String -> Maybe String
validateName s = if null s then Nothing else Just s

validateAge :: Int -> Maybe Int
validateAge n = if n < 0 || n > 150 then Nothing else Just n

validateEmail :: String -> Maybe String
validateEmail s = if '@' `elem` s then Just s else Nothing

-- Combine all validations with Applicative:
makeUser :: String -> Int -> String -> Maybe User
makeUser name age email =
  User <$> validateName name
       <*> validateAge age
       <*> validateEmail email

makeUser "Alice" 30 "alice@example.com"  -- Just (User "Alice" 30 "alice@example.com")
makeUser "" 30 "alice@example.com"        -- Nothing (name validation failed)
makeUser "Alice" -5 "alice@example.com"   -- Nothing (age validation failed)
```

### Parsing Multiple Values

```haskell
-- Imagine we have:
parseName :: String -> Maybe String
parseAge :: String -> Maybe Int

-- Parse a line like "Alice,30"
parsePerson :: String -> Maybe Person
parsePerson line = case splitOn ',' line of
  [nameStr, ageStr] -> Person <$> parseName nameStr <*> parseAge ageStr
  _                 -> Nothing
```

### Combining Lists

```haskell
-- Generate all coordinates in a grid
coords :: Int -> Int -> [(Int, Int)]
coords rows cols = (,) <$> [0..rows-1] <*> [0..cols-1]

coords 2 3  -- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
```

---

## Scala Comparison Summary

| Haskell | Scala (standard) | Scala (Cats) |
|---------|------------------|--------------|
| `pure x` | `Some(x)`, `List(x)` | `x.pure[F]` |
| `f <$> x` | `x.map(f)` | `x.map(f)` |
| `f <*> x` | - | `(f, x).mapN(...)` |
| `f <$> x <*> y` | `for { a <- x; b <- y } yield f(a,b)` | `(x, y).mapN(f)` |
| `liftA2 f x y` | - | `(x, y).mapN(f)` |

In Scala, `mapN` from Cats is the closest equivalent to Applicative style.

---

## Key Takeaways

1. **Applicative** lets you apply wrapped functions to wrapped values
2. **pure** lifts a value into a context
3. **<*>** applies `f a -> f b` given `f (a -> b)`
4. **Pattern**: `f <$> x <*> y <*> z` for multi-argument functions
5. **Use case**: Combining independent computations that might fail
6. **Maybe Applicative**: Both must be Just, otherwise Nothing
7. **List Applicative**: All combinations (Cartesian product)

---

## Common Pitfalls

❌ **Confusing `<$>` and `<*>`**: `<$>` needs a plain function, `<*>` needs a wrapped function

❌ **Forgetting the first `<$>`**: The pattern is `f <$> x <*> y`, not `f <*> x <*> y`

❌ **Using Applicative when you need Monad**: If the second computation depends on the first result, you need Monad

✅ **Do use `<$>` and `<*>` for combining Maybe values** instead of nested case expressions

✅ **Do use `liftA2` for cleaner binary function lifting**

---

## Next Steps

1. Complete `exercises_applicatives.hs`
2. Test with `:load tests_applicatives.hs` then `runAllTests`
3. After mastering Applicative, you're ready for Monads (Week 4)
