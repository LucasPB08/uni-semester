# Week 3: Functors and IO

## Goals
- Understand the Functor typeclass and `fmap`
- Write Functor instances for custom types
- Learn basic IO operations in Haskell
- Understand the difference between pure and impure code
- Use do-notation for IO actions

## Context

You've already used type classes like `Eq`, `Ord`, and `Show`. Now we introduce **Functor** - a typeclass for things you can "map over". You also wrote `treeMap` - that's exactly what Functor captures!

We'll also cover **IO** - how Haskell handles side effects (printing, reading input, files) while remaining a pure functional language.

---

## Part 1: Functors

### What is a Functor?

A **Functor** is any type that can be "mapped over" - you can apply a function to the value(s) inside without changing the structure.

**The Functor typeclass:**
```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

**Reading this:**
- `f` is a type constructor (like `[]`, `Maybe`, `Tree`)
- `fmap` takes a function `a -> b` and a `f a`, returns `f b`
- It applies the function "inside" the container

---

### Lists are Functors

```haskell
-- For lists, fmap is just map!
instance Functor [] where
  fmap = map

-- Examples:
fmap (*2) [1,2,3]      -- [2,4,6]
fmap show [1,2,3]      -- ["1","2","3"]
fmap length ["hi","hello"]  -- [2,5]
```

**Scala equivalent:**
```scala
List(1,2,3).map(_ * 2)  // List(2,4,6)
```

---

### Maybe is a Functor

```haskell
instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just x) = Just (f x)

-- Examples:
fmap (*2) (Just 5)   -- Just 10
fmap (*2) Nothing    -- Nothing
fmap show (Just 42)  -- Just "42"
```

**Key insight:** If there's no value (`Nothing`), the function isn't applied. The structure is preserved.

**Scala equivalent:**
```scala
Some(5).map(_ * 2)   // Some(10)
None.map(_ * 2)      // None
```

---

### Your Tree is a Functor!

Remember your `treeMap` function?

```haskell
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Empty = Empty
treeMap f (Node x left right) = Node (f x) (treeMap f left) (treeMap f right)
```

That's exactly `fmap` for `Tree`! Here's how you'd make it an instance:

```haskell
instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
```

Now you can use `fmap` instead of `treeMap`:
```haskell
fmap (*2) (Node 5 (Node 3 Empty Empty) Empty)
-- Node 10 (Node 6 Empty Empty) Empty
```

---

### The Infix Operator: <$>

`<$>` is infix `fmap`:

```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

-- These are equivalent:
fmap (*2) [1,2,3]
(*2) <$> [1,2,3]

fmap show (Just 5)
show <$> Just 5
```

**Why use `<$>`?** It reads more naturally left-to-right: "apply this function to that container."

---

### Functor Laws

Every Functor instance must satisfy two laws:

**1. Identity:** Mapping `id` changes nothing
```haskell
fmap id x == x

-- Example:
fmap id [1,2,3] == [1,2,3]  -- True
fmap id (Just 5) == Just 5  -- True
```

**2. Composition:** Mapping f then g equals mapping (g . f)
```haskell
fmap g (fmap f x) == fmap (g . f) x

-- Example:
fmap (*2) (fmap (+1) [1,2,3]) == fmap ((*2) . (+1)) [1,2,3]
-- [4,6,8] == [4,6,8]  -- True
```

These laws ensure `fmap` behaves predictably.

---

### Writing Functor Instances

**Pattern:** Apply the function to all values, preserve the structure.

```haskell
-- For a type: data Box a = Box a
instance Functor Box where
  fmap f (Box x) = Box (f x)

-- For a type: data Pair a = Pair a a
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

-- For a type: data Either e a = Left e | Right a
-- Only map over the Right value (Left holds errors)
instance Functor (Either e) where
  fmap _ (Left e)  = Left e
  fmap f (Right x) = Right (f x)
```

---

## Part 2: IO

### The Problem: Side Effects

Haskell is **pure** - functions can't have side effects. But programs need to:
- Print to the console
- Read user input
- Read/write files
- Make network requests

**Solution:** The `IO` type marks actions that perform side effects.

---

### IO Actions

An `IO a` is an **action** that, when executed, performs some side effect and produces a value of type `a`.

```haskell
putStrLn :: String -> IO ()      -- Print string, return nothing
getLine :: IO String             -- Read line, return the string
print :: Show a => a -> IO ()    -- Print any showable value
readFile :: FilePath -> IO String  -- Read file contents
```

**Key insight:** `IO String` is not a `String`. It's an action that *produces* a String when run.

---

### Basic IO Examples

```haskell
-- Print hello
main :: IO ()
main = putStrLn "Hello, World!"

-- Print a number
main = print 42

-- Print multiple lines (using >>)
main = putStrLn "Line 1" >> putStrLn "Line 2"
```

The `>>` operator sequences IO actions: do first action, ignore result, do second action.

---

### Do-Notation for IO

Do-notation makes sequencing IO actions readable:

```haskell
-- Without do-notation (ugly):
main = putStrLn "What's your name?" >>
       getLine >>= \name ->
       putStrLn ("Hello, " ++ name ++ "!")

-- With do-notation (clean):
main = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")
```

**Reading do-notation:**
- Each line is an IO action
- `<-` extracts the result from an IO action
- The last line is the return value of the whole block

---

### Binding with <-

The `<-` operator extracts values from IO:

```haskell
main = do
  line <- getLine        -- Run getLine, bind result to 'line'
  let upper = map toUpper line  -- Pure computation (use 'let', no <-)
  putStrLn upper         -- Print the result
```

**Important:**
- `<-` is for IO actions (extracting from `IO a`)
- `let` is for pure computations (no IO involved)

---

### Return: Wrapping Pure Values in IO

`return` wraps a pure value in IO:

```haskell
return :: a -> IO a

-- Example:
main = do
  let x = 5 + 3      -- Pure computation
  return x           -- Wrap in IO, return from main

-- Or:
greet :: String -> IO String
greet name = return ("Hello, " ++ name)
```

**Note:** `return` doesn't exit the function like in imperative languages. It just wraps a value in IO.

---

### Pure vs Impure

**Pure functions:**
- Same input → same output, always
- No side effects
- Examples: `map`, `filter`, `(+)`, your `treeMap`

**Impure (IO) actions:**
- May produce different results each time
- Have side effects
- Examples: `getLine`, `putStrLn`, `readFile`

```haskell
-- Pure: always returns the same result
double :: Int -> Int
double x = x * 2

-- Impure: result depends on user input
askNumber :: IO Int
askNumber = do
  putStrLn "Enter a number:"
  line <- getLine
  return (read line)
```

---

### Combining Pure and Impure

Keep as much code pure as possible. Use IO only at the edges:

```haskell
-- Pure logic
processData :: String -> String
processData = map toUpper . filter (/= ' ')

-- IO wrapper
main :: IO ()
main = do
  putStrLn "Enter text:"
  input <- getLine
  let result = processData input  -- Pure function!
  putStrLn result
```

**Pattern:** Read input (IO) → Process (pure) → Output (IO)

---

### Useful IO Functions

```haskell
-- Output
putStrLn :: String -> IO ()          -- Print line
putStr :: String -> IO ()            -- Print without newline
print :: Show a => a -> IO ()        -- Print any showable value

-- Input
getLine :: IO String                 -- Read one line
getChar :: IO Char                   -- Read one character
getContents :: IO String             -- Read all stdin

-- Sequencing
(>>) :: IO a -> IO b -> IO b         -- Do first, then second (ignore first result)
(>>=) :: IO a -> (a -> IO b) -> IO b -- Do first, pass result to second

-- Pure to IO
return :: a -> IO a                  -- Wrap pure value in IO
```

---

### Scala Comparison

```scala
// Scala (side effects happen anywhere)
def greet(): Unit = {
  println("What's your name?")
  val name = scala.io.StdIn.readLine()
  println(s"Hello, $name!")
}

// Haskell (side effects are explicit in types)
greet :: IO ()
greet = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")
```

In Haskell, you can tell from the type `IO ()` that this function has side effects. In Scala, you can't tell from `Unit` alone.

---

## Key Takeaways

### Functor
1. **Functor** = types you can map over
2. **fmap** applies a function inside a container
3. **<$>** is infix fmap: `f <$> container`
4. Lists, Maybe, Tree, Either are all Functors
5. Your `treeMap` is `fmap` for Tree!

### IO
1. **IO a** = an action that produces an `a`
2. **do-notation** sequences IO actions
3. **<-** extracts values from IO actions
4. **let** is for pure computations in do-blocks
5. **return** wraps pure values in IO
6. Keep logic **pure**, use IO only at edges

---

## Common Pitfalls

❌ **Confusing `fmap` types**: `fmap :: (a -> b) -> f a -> f b`, not `f a -> (a -> b) -> f b`

❌ **Using `<-` for pure values**: Use `let` for pure, `<-` for IO

❌ **Forgetting `return`**: The last expression in a do-block must be `IO a`

❌ **Thinking `return` exits**: It just wraps a value, doesn't return from function

✅ **Do use `fmap`/`<$>` instead of pattern matching** when you just want to transform values

✅ **Do keep functions pure** and use IO only where necessary

✅ **Do use `let` in do-blocks** for intermediate pure computations

---

## Next Steps

1. Work through `exercises_functors_io.hs`
2. Test in GHCi (note: IO exercises need to be run, not just evaluated)
3. Run tests: `:load tests_functors_io.hs` then `runAllTests`
