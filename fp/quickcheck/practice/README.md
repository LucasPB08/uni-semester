# QuickCheck: Property-Based Testing in Haskell

## Goals
- Understand property-based testing vs example-based testing
- Write QuickCheck properties to test function behavior
- Use generators and understand shrinking
- Find bugs by thinking about properties, not just examples

## Context

You've been writing tests with specific examples:
```haskell
test "reverse [1,2,3]" [3,2,1] (reverse [1,2,3])
```

But what if there's a bug that only shows up for certain inputs? **Property-based testing** generates hundreds of random inputs to find edge cases you didn't think of.

---

## Example-Based vs Property-Based Testing

### Example-Based (what you've been doing)
```haskell
-- Test specific cases
testReverse = do
  assert (reverse [1,2,3] == [3,2,1])
  assert (reverse [] == [])
  assert (reverse [1] == [1])
```

**Problem:** You only test cases you think of. Bugs hide in cases you don't think of.

### Property-Based (QuickCheck)
```haskell
-- Test a property that should hold for ALL inputs
prop_reverseReverse :: [Int] -> Bool
prop_reverseReverse xs = reverse (reverse xs) == xs
```

QuickCheck generates hundreds of random lists and checks if the property holds for all of them.

---

## Getting Started with QuickCheck

### Import
```haskell
import Test.QuickCheck
```

### Running Tests in GHCi
```haskell
ghci> import Test.QuickCheck
ghci> quickCheck prop_reverseReverse
+++ OK, passed 100 tests.
```

### A Failing Property
```haskell
-- This property is FALSE
prop_reverseIdentity :: [Int] -> Bool
prop_reverseIdentity xs = reverse xs == xs

ghci> quickCheck prop_reverseIdentity
*** Failed! Falsifiable (after 4 tests and 3 shrinks):
[0,1]
```

QuickCheck found a counterexample: `[0,1]` is not equal to its reverse `[1,0]`.

---

## Writing Properties

### Basic Property Structure
```haskell
-- Properties are functions that return Bool (or Property)
-- Convention: prefix with prop_
prop_name :: InputTypes -> Bool
prop_name inputs = someConditionThatShouldBeTrue
```

### Common Property Patterns

**1. Idempotence** - Applying twice equals applying once
```haskell
prop_sortIdempotent :: [Int] -> Bool
prop_sortIdempotent xs = sort (sort xs) == sort xs
```

**2. Inverse** - Two operations cancel out
```haskell
prop_reverseInverse :: [Int] -> Bool
prop_reverseInverse xs = reverse (reverse xs) == xs
```

**3. Preservation** - Some property is maintained
```haskell
prop_sortPreservesLength :: [Int] -> Bool
prop_sortPreservesLength xs = length (sort xs) == length xs

prop_sortPreservesElements :: [Int] -> Bool
prop_sortPreservesElements xs = sort (sort xs) == sort xs
```

**4. Relationship to other functions**
```haskell
prop_mapLength :: [Int] -> Bool
prop_mapLength xs = length (map (+1) xs) == length xs
```

**5. Model-based** - Compare to a known-correct implementation
```haskell
prop_myReverse :: [Int] -> Bool
prop_myReverse xs = myReverse xs == reverse xs
```

---

## Property Combinators

### Conditional Properties (==>)

Sometimes a property only makes sense for certain inputs:

```haskell
-- Only test non-empty lists
prop_headReverse :: [Int] -> Property
prop_headReverse xs = not (null xs) ==> head (reverse xs) == last xs

-- Only test positive numbers
prop_sqrtPositive :: Double -> Property
prop_sqrtPositive x = x >= 0 ==> sqrt x * sqrt x ~= x
  where a ~= b = abs (a - b) < 0.0001
```

**Note:** Using `==>` changes return type from `Bool` to `Property`.

### Labeling Tests

```haskell
prop_reverseLength :: [Int] -> Property
prop_reverseLength xs =
  label (classifyLength xs) $ length (reverse xs) == length xs
  where
    classifyLength ys
      | null ys = "empty"
      | length ys < 5 = "small"
      | otherwise = "large"
```

### Collecting Statistics

```haskell
prop_insert :: Int -> [Int] -> Property
prop_insert x xs =
  collect (length xs) $ x `elem` insert x xs
```

---

## Using Arbitrary (Generators)

QuickCheck uses the `Arbitrary` typeclass to generate random values.

### Built-in Generators
```haskell
-- These types have Arbitrary instances:
-- Int, Integer, Float, Double, Bool, Char, String
-- [a] (if a has Arbitrary), Maybe a, Either a b, tuples, etc.
```

### Controlling Generation

```haskell
-- Generate in a specific range
prop_bounded :: Property
prop_bounded = forAll (choose (1, 100)) $ \n ->
  n >= 1 && n <= 100

-- Generate from a list
prop_element :: Property
prop_element = forAll (elements ['a'..'z']) $ \c ->
  c >= 'a' && c <= 'z'

-- Generate positive numbers only
prop_positive :: Property
prop_positive = forAll (arbitrary `suchThat` (> 0)) $ \n ->
  (n :: Int) > 0
```

### Custom Generators for Your Types

```haskell
data Color = Red | Green | Blue deriving (Show, Eq)

instance Arbitrary Color where
  arbitrary = elements [Red, Green, Blue]

data Person = Person String Int deriving (Show, Eq)

instance Arbitrary Person where
  arbitrary = do
    name <- listOf1 (elements ['a'..'z'])  -- Non-empty string of lowercase
    age <- choose (0, 120)
    return (Person name age)
```

---

## Shrinking

When QuickCheck finds a failing case, it tries to **shrink** it to a minimal example.

```haskell
-- If this fails for [1,2,3,4,5,6,7,8,9,10]
-- QuickCheck will shrink to find the smallest failing case
prop_bad :: [Int] -> Bool
prop_bad xs = length xs < 5

ghci> quickCheck prop_bad
*** Failed! Falsifiable (after 6 tests and 1 shrink):
[0,0,0,0,0]
```

The shrunk result `[0,0,0,0,0]` is simpler than whatever random list first failed.

### Custom Shrinking

```haskell
instance Arbitrary Person where
  arbitrary = ...
  shrink (Person name age) =
    [Person name' age | name' <- shrink name] ++
    [Person name age' | age' <- shrink age]
```

---

## Practical Examples

### Testing a Sort Function
```haskell
-- Property 1: Output is sorted
prop_sortOrdered :: [Int] -> Bool
prop_sortOrdered xs = isSorted (sort xs)
  where isSorted ys = and $ zipWith (<=) ys (drop 1 ys)

-- Property 2: Same length
prop_sortLength :: [Int] -> Bool
prop_sortLength xs = length (sort xs) == length xs

-- Property 3: Same elements
prop_sortElements :: [Int] -> Bool
prop_sortElements xs = sort (sort xs) == sort xs

-- Property 4: All original elements present
prop_sortPermutation :: [Int] -> Bool
prop_sortPermutation xs = sort xs `isPermutationOf` xs
  where isPermutationOf as bs = sort as == sort bs
```

### Testing Encode/Decode
```haskell
-- A function and its inverse should cancel out
prop_encodeDecode :: String -> Bool
prop_encodeDecode s = decode (encode s) == s

prop_decodeEncode :: String -> Property
prop_decodeEncode s = isValidEncoded s ==> encode (decode s) == s
```

### Testing Arithmetic
```haskell
prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = x + y == y + x

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z = (x + y) + z == x + (y + z)

prop_multDistributive :: Int -> Int -> Int -> Bool
prop_multDistributive x y z = x * (y + z) == x * y + x * z
```

---

## Scala Comparison (ScalaCheck)

| Haskell (QuickCheck) | Scala (ScalaCheck) |
|----------------------|-------------------|
| `prop_name :: A -> Bool` | `forAll { (a: A) => ... }` |
| `quickCheck prop_name` | `prop_name.check()` |
| `==>` (implication) | `==>` |
| `forAll gen $ \x -> ...` | `forAll(gen) { x => ... }` |
| `arbitrary` | `arbitrary[A]` |
| `choose (lo, hi)` | `Gen.choose(lo, hi)` |
| `elements xs` | `Gen.oneOf(xs)` |
| `listOf gen` | `Gen.listOf(gen)` |

**Scala Example:**
```scala
import org.scalacheck.Prop.forAll

val propReverseReverse = forAll { (xs: List[Int]) =>
  xs.reverse.reverse == xs
}

propReverseReverse.check()
```

---

## Running QuickCheck

### In GHCi
```haskell
ghci> :load MyTests.hs
ghci> quickCheck prop_myProperty
+++ OK, passed 100 tests.
```

### More Tests
```haskell
ghci> quickCheckWith stdArgs { maxSuccess = 1000 } prop_myProperty
+++ OK, passed 1000 tests.
```

### Verbose Output
```haskell
ghci> verboseCheck prop_myProperty
-- Shows every generated test case
```

### Running All Properties in a Module
```haskell
-- In your test file
import Test.QuickCheck

return []  -- Template Haskell splice
runTests = $quickCheckAll

-- Then in GHCi:
ghci> runTests
```

---

## Common Mistakes

❌ **Testing implementation, not specification**
```haskell
-- BAD: This just duplicates the implementation
prop_bad xs = mySort xs == mySort xs
```

❌ **Properties that are always true**
```haskell
-- BAD: This tells us nothing
prop_useless xs = length xs >= 0
```

❌ **Forgetting edge cases in conditions**
```haskell
-- BAD: Discards too many tests
prop_tooStrict xs = length xs > 100 ==> ...
```

✅ **Good properties test relationships and invariants**
```haskell
-- GOOD: Tests a meaningful relationship
prop_good xs = reverse (reverse xs) == xs
```

---

## Key Takeaways

1. **Property-based testing** finds bugs you didn't think of
2. **Properties** describe what should be true for ALL inputs
3. **Common patterns**: idempotence, inverse, preservation, model-based
4. **==>** for conditional properties
5. **Shrinking** gives you minimal failing examples
6. **Arbitrary** generates random test data
7. Think about **what** your function should do, not **how**

---

## Exercise Instructions

The `exercises.hs` file contains functions that **may have bugs**. Your task:

1. Write QuickCheck properties in `tests.hs` to test each function
2. Run your properties - if they fail, you found a bug!
3. Some functions are correct, some have subtle bugs
4. Good properties will catch the bugs; weak properties won't

**Goal:** Practice thinking about function specifications, not just examples.
