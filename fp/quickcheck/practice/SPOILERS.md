# SPOILERS: Bug Summary

**DO NOT READ THIS FILE until you've attempted the exercises!**

The goal is to write QuickCheck properties that find the bugs. Only check this file to verify your findings.

---

## Functions with Bugs

### 1. myProduct
**Bug:** Returns 0 for empty list instead of 1
```haskell
myProduct [] = 0  -- WRONG! Should be 1 (identity for multiplication)
```
**Property that catches it:** `prop_myProductEmpty = myProduct [] == 1`

### 2. myFilter
**Bug:** Negated predicate - keeps elements that DON'T satisfy p
```haskell
| not (p x) = x : myFilter p xs  -- WRONG! Should be: | p x = ...
```
**Property that catches it:** `prop_myFilterSatisfies xs = all (> 0) (myFilter (> 0) xs)`

### 3. safeTail
**Bug:** Returns `Just []` for empty list instead of `Nothing`
```haskell
safeTail [] = Just []  -- WRONG! Should be Nothing
```
**Property that catches it:** `prop_safeTailEmpty = safeTail ([] :: [Int]) == Nothing`

### 4. rleDecode
**Bug:** Limits count to 9 for no good reason, corrupting longer runs
```haskell
| n > 9 = replicate 9 x ++ rleDecode ((n - 9, x) : rest)  -- WRONG!
```
**Property that catches it:** `prop_rleInverse s = rleDecode (rleEncode s) == s`
(Fails for strings with runs > 9, e.g., "aaaaaaaaaa")

### 5. myAbs
**Bug:** Overflow for `minBound :: Int`
```haskell
myAbs minBound  -- Returns minBound (negative!) because -minBound overflows
```
**Property that catches it:** `prop_myAbsNonNegative x = myAbs x >= 0`
(QuickCheck will eventually try minBound and find the bug)

---

## Correct Functions

The following functions are implemented correctly:
- myReverse
- myLast (crashes on empty list, but that's documented as precondition)
- myTake
- myDrop
- myElem
- myNub
- mySum
- myLength
- myConcat
- myZip
- myMap
- myFoldr
- myFoldl
- safeHead
- safeDiv
- safeIndex
- myMax
- myMin
- myClamp
- caesarEncode / caesarDecode
- rleEncode (encoding is correct, decoding is buggy)
- insertionSort
- binarySearch
- median
- fibs
- factorial
- myGcd
- isPrime
- myReplicate
- myIntersperse

---

## Difficulty of Finding Each Bug

| Bug | Difficulty | Why |
|-----|------------|-----|
| myProduct empty | Easy | Direct test: `myProduct [] == 1` |
| myFilter | Easy | Any filter test will fail immediately |
| safeTail empty | Easy | Direct test: `safeTail [] == Nothing` |
| rleDecode | Medium | Need strings with runs > 9 characters |
| myAbs minBound | Hard | Need to hit minBound specifically, or property must check all negatives become positive |

---

## Learning Points

1. **Test the identity/base cases explicitly** - myProduct empty, safeTail empty
2. **Test that functions satisfy their specification** - myFilter should keep elements satisfying predicate
3. **Test inverse relationships** - encode/decode should round-trip
4. **Consider edge cases** - minBound, very long runs, empty inputs
5. **QuickCheck's shrinking helps** - when a test fails, it finds minimal counterexamples
