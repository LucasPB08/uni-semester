-- QuickCheck Tests: Find the Bugs!
--
-- YOUR TASK: Write QuickCheck properties to test the functions in exercises.hs
-- Run your properties with: quickCheck prop_name
-- Or run all tests with: runAllTests
--
-- Some functions have bugs - your properties should find them!
-- A good property will FAIL on buggy functions.
-- A weak property might PASS even when there's a bug.

module Tests where

import Test.QuickCheck
import Exercises
import Data.List (sort, nub, isPrefixOf)

-- ============================================================================
-- Example: Properties for myReverse (COMPLETED - use as reference)
-- ============================================================================

-- Property: Reversing twice gives back the original
prop_reverseReverse :: [Int] -> Bool
prop_reverseReverse xs = myReverse (myReverse xs) == xs

-- Property: Reverse preserves length
prop_reverseLength :: [Int] -> Bool
prop_reverseLength xs = length (myReverse xs) == length xs

-- Property: Reverse of singleton is itself
prop_reverseSingleton :: Int -> Bool
prop_reverseSingleton x = myReverse [x] == [x]

-- Property: Reverse of concatenation
prop_reverseConcat :: [Int] -> [Int] -> Bool
prop_reverseConcat xs ys = myReverse (xs ++ ys) == myReverse ys ++ myReverse xs

-- ============================================================================
-- Section 1: List Operations - YOUR TURN!
-- ============================================================================

-- Properties for myLast
-- Hint: Compare to head (reverse xs) for non-empty lists
-- Hint: Use ==> for preconditions

prop_myLast :: [Int] -> Property
prop_myLast xs = undefined  -- YOUR CODE HERE

-- Properties for myTake
-- Hint: Test relationship with myDrop
-- Hint: Test length of result
-- Hint: Consider edge cases: n = 0, n > length xs, n < 0

prop_myTakeDrop :: Int -> [Int] -> Bool
prop_myTakeDrop n xs = undefined  -- YOUR CODE HERE

prop_myTakeLength :: Int -> [Int] -> Property
prop_myTakeLength n xs = undefined  -- YOUR CODE HERE

-- Properties for myDrop
-- Hint: Similar to myTake tests

prop_myDropLength :: Int -> [Int] -> Property
prop_myDropLength n xs = undefined  -- YOUR CODE HERE

-- Properties for myElem
-- Hint: Compare to standard elem

prop_myElem :: Int -> [Int] -> Bool
prop_myElem x xs = undefined  -- YOUR CODE HERE

-- Properties for myNub
-- Hint: All elements in result should be unique (no duplicates)
-- Hint: All elements from input should appear in result
-- Hint: ORDER should be preserved (first occurrence of each element)
-- This is tricky - think carefully about what properties myNub should satisfy!

prop_myNubUnique :: [Int] -> Bool
prop_myNubUnique xs = undefined  -- YOUR CODE HERE

prop_myNubPreservesElements :: [Int] -> Bool
prop_myNubPreservesElements xs = undefined  -- YOUR CODE HERE

prop_myNubOrder :: [Int] -> Bool
prop_myNubOrder xs = undefined  -- YOUR CODE HERE

-- Properties for mySum
-- Hint: Empty list should sum to 0
-- Hint: Test with concatenation
-- Hint: Compare to standard sum

prop_mySum :: [Int] -> Bool
prop_mySum xs = undefined  -- YOUR CODE HERE

prop_mySumConcat :: [Int] -> [Int] -> Bool
prop_mySumConcat xs ys = undefined  -- YOUR CODE HERE

-- Properties for myProduct
-- Hint: What should myProduct [] be? (Think: identity for multiplication)
-- Hint: Test with concatenation
-- Hint: Compare to standard product

prop_myProductEmpty :: Bool
prop_myProductEmpty = undefined  -- YOUR CODE HERE

prop_myProduct :: [Int] -> Bool
prop_myProduct xs = undefined  -- YOUR CODE HERE

-- Properties for myLength
-- Hint: Compare to standard length

prop_myLength :: [Int] -> Bool
prop_myLength xs = undefined  -- YOUR CODE HERE

-- Properties for myConcat
-- Hint: Compare to standard concat
-- Hint: Test length property

prop_myConcat :: [[Int]] -> Bool
prop_myConcat xss = undefined  -- YOUR CODE HERE

-- Properties for myZip
-- Hint: Length should be min of two input lengths
-- Hint: map fst and map snd should give prefixes of inputs

prop_myZipLength :: [Int] -> [Char] -> Bool
prop_myZipLength xs ys = undefined  -- YOUR CODE HERE

prop_myZipFst :: [Int] -> [Char] -> Bool
prop_myZipFst xs ys = undefined  -- YOUR CODE HERE

-- ============================================================================
-- Section 2: Higher-Order Functions
-- ============================================================================

-- Properties for myMap
-- Hint: myMap id should be identity
-- Hint: Length should be preserved
-- Hint: Test composition: myMap (f . g) == myMap f . myMap g

prop_myMapId :: [Int] -> Bool
prop_myMapId xs = undefined  -- YOUR CODE HERE

prop_myMapLength :: [Int] -> Bool
prop_myMapLength xs = undefined  -- YOUR CODE HERE

prop_myMapComposition :: [Int] -> Bool
prop_myMapComposition xs = undefined  -- YOUR CODE HERE

-- Properties for myFilter
-- Hint: All elements in result should satisfy the predicate
-- Hint: Length of result <= length of input
-- Hint: myFilter (const True) should return the original list
-- Hint: myFilter (const False) should return []

prop_myFilterAll :: [Int] -> Bool
prop_myFilterAll xs = undefined  -- YOUR CODE HERE

prop_myFilterNone :: [Int] -> Bool
prop_myFilterNone xs = undefined  -- YOUR CODE HERE

prop_myFilterSatisfies :: [Int] -> Bool
prop_myFilterSatisfies xs = undefined  -- YOUR CODE HERE

-- Properties for myFoldr
-- Hint: myFoldr (:) [] xs == xs
-- Hint: Compare to standard foldr

prop_myFoldrCons :: [Int] -> Bool
prop_myFoldrCons xs = undefined  -- YOUR CODE HERE

prop_myFoldrSum :: [Int] -> Bool
prop_myFoldrSum xs = undefined  -- YOUR CODE HERE

-- Properties for myFoldl
-- Hint: myFoldl (flip (:)) [] xs == reverse xs

prop_myFoldlReverse :: [Int] -> Bool
prop_myFoldlReverse xs = undefined  -- YOUR CODE HERE

-- ============================================================================
-- Section 3: Safe Variants
-- ============================================================================

-- Properties for safeHead
-- Hint: safeHead [] should be Nothing
-- Hint: safeHead (x:xs) should be Just x

prop_safeHeadEmpty :: Bool
prop_safeHeadEmpty = undefined  -- YOUR CODE HERE

prop_safeHeadNonEmpty :: Int -> [Int] -> Bool
prop_safeHeadNonEmpty x xs = undefined  -- YOUR CODE HERE

-- Properties for safeTail
-- Hint: safeTail [] should be Nothing (NOT Just [])
-- Hint: safeTail [x] should be Just []
-- Hint: safeTail (x:xs) should be Just xs

prop_safeTailEmpty :: Bool
prop_safeTailEmpty = undefined  -- YOUR CODE HERE

prop_safeTailSingleton :: Int -> Bool
prop_safeTailSingleton x = undefined  -- YOUR CODE HERE

prop_safeTailNonEmpty :: Int -> [Int] -> Bool
prop_safeTailNonEmpty x xs = undefined  -- YOUR CODE HERE

-- Properties for safeDiv
-- Hint: Division by zero should be Nothing

prop_safeDivByZero :: Int -> Bool
prop_safeDivByZero x = undefined  -- YOUR CODE HERE

prop_safeDivNonZero :: Int -> Int -> Property
prop_safeDivNonZero x y = undefined  -- YOUR CODE HERE

-- Properties for safeIndex
-- Hint: Negative indices should be Nothing
-- Hint: Indices >= length should be Nothing

prop_safeIndexBounds :: [Int] -> Int -> Property
prop_safeIndexBounds xs n = undefined  -- YOUR CODE HERE

-- ============================================================================
-- Section 4: Numeric Functions
-- ============================================================================

-- Properties for myAbs
-- Hint: Result should always be >= 0
-- Hint: myAbs x == x when x >= 0
-- Hint: myAbs x == -x when x < 0
-- WARNING: There's a subtle bug with minBound. Can you find it?

prop_myAbsNonNegative :: Int -> Bool
prop_myAbsNonNegative x = undefined  -- YOUR CODE HERE

prop_myAbsPositive :: Int -> Property
prop_myAbsPositive x = undefined  -- YOUR CODE HERE

prop_myAbsNegative :: Int -> Property
prop_myAbsNegative x = undefined  -- YOUR CODE HERE

-- Properties for myMax, myMin
-- Hint: Result should be >= both inputs (for max) or <= both inputs (for min)
-- Hint: Result should be one of the inputs

prop_myMaxGeq :: Int -> Int -> Bool
prop_myMaxGeq x y = undefined  -- YOUR CODE HERE

prop_myMinLeq :: Int -> Int -> Bool
prop_myMinLeq x y = undefined  -- YOUR CODE HERE

-- Properties for myClamp

prop_myClampBounds :: Int -> Int -> Int -> Property
prop_myClampBounds lo hi x = undefined  -- YOUR CODE HERE

-- ============================================================================
-- Section 5: String/Encoding Functions
-- ============================================================================

-- Properties for Caesar cipher
-- Hint: Decode should be inverse of encode

prop_caesarInverse :: Int -> String -> Bool
prop_caesarInverse n s = undefined  -- YOUR CODE HERE

-- Properties for run-length encoding
-- Hint: Decode should be inverse of encode
-- THIS ONE MIGHT REVEAL A BUG!

prop_rleInverse :: [Char] -> Bool
prop_rleInverse s = undefined  -- YOUR CODE HERE

-- ============================================================================
-- Section 6: Sorting and Searching
-- ============================================================================

-- Properties for insertionSort
-- Hint: Result should be sorted
-- Hint: Result should be a permutation of input

prop_insertionSortSorted :: [Int] -> Bool
prop_insertionSortSorted xs = undefined  -- YOUR CODE HERE

prop_insertionSortPermutation :: [Int] -> Bool
prop_insertionSortPermutation xs = undefined  -- YOUR CODE HERE

-- Helper function for checking if a list is sorted
isSorted :: Ord a => [a] -> Bool
isSorted xs = and $ zipWith (<=) xs (drop 1 xs)

-- Helper function for checking permutation
isPermutationOf :: Ord a => [a] -> [a] -> Bool
isPermutationOf xs ys = sort xs == sort ys

-- Properties for binarySearch
-- Hint: Should find elements that exist
-- Hint: Should not find elements that don't exist
-- Precondition: list must be sorted!

prop_binarySearchFinds :: Int -> [Int] -> Property
prop_binarySearchFinds x xs = undefined  -- YOUR CODE HERE

-- Properties for median

prop_medianInList :: [Int] -> Property
prop_medianInList xs = undefined  -- YOUR CODE HERE

-- ============================================================================
-- Section 7: Misc
-- ============================================================================

-- Properties for fibs
-- Hint: Test specific values
-- Hint: Test the recurrence relation: fibs[n] = fibs[n-1] + fibs[n-2]

prop_fibsStart :: Bool
prop_fibsStart = undefined  -- YOUR CODE HERE

prop_fibsRecurrence :: Int -> Property
prop_fibsRecurrence n = undefined  -- YOUR CODE HERE

-- Properties for factorial
-- Hint: factorial 0 = 1
-- Hint: factorial n = n * factorial (n-1)

prop_factorialBase :: Bool
prop_factorialBase = undefined  -- YOUR CODE HERE

prop_factorialRecurrence :: Integer -> Property
prop_factorialRecurrence n = undefined  -- YOUR CODE HERE

-- Properties for myGcd
-- Hint: Compare to standard gcd

prop_myGcd :: Int -> Int -> Bool
prop_myGcd x y = undefined  -- YOUR CODE HERE

-- Properties for isPrime
-- Hint: 2 is prime
-- Hint: 1 is not prime
-- Hint: Even numbers > 2 are not prime

prop_isPrime2 :: Bool
prop_isPrime2 = undefined  -- YOUR CODE HERE

prop_isPrime1 :: Bool
prop_isPrime1 = undefined  -- YOUR CODE HERE

-- Properties for myReplicate

prop_myReplicateLength :: Int -> Char -> Property
prop_myReplicateLength n x = undefined  -- YOUR CODE HERE

prop_myReplicateAll :: Int -> Int -> Property
prop_myReplicateAll n x = undefined  -- YOUR CODE HERE

-- Properties for myIntersperse
-- Hint: Empty list stays empty
-- Hint: Singleton stays singleton
-- Hint: Length of result = 2 * length input - 1 (for non-empty)

prop_myIntersperseEmpty :: Char -> Bool
prop_myIntersperseEmpty sep = undefined  -- YOUR CODE HERE

prop_myIntersperseSingleton :: Char -> Int -> Bool
prop_myIntersperseSingleton sep x = undefined  -- YOUR CODE HERE

prop_myIntersperseLength :: Char -> [Int] -> Property
prop_myIntersperseLength sep xs = undefined  -- YOUR CODE HERE

-- ============================================================================
-- Run All Tests
-- ============================================================================

-- Run this to test all your properties at once
-- Usage in GHCi: runAllTests
runAllTests :: IO ()
runAllTests = do
  putStrLn "============================================"
  putStrLn "QuickCheck Tests"
  putStrLn "============================================"
  putStrLn ""

  putStrLn "--- myReverse (example) ---"
  quickCheck prop_reverseReverse
  quickCheck prop_reverseLength
  quickCheck prop_reverseSingleton
  quickCheck prop_reverseConcat
  putStrLn ""

  -- Uncomment these as you implement the properties:

  -- putStrLn "--- myLast ---"
  -- quickCheck prop_myLast
  -- putStrLn ""

  -- putStrLn "--- myTake / myDrop ---"
  -- quickCheck prop_myTakeDrop
  -- quickCheck prop_myTakeLength
  -- quickCheck prop_myDropLength
  -- putStrLn ""

  -- putStrLn "--- myElem ---"
  -- quickCheck prop_myElem
  -- putStrLn ""

  -- putStrLn "--- myNub ---"
  -- quickCheck prop_myNubUnique
  -- quickCheck prop_myNubPreservesElements
  -- quickCheck prop_myNubOrder
  -- putStrLn ""

  -- putStrLn "--- mySum ---"
  -- quickCheck prop_mySum
  -- quickCheck prop_mySumConcat
  -- putStrLn ""

  -- putStrLn "--- myProduct ---"
  -- quickCheck prop_myProductEmpty
  -- quickCheck prop_myProduct
  -- putStrLn ""

  -- putStrLn "--- myLength ---"
  -- quickCheck prop_myLength
  -- putStrLn ""

  -- putStrLn "--- myConcat ---"
  -- quickCheck prop_myConcat
  -- putStrLn ""

  -- putStrLn "--- myZip ---"
  -- quickCheck prop_myZipLength
  -- quickCheck prop_myZipFst
  -- putStrLn ""

  -- putStrLn "--- myMap ---"
  -- quickCheck prop_myMapId
  -- quickCheck prop_myMapLength
  -- quickCheck prop_myMapComposition
  -- putStrLn ""

  -- putStrLn "--- myFilter ---"
  -- quickCheck prop_myFilterAll
  -- quickCheck prop_myFilterNone
  -- quickCheck prop_myFilterSatisfies
  -- putStrLn ""

  -- putStrLn "--- myFoldr ---"
  -- quickCheck prop_myFoldrCons
  -- quickCheck prop_myFoldrSum
  -- putStrLn ""

  -- putStrLn "--- myFoldl ---"
  -- quickCheck prop_myFoldlReverse
  -- putStrLn ""

  -- putStrLn "--- safeHead ---"
  -- quickCheck prop_safeHeadEmpty
  -- quickCheck prop_safeHeadNonEmpty
  -- putStrLn ""

  -- putStrLn "--- safeTail ---"
  -- quickCheck prop_safeTailEmpty
  -- quickCheck prop_safeTailSingleton
  -- quickCheck prop_safeTailNonEmpty
  -- putStrLn ""

  -- putStrLn "--- safeDiv ---"
  -- quickCheck prop_safeDivByZero
  -- quickCheck prop_safeDivNonZero
  -- putStrLn ""

  -- putStrLn "--- safeIndex ---"
  -- quickCheck prop_safeIndexBounds
  -- putStrLn ""

  -- putStrLn "--- myAbs ---"
  -- quickCheck prop_myAbsNonNegative
  -- quickCheck prop_myAbsPositive
  -- quickCheck prop_myAbsNegative
  -- putStrLn ""

  -- putStrLn "--- myMax / myMin ---"
  -- quickCheck prop_myMaxGeq
  -- quickCheck prop_myMinLeq
  -- putStrLn ""

  -- putStrLn "--- myClamp ---"
  -- quickCheck prop_myClampBounds
  -- putStrLn ""

  -- putStrLn "--- Caesar cipher ---"
  -- quickCheck prop_caesarInverse
  -- putStrLn ""

  -- putStrLn "--- Run-length encoding ---"
  -- quickCheck prop_rleInverse
  -- putStrLn ""

  -- putStrLn "--- insertionSort ---"
  -- quickCheck prop_insertionSortSorted
  -- quickCheck prop_insertionSortPermutation
  -- putStrLn ""

  -- putStrLn "--- binarySearch ---"
  -- quickCheck prop_binarySearchFinds
  -- putStrLn ""

  -- putStrLn "--- median ---"
  -- quickCheck prop_medianInList
  -- putStrLn ""

  -- putStrLn "--- fibs ---"
  -- quickCheck prop_fibsStart
  -- quickCheck prop_fibsRecurrence
  -- putStrLn ""

  -- putStrLn "--- factorial ---"
  -- quickCheck prop_factorialBase
  -- quickCheck prop_factorialRecurrence
  -- putStrLn ""

  -- putStrLn "--- myGcd ---"
  -- quickCheck prop_myGcd
  -- putStrLn ""

  -- putStrLn "--- isPrime ---"
  -- quickCheck prop_isPrime2
  -- quickCheck prop_isPrime1
  -- putStrLn ""

  -- putStrLn "--- myReplicate ---"
  -- quickCheck prop_myReplicateLength
  -- quickCheck prop_myReplicateAll
  -- putStrLn ""

  -- putStrLn "--- myIntersperse ---"
  -- quickCheck prop_myIntersperseEmpty
  -- quickCheck prop_myIntersperseSingleton
  -- quickCheck prop_myIntersperseLength
  -- putStrLn ""

  putStrLn "============================================"
  putStrLn "Done!"
  putStrLn "============================================"
