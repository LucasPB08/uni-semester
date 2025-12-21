-- QuickCheck Exercises: Functions That May Have Bugs
--
-- YOUR TASK: Write QuickCheck properties in tests.hs to test these functions.
-- Some functions are correct, some have subtle bugs.
-- Good properties will catch the bugs!
--
-- DO NOT modify this file. Write your tests in tests.hs.

module Exercises where

import Data.List (sort, nub)
import Data.Char (ord, chr)

-- ============================================================================
-- Section 1: List Operations
-- ============================================================================

-- | Reverse a list
-- Should satisfy: myReverse (myReverse xs) == xs
-- Should satisfy: length (myReverse xs) == length xs
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- | Get the last element of a list
-- Precondition: list is non-empty
-- Should satisfy: myLast xs == head (reverse xs)  (for non-empty xs)
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs
myLast [] = error "myLast: empty list"

-- | Take the first n elements
-- Should satisfy: length (myTake n xs) == min n (length xs)  (for n >= 0)
-- Should satisfy: myTake n xs ++ myDrop n xs == xs
myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake n (x:xs)
  | n <= 0    = []
  | otherwise = x : myTake (n - 1) xs

-- | Drop the first n elements
-- Should satisfy: myTake n xs ++ myDrop n xs == xs
myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop n xs@(_:xs')
  | n <= 0    = xs
  | otherwise = myDrop (n - 1) xs'

-- | Check if element is in list
-- Should satisfy: myElem x xs == (x `elem` xs)
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs)
  | y == x    = True
  | otherwise = myElem y xs

-- | Remove duplicates from a list
-- Should satisfy: all elements in result are unique
-- Should satisfy: all elements in input appear in result
-- Should preserve the ORDER of first occurrences!
myNub :: Eq a => [a] -> [a]
myNub xs = go xs []
  where
    go [] _ = []
    go (y:ys) seen
      | y `elem` seen = go ys seen
      | otherwise     = y : go ys (y : seen)

-- | Sum of a list
-- Should satisfy: mySum [] == 0
-- Should satisfy: mySum [x] == x
-- Should satisfy: mySum (xs ++ ys) == mySum xs + mySum ys
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

-- | Product of a list
-- Should satisfy: myProduct [] == 1 (identity for multiplication)
-- Should satisfy: myProduct [x] == x
-- Should satisfy: myProduct (xs ++ ys) == myProduct xs * myProduct ys
myProduct :: Num a => [a] -> a
myProduct [] = 0  -- Hmm, what should the product of an empty list be?
myProduct (x:xs) = x * myProduct xs

-- | Length of a list
-- Should satisfy: myLength [] == 0
-- Should satisfy: myLength (x:xs) == 1 + myLength xs
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- | Concatenate a list of lists
-- Should satisfy: myConcat [[]] == []
-- Should satisfy: myConcat [xs] == xs
-- Should satisfy: length (myConcat xss) == sum (map length xss)
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (xs:xss) = xs ++ myConcat xss

-- | Zip two lists together
-- Should satisfy: length (myZip xs ys) == min (length xs) (length ys)
-- Should satisfy: map fst (myZip xs ys) `isPrefixOf` xs
-- Should satisfy: map snd (myZip xs ys) `isPrefixOf` ys
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

-- ============================================================================
-- Section 2: Higher-Order Functions
-- ============================================================================

-- | Map a function over a list
-- Should satisfy: length (myMap f xs) == length xs
-- Should satisfy: myMap id xs == xs
-- Should satisfy: myMap (f . g) xs == myMap f (myMap g xs)
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- | Filter elements satisfying a predicate
-- Should satisfy: all p (myFilter p xs) == True
-- Should satisfy: length (myFilter p xs) <= length xs
-- Should satisfy: myFilter (const True) xs == xs
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
  | not (p x) = x : myFilter p xs  -- Keep elements that DON'T satisfy p? Hmm...
  | otherwise = myFilter p xs

-- | Fold from the right
-- Should satisfy: myFoldr (:) [] xs == xs
-- Should satisfy: myFoldr (+) 0 xs == sum xs
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z [] = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

-- | Fold from the left
-- Should satisfy: myFoldl (flip (:)) [] xs == reverse xs
-- Should satisfy: myFoldl (+) 0 xs == sum xs
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ z [] = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs

-- ============================================================================
-- Section 3: Safe Variants
-- ============================================================================

-- | Safe head - returns Nothing for empty list
-- Should satisfy: safeHead [] == Nothing
-- Should satisfy: safeHead (x:xs) == Just x
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- | Safe tail - returns Nothing for empty list
-- Should satisfy: safeTail [] == Nothing
-- Should satisfy: safeTail [x] == Just []
-- Should satisfy: safeTail (x:xs) == Just xs
safeTail :: [a] -> Maybe [a]
safeTail [] = Just []  -- Empty list has an empty tail, right? Or should this be Nothing?
safeTail (_:xs) = Just xs

-- | Safe division
-- Should satisfy: safeDiv x 0 == Nothing
-- Should satisfy: safeDiv x y == Just (x `div` y) when y /= 0
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

-- | Safe index
-- Should satisfy: safeIndex xs n == Nothing if n < 0 or n >= length xs
-- Should satisfy: safeIndex xs n == Just (xs !! n) if 0 <= n < length xs
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n
  | n < 0     = Nothing
  | otherwise = safeIndex xs (n - 1)

-- ============================================================================
-- Section 4: Numeric Functions
-- ============================================================================

-- | Absolute value
-- Should satisfy: myAbs x >= 0
-- Should satisfy: myAbs x == x when x >= 0
-- Should satisfy: myAbs x == -x when x < 0
-- WARNING: What happens with minBound :: Int?
myAbs :: Int -> Int
myAbs x
  | x < 0     = -x
  | otherwise = x

-- | Maximum of two values
-- Should satisfy: myMax x y >= x && myMax x y >= y
-- Should satisfy: myMax x y == x || myMax x y == y
myMax :: Ord a => a -> a -> a
myMax x y
  | x >= y    = x
  | otherwise = y

-- | Minimum of two values
-- Should satisfy: myMin x y <= x && myMin x y <= y
-- Should satisfy: myMin x y == x || myMin x y == y
myMin :: Ord a => a -> a -> a
myMin x y
  | x <= y    = x
  | otherwise = y

-- | Clamp a value to a range
-- Should satisfy: myClamp lo hi x >= lo && myClamp lo hi x <= hi
-- Should satisfy: myClamp lo hi x == x when lo <= x && x <= hi
-- Precondition: lo <= hi
myClamp :: Ord a => a -> a -> a -> a
myClamp lo hi x
  | x < lo    = lo
  | x > hi    = hi
  | otherwise = x

-- ============================================================================
-- Section 5: String/Encoding Functions
-- ============================================================================

-- | Simple Caesar cipher (shift by n)
-- Should satisfy: caesarDecode n (caesarEncode n s) == s
caesarEncode :: Int -> String -> String
caesarEncode n = map (shiftChar n)
  where
    shiftChar k c
      | c >= 'a' && c <= 'z' = chr $ ((ord c - ord 'a' + k) `mod` 26) + ord 'a'
      | c >= 'A' && c <= 'Z' = chr $ ((ord c - ord 'A' + k) `mod` 26) + ord 'A'
      | otherwise = c

caesarDecode :: Int -> String -> String
caesarDecode n = caesarEncode (-n)

-- | Run-length encoding
-- "aaabbc" -> [(3,'a'),(2,'b'),(1,'c')]
-- Should satisfy: rleDecode (rleEncode s) == s
rleEncode :: Eq a => [a] -> [(Int, a)]
rleEncode [] = []
rleEncode (x:xs) = (1 + length (takeWhile (== x) xs), x) : rleEncode (dropWhile (== x) xs)

-- | Run-length decoding
-- [(3,'a'),(2,'b')] -> "aaabb"
rleDecode :: [(Int, a)] -> [a]
rleDecode [] = []
rleDecode ((n, x):rest)
  | n <= 0    = rleDecode rest  -- Skip zero or negative counts
  | n > 9     = replicate 9 x ++ rleDecode ((n - 9, x) : rest)  -- Limit to 9? Why?
  | otherwise = replicate n x ++ rleDecode rest

-- ============================================================================
-- Section 6: Sorting and Searching
-- ============================================================================

-- | Insertion sort
-- Should satisfy: isSorted (insertionSort xs)
-- Should satisfy: insertionSort xs is a permutation of xs
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
      | y <= z    = y : z : zs
      | otherwise = z : insert y zs

-- | Binary search on a sorted list
-- Precondition: list is sorted
-- Should satisfy: binarySearch x xs == (x `elem` xs) when xs is sorted
binarySearch :: Ord a => a -> [a] -> Bool
binarySearch _ [] = False
binarySearch x xs =
  let mid = length xs `div` 2
      midVal = xs !! mid
  in case compare x midVal of
       EQ -> True
       LT -> binarySearch x (take mid xs)
       GT -> binarySearch x (drop (mid + 1) xs)

-- | Find the median of a list
-- For odd-length lists, return the middle element
-- For even-length lists, return the lower middle element
-- Precondition: non-empty list
-- Should satisfy: median xs is in xs
-- Should satisfy: approximately half the elements are <= median xs
median :: Ord a => [a] -> a
median xs =
  let sorted = sort xs
      n = length sorted
  in sorted !! (n `div` 2)

-- ============================================================================
-- Section 7: Misc
-- ============================================================================

-- | Fibonacci sequence (infinite)
-- Should satisfy: take 10 fibs == [0,1,1,2,3,5,8,13,21,34]
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- | Factorial
-- Should satisfy: factorial n == product [1..n] for n >= 0
-- Precondition: n >= 0
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- | Greatest common divisor
-- Should satisfy: myGcd a b divides both a and b
-- Should satisfy: myGcd a b >= myGcd a b' for any common divisor b'
-- Should satisfy: myGcd a b == gcd a b
myGcd :: Int -> Int -> Int
myGcd a 0 = abs a
myGcd a b = myGcd b (a `mod` b)

-- | Check if a number is prime
-- Should satisfy: isPrime 2 == True
-- Should satisfy: isPrime 1 == False
-- Should satisfy: isPrime n == (n > 1 && no divisors in [2..n-1])
isPrime :: Int -> Bool
isPrime n
  | n < 2     = False
  | n == 2    = True
  | even n    = False
  | otherwise = all (\d -> n `mod` d /= 0) [3, 5 .. isqrt n]
  where
    isqrt = floor . sqrt . fromIntegral

-- | Replicate a value n times
-- Should satisfy: length (myReplicate n x) == max 0 n
-- Should satisfy: all (== x) (myReplicate n x)
myReplicate :: Int -> a -> [a]
myReplicate n x
  | n <= 0    = []
  | otherwise = x : myReplicate (n - 1) x

-- | Intersperse an element between list elements
-- Should satisfy: myIntersperse sep [] == []
-- Should satisfy: myIntersperse sep [x] == [x]
-- Should satisfy: length (myIntersperse sep xs) == max 0 (2 * length xs - 1)
myIntersperse :: a -> [a] -> [a]
myIntersperse _ [] = []
myIntersperse _ [x] = [x]
myIntersperse sep (x:xs) = x : sep : myIntersperse sep xs
