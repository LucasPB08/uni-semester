-- Week 3: Applicatives Exercises
-- Complete each exercise by replacing `undefined` with your implementation

module Applicatives where

import Control.Applicative (liftA2)

-- ============================================================================
-- Exercise 1: Basic Applicative Operations
-- ============================================================================

-- | Apply a Maybe function to a Maybe value
-- Examples:
--   applyMaybe (Just (+1)) (Just 5) == Just 6
--   applyMaybe Nothing (Just 5) == Nothing
--   applyMaybe (Just (*2)) Nothing == Nothing
applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe mf  ma = mf <*> ma

-- | Add two Maybe Int values using Applicative style
-- Examples:
--   addMaybes (Just 3) (Just 5) == Just 8
--   addMaybes Nothing (Just 5) == Nothing
--   addMaybes (Just 3) Nothing == Nothing
addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes a b = pure (+) <*> a <*> b

-- | Multiply three Maybe Int values
-- Examples:
--   multiplyThree (Just 2) (Just 3) (Just 4) == Just 24
--   multiplyThree (Just 2) Nothing (Just 4) == Nothing
multiplyThree :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
multiplyThree a b c = (*) <$> ((*) <$> a <*> b) <*> c

-- ============================================================================
-- Exercise 2: List Applicative
-- ============================================================================

-- | Apply each function to each value (all combinations)
-- Examples:
--   applyList [(+1), (*2)] [10, 20] == [11, 21, 20, 40]
--   applyList [] [1,2,3] == []
--   applyList [(+1)] [] == []
applyList :: [a -> b] -> [a] -> [b]
applyList fs xs = fs <*> xs

-- | Generate all pairs from two lists
-- Examples:
--   allPairs [1,2] ["a","b"] == [(1,"a"),(1,"b"),(2,"a"),(2,"b")]
--   allPairs [] ["a"] == []
allPairs :: [a] -> [b] -> [(a, b)]
allPairs xs ys = (,) <$> xs <*> ys
allPairs xs ys = [(x,y) | x <- xs, y <- ys]

-- | Generate all sums of elements from two lists
-- Examples:
--   allSums [1,2] [10,20] == [11,21,12,22]
--   allSums [0] [5,6,7] == [5,6,7]
allSums :: Num a => [a] -> [a] -> [a]
allSums xs ys = (+) <$> xs <*> ys 

-- | Generate a multiplication table
-- multiplicationTable 3 should give all products of 1..3 with 1..3:
--   [1,2,3,2,4,6,3,6,9]  (1*1, 1*2, 1*3, 2*1, 2*2, 2*3, 3*1, 3*2, 3*3)
multiplicationTable :: Int -> [Int]
multiplicationTable n = (*) <$> [1..n] <*> [1..n]
multiplicationTable n = [x*y | x <- [1..n], y <- [1..n]]

-- ============================================================================
-- Exercise 3: Combining with Applicative
-- ============================================================================

data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Show, Eq)

-- | Create a Person from Maybe name and Maybe age
-- Examples:
--   makePerson (Just "Alice") (Just 30) == Just (Person "Alice" 30)
--   makePerson Nothing (Just 30) == Nothing
--   makePerson (Just "Bob") Nothing == Nothing
makePerson :: Maybe String -> Maybe Int -> Maybe Person
makePerson s n = Person <$> s <*> n

data Point = Point Int Int deriving (Show, Eq)

-- | Create a Point from two Maybe coordinates
-- Examples:
--   makePoint (Just 3) (Just 4) == Just (Point 3 4)
--   makePoint Nothing (Just 4) == Nothing
makePoint :: Maybe Int -> Maybe Int -> Maybe Point
makePoint p1 p2 = Point <$> p1 <*> p2

-- | Calculate distance squared between origin and a Maybe Point
-- If point is Nothing, return Nothing
-- Examples:
--   distanceSquared (Just (Point 3 4)) == Just 25
--   distanceSquared Nothing == Nothing
distanceSquared :: Maybe Point -> Maybe Int
distanceSquared p = fmap (\(Point p1 p2) -> p1^2 + p2^2) p

-- ============================================================================
-- Exercise 4: Validation with Applicative
-- ============================================================================

-- | Validate that a string is non-empty, return Nothing if empty
-- Examples:
--   validateNonEmpty "" == Nothing
--   validateNonEmpty "hello" == Just "hello"
validateNonEmpty :: String -> Maybe String
validateNonEmpty [] = Nothing 
validateNonEmpty s = Just s 

-- | Validate that an Int is positive (> 0), return Nothing if not
-- Examples:
--   validatePositive 5 == Just 5
--   validatePositive 0 == Nothing
--   validatePositive (-3) == Nothing
validatePositive :: Int -> Maybe Int
validatePositive n 
  | n <= 0 = Nothing
  | otherwise = Just n 

-- | Validate that an Int is within a range (inclusive)
-- Examples:
--   validateRange 1 10 5 == Just 5
--   validateRange 1 10 0 == Nothing
--   validateRange 1 10 11 == Nothing
validateRange :: Int -> Int -> Int -> Maybe Int
validateRange lower upper n 
  | n <= upper && lower <= n = Just n 
  | otherwise = Nothing 

data UserInput = UserInput
  { username :: String
  , userAge  :: Int
  , score    :: Int
  } deriving (Show, Eq)

-- | Validate and create UserInput
-- - username must be non-empty
-- - age must be positive
-- - score must be between 0 and 100 (inclusive)
-- Examples:
--   validateUser "alice" 25 85 == Just (UserInput "alice" 25 85)
--   validateUser "" 25 85 == Nothing
--   validateUser "alice" (-1) 85 == Nothing
--   validateUser "alice" 25 101 == Nothing
validateUser :: String -> Int -> Int -> Maybe UserInput
validateUser name age score = UserInput 
  <$> validateNonEmpty name 
  <*> validatePositive age 
  <*> validateRange 0 100 score

-- ============================================================================
-- Exercise 5: Applicative with Either
-- ============================================================================

-- | Safe division that returns Left on division by zero
-- Examples:
--   safeDiv 10 2 == Right 5
--   safeDiv 10 0 == Left "Division by zero"
safeDiv :: Int -> Int -> Either String Int
safeDiv _ 0 = Left "Division by zero"
safeDiv m n = Right (div m n) 

-- | Add two Either Int values
-- Examples:
--   addEithers (Right 3) (Right 5) == Right 8
--   addEithers (Left "error") (Right 5) == Left "error"
--   addEithers (Right 3) (Left "oops") == Left "oops"
addEithers :: Either String Int -> Either String Int -> Either String Int
addEithers n m = (+) <$> n <*> m 

-- | Combine two computations that might fail
-- Given two Either values, create a pair if both are Right
-- Examples:
--   combineBoth (Right 1) (Right "a") == Right (1, "a")
--   combineBoth (Left "err") (Right "a") == Left "err"
combineBoth :: Either e a -> Either e b -> Either e (a, b)
combineBoth  l r = (,) <$> l <*> r 

-- ============================================================================
-- Exercise 6: Applicative for Custom Types
-- ============================================================================

-- A Box that holds exactly one value
data Box a = Box a deriving (Show, Eq)

-- | Make Box a Functor first (needed for Applicative)
instance Functor Box where
  fmap f (Box a)= Box (f a)

-- | Make Box an Applicative
-- pure should wrap a value in a Box
-- (<*>) should apply a boxed function to a boxed value
instance Applicative Box where
  pure = Box  
  Box f <*> Box a = Box (f a)

-- | Use the Box Applicative to add two boxed values
-- Example: addBoxes (Box 3) (Box 5) == Box 8
addBoxes :: Box Int -> Box Int -> Box Int
addBoxes a b = (+) <$> a <*> b

-- ============================================================================
-- Exercise 7: Using liftA2 and liftA3
-- ============================================================================

-- | Rewrite addMaybes using liftA2
-- Examples: same as addMaybes
addMaybesLift :: Maybe Int -> Maybe Int -> Maybe Int
addMaybesLift ma mb = liftA2 (+) ma mb 

-- | Use liftA2 to concatenate two Maybe Strings
-- Examples:
--   concatMaybes (Just "Hello, ") (Just "World") == Just "Hello, World"
--   concatMaybes Nothing (Just "World") == Nothing
concatMaybes :: Maybe String -> Maybe String -> Maybe String
concatMaybes s1 s2 = liftA2 (++) s1 s2

-- liftA3 is similar but for 3 arguments
-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c

-- | Create a 3-tuple from three Maybe values using liftA3
-- Examples:
--   makeTriple (Just 1) (Just 2) (Just 3) == Just (1, 2, 3)
--   makeTriple Nothing (Just 2) (Just 3) == Nothing
makeTriple :: Maybe a -> Maybe b -> Maybe c -> Maybe (a, b, c)
makeTriple ma mb mc = liftA3 (,,) ma mb mc

-- ============================================================================
-- Exercise 8: Sequencing with *> and <*
-- ============================================================================

-- | Return the first Maybe value if both are Just, otherwise Nothing
-- This is the behavior of (<*)
-- Examples:
--   keepFirst (Just 1) (Just 2) == Just 1
--   keepFirst Nothing (Just 2) == Nothing
--   keepFirst (Just 1) Nothing == Nothing
keepFirst :: Maybe a -> Maybe b -> Maybe a
keepFirst ma mb = ma <* mb

-- | Return the second Maybe value if both are Just, otherwise Nothing
-- This is the behavior of (*>)
-- Examples:
--   keepSecond (Just 1) (Just 2) == Just 2
--   keepSecond Nothing (Just 2) == Nothing
--   keepSecond (Just 1) Nothing == Nothing
keepSecond :: Maybe a -> Maybe b -> Maybe b
keepSecond ma mb = ma *> mb

-- ============================================================================
-- Exercise 9: Practical Application
-- ============================================================================

data Config = Config
  { host :: String
  , port :: Int
  , debug :: Bool
  } deriving (Show, Eq)

-- | Parse a string to Int, Nothing if invalid
-- Hint: use reads
-- Examples:
--   parseInt "42" == Just 42
--   parseInt "abc" == Nothing
--   parseInt "" == Nothing
parseInt :: String -> Maybe Int
parseInt s = case (reads s :: [(Int, String)]) of 
  [] -> Nothing 
  [(n, "")] -> Just n

-- | Parse a string to Bool
-- "true" -> Just True, "false" -> Just False, anything else -> Nothing
-- Examples:
--   parseBool "true" == Just True
--   parseBool "false" == Just False
--   parseBool "yes" == Nothing
parseBool :: String -> Maybe Bool
parseBool string 
  | string == "true" = Just True
  | string == "false" = Just False 
  | otherwise = Nothing

-- | Parse config from three string inputs using Applicative
-- The host string must be non-empty
-- The port string must parse to a valid positive Int
-- The debug string must parse to a Bool
-- Examples:
--   parseConfig "localhost" "8080" "true" == Just (Config "localhost" 8080 True)
--   parseConfig "" "8080" "true" == Nothing
--   parseConfig "localhost" "abc" "true" == Nothing
--   parseConfig "localhost" "8080" "yes" == Nothing
parseConfig :: String -> String -> String -> Maybe Config
parseConfig host port debug = Config 
  <$> validateNonEmpty host 
  <*> (parseInt port >>= validatePositive) 
  <*> parseBool debug

-- ============================================================================
-- Exercise 10: Challenge - ZipList Applicative
-- ============================================================================

-- The standard list Applicative gives all combinations.
-- ZipList is an alternative that zips lists together.

newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show, Eq)

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (map f xs)

-- | Implement Applicative for ZipList
-- pure should create an infinite list of the value
-- (<*>) should zip the functions with the values
-- Examples (after implementation):
--   ZipList [(+1), (*2)] <*> ZipList [10, 20] == ZipList [11, 40]
--   pure (+) <*> ZipList [1,2,3] <*> ZipList [10,20,30] == ZipList [11,22,33]
instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList [] <*> _ = ZipList []
  _ <*> ZipList [] = ZipList []
  ZipList fs <*> ZipList ys = ZipList (zipWith (\f y -> f y) fs ys )

-- | Add corresponding elements of two lists
-- Examples:
--   zipAdd (ZipList [1,2,3]) (ZipList [10,20,30]) == ZipList [11,22,33]
--   zipAdd (ZipList [1,2]) (ZipList [10,20,30]) == ZipList [11,22]
zipAdd :: ZipList Int -> ZipList Int -> ZipList Int
zipAdd z1 z2 = (pure (+)) <*> z1 <*> z2
