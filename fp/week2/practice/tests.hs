-- Week 2 Tests
-- Load this file in GHCi and run `runAllTests`

import Data.Char (toUpper)

-- Import the exercises
-- If you get errors, make sure exercises.hs is in the same directory
-- and load it with: :load exercises.hs

tails :: [a] -> [[a]]
tails [] = [[]]
tails all@(x : xs) = all : tails xs

applyTwice :: (a -> a) -> a -> a
applyTwice f a = f (f a)

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose fun1 fun2 x = fun1 (fun2 x)

flipArgs :: (a -> b -> c) -> (b -> a -> c)
flipArgs fun a b = fun b a

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 fun x = x
applyNTimes n fun x 
  | n <= 0 = x
  | otherwise = applyNTimes (n-1) fun (fun x)

buildMultiplier :: Int -> (Int -> Int)
buildMultiplier n x = n * x

duplicateEach :: [a] -> [a]
duplicateEach [] = []
duplicateEach (x:xs) = x : x : duplicateEach xs

pipelineFunctions :: [a -> a] -> (a -> a)
pipelineFunctions [] el = el
pipelineFunctions [fun] el = fun el
pipelineFunctions (fun : rest) el = fun (pipelineFunctions rest el)
-- Test Helper Functions
testCase :: (Eq a, Show a) => String -> a -> a -> IO ()
testCase name expected actual =
  if expected == actual
    then putStrLn $ "  [PASS] " ++ name
    else putStrLn $ "  [FAIL] " ++ name ++ "\n    Expected: " ++ show expected ++ "\n    Got: " ++ show actual

buildFilter :: (a -> Bool) -> ([a] -> [a])
buildFilter fun [] = []
buildFilter fun (x : xs) 
 | fun x = x : buildFilter fun xs
 | otherwise = buildFilter fun xs

inRange :: Int -> Int -> Int -> Bool
inRange lower upper el 
 | lower <= el && el <= upper = True
 | otherwise = False

testHeader :: String -> IO ()
testHeader name = putStrLn $ "\n" ++ name ++ ":"

-- ============================================================================
-- Test Suite
-- ============================================================================

testTails :: IO ()
testTails = do
  testHeader "Exercise 1: tails"
  testCase "tails [1,2,3]" [[1,2,3], [2,3], [3], []] (tails [1,2,3])
  testCase "tails \"abc\"" ["abc", "bc", "c", ""] (tails "abc")
  testCase "tails []" [[]] (tails ([] :: [Int]))
  testCase "tails [1]" [[1], []] (tails [1])
  testCase "tails [5,10]" [[5,10], [10], []] (tails [5,10])

testApplyTwice :: IO ()
testApplyTwice = do
  testHeader "Exercise 2: applyTwice"
  testCase "applyTwice (+3) 5" 11 (applyTwice (+3) 5)
  testCase "applyTwice (*2) 3" 12 (applyTwice (*2) 3)
  testCase "applyTwice tail [1,2,3,4]" [3,4] (applyTwice tail [1,2,3,4])
  testCase "applyTwice reverse [1,2]" [1,2] (applyTwice reverse [1,2])
  testCase "applyTwice (+1) 0" 2 (applyTwice (+1) 0)

testCompose :: IO ()
testCompose = do
  testHeader "Exercise 3: compose"
  let double x = x * 2
      plusOne x = x + 1
  testCase "compose plusOne double 5" 11 (compose plusOne double 5)
  testCase "compose (*3) (+2) 5" 21 (compose (*3) (+2) 5)
  testCase "compose head tail [1,2,3]" 2 (compose head tail [1,2,3])
  let exclaim s = s ++ "!"
  testCase "compose exclaim (map toUpper) \"hi\"" "HI!" (compose exclaim (map toUpper) "hi")

testFlipArgs :: IO ()
testFlipArgs = do
  testHeader "Exercise 4: flipArgs"
  let subtract' x y = x - y
  testCase "flipArgs subtract 5 10" 5 (flipArgs subtract' 5 10)
  let divide x y = x / y
  testCase "flipArgs divide 2.0 10.0" 5.0 (flipArgs divide 2.0 10.0)
  testCase "flipArgs (:) [2,3] 1" [1,2,3] (flipArgs (:) [2,3] 1)
  testCase "flipArgs (++) \"world\" \"hello\"" "helloworld" (flipArgs (++) "world" "hello")

testApplyNTimes :: IO ()
testApplyNTimes = do
  testHeader "Exercise 5: applyNTimes"
  testCase "applyNTimes 3 (+1) 5" 8 (applyNTimes 3 (+1) 5)
  testCase "applyNTimes 4 (*2) 1" 16 (applyNTimes 4 (*2) 1)
  testCase "applyNTimes 2 tail [1,2,3,4]" [3,4] (applyNTimes 2 tail [1,2,3,4])
  testCase "applyNTimes 0 (*10) 5" 5 (applyNTimes 0 (*10) 5)
  testCase "applyNTimes (-1) (+1) 5" 5 (applyNTimes (-1) (+1) 5)

testBuildMultiplier :: IO ()
testBuildMultiplier = do
  testHeader "Exercise 6: buildMultiplier"
  let times3 = buildMultiplier 3
  testCase "times3 5" 15 (times3 5)
  let times10 = buildMultiplier 10
  testCase "times10 7" 70 (times10 7)
  testCase "buildMultiplier 0 100" 0 (buildMultiplier 0 100)
  testCase "buildMultiplier 5 (-2)" (-10) (buildMultiplier 5 (-2))

testDuplicateEach :: IO ()
testDuplicateEach = do
  testHeader "Exercise 7: duplicateEach"
  testCase "duplicateEach [1,2,3]" [1,1,2,2,3,3] (duplicateEach [1,2,3])
  testCase "duplicateEach \"ab\"" "aabb" (duplicateEach "ab")
  testCase "duplicateEach []" ([] :: [Int]) (duplicateEach [])
  testCase "duplicateEach [5]" [5,5] (duplicateEach [5])

testPipelineFunctions :: IO ()
testPipelineFunctions = do
  testHeader "BONUS Exercise 8: pipelineFunctions"
  let fs = [(+1), (*2), (+10)]
  testCase "pipelineFunctions [(+1), (*2), (+10)] 5" 31 (pipelineFunctions fs 5)
  let fs2 = [tail, tail]
  testCase "pipelineFunctions [tail, tail] [1,2,3,4]" [3,4] (pipelineFunctions fs2 [1,2,3,4])
  testCase "pipelineFunctions [] 10" 10 (pipelineFunctions [] 10)

testBuildFilter :: IO ()
testBuildFilter = do
  testHeader "BONUS Exercise 9: buildFilter"
  let keepEvens = buildFilter even
  testCase "keepEvens [1,2,3,4,5,6]" [2,4,6] (keepEvens [1,2,3,4,5,6])
  let keepPositive = buildFilter (> 0)
  testCase "keepPositive [-1,2,-3,4]" [2,4] (keepPositive [-1,2,-3,4])
  let keepLong = buildFilter (\s -> length s > 3)
  testCase "keepLong [\"hi\",\"hello\",\"yo\",\"world\"]" ["hello","world"] (keepLong ["hi","hello","yo","world"])

testInRange :: IO ()
testInRange = do
  testHeader "BONUS Exercise 10: inRange"
  let isDigit = inRange 0 9
  testCase "isDigit 5" True (isDigit 5)
  testCase "isDigit 10" False (isDigit 10)
  let isTeen = inRange 13 19
  testCase "isTeen 15" True (isTeen 15)
  testCase "isTeen 12" False (isTeen 12)
  testCase "inRange 10 20 25" False (inRange 10 20 25)
  testCase "inRange 10 20 15" True (inRange 10 20 15)

-- ============================================================================
-- Run All Tests
-- ============================================================================

runAllTests :: IO ()
runAllTests = do
  putStrLn "\n========================================="
  putStrLn "Running Week 2 Tests"
  putStrLn "=========================================\n"
  testTails
  testApplyTwice
  testCompose
  testFlipArgs
  testApplyNTimes
  testBuildMultiplier
  testDuplicateEach
  testPipelineFunctions
  testBuildFilter
  testInRange
  putStrLn "\n========================================="
  putStrLn "Tests Complete!"
  putStrLn "=========================================\n"

-- ============================================================================
-- Usage Instructions
-- ============================================================================

{-
To run these tests:

1. Make sure exercises.hs is in the same directory and has your solutions

2. Start GHCi:
   ghci

3. Load this test file (it will also load exercises.hs):
   :load tests.hs

4. Run all tests:
   runAllTests

5. Or run individual test suites:
   testTails
   testApplyTwice
   testCompose
   etc.

6. If you modify your solutions:
   :reload
   runAllTests
-}
