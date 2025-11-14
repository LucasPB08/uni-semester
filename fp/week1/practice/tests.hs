-- Test file for Week 1 exercises
-- No libraries needed - just load and run!

{-
  HOW TO USE:

  1. Implement your functions in exercises.hs
  2. Load this file in GHCi:
     :load tests.hs

  3. Run all tests:
     runAllTests

  4. Or check individual exercises:
     testAdd
     testDescribeNumber
     testCountElements
     etc.

  5. After making changes to exercises.hs:
     :reload
     runAllTests
-}

-- ============================================================================
-- Import your solutions from exercises.hs
-- ============================================================================

-- Copy your implementations here, or load exercises.hs first then load this file

-- Exercise 1: add
add :: Int -> Int -> Int
add x y = x + y

-- Exercise 2: both
both :: Bool -> Bool -> Bool
both x y = x && y

-- Exercise 3: describeNumber (YOU NEED TO IMPLEMENT)
describeNumber :: Int -> String
describeNumber n 
  | n > 0 = "positive"
  | n < 0 = "negative"
  | otherwise = "zero" -- Replace with your implementation

-- Exercise 4: countElements (YOU NEED TO IMPLEMENT)
countElements :: [a] -> Int
countElements [] = 0
countElements (x:xs) = 1 + countElements xs 
  -- Replace with your implementation

-- Exercise 5: makeRange (YOU NEED TO IMPLEMENT)
makeRange :: Int -> [Int]
makeRange n 
  | n <= 0 = []
  | otherwise = makeRange (n-1) ++ [n] -- Replace with your implementation

-- Exercise 6: doubleAll (YOU NEED TO IMPLEMENT)
doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x : xs) = (2*x) : doubleAll xs  -- Replace with your implementation

-- Exercise 7: keepPositive (YOU NEED TO IMPLEMENT)
keepPositive :: [Int] -> [Int]
keepPositive [] = []
keepPositive (x : xs)  
  | x <= 0 = keepPositive(xs)
  | otherwise = x : keepPositive(xs) -- Replace with your implementation

-- ============================================================================
-- Test Utilities
-- ============================================================================

-- Helper to run a test and print the result
runTest :: String -> Bool -> IO ()
runTest name result =
  putStrLn $ name ++ ": " ++ if result then "[PASS]" else "[FAIL]"

-- ============================================================================
-- Test Suites
-- ============================================================================

testAdd :: IO ()
testAdd = do
  putStrLn "\n--- Testing add ---"
  runTest "add 3 5 == 8" (add 3 5 == 8)
  runTest "add 10 20 == 30" (add 10 20 == 30)
  runTest "add (-5) 15 == 10" (add (-5) 15 == 10)
  runTest "add 0 0 == 0" (add 0 0 == 0)

testBoth :: IO ()
testBoth = do
  putStrLn "\n--- Testing both ---"
  runTest "both True True == True" (both True True == True)
  runTest "both True False == False" (both True False == False)
  runTest "both False True == False" (both False True == False)
  runTest "both False False == False" (both False False == False)

testDescribeNumber :: IO ()
testDescribeNumber = do
  putStrLn "\n--- Testing describeNumber ---"
  runTest "describeNumber 5 == \"positive\"" (describeNumber 5 == "positive")
  runTest "describeNumber (-3) == \"negative\"" (describeNumber (-3) == "negative")
  runTest "describeNumber 0 == \"zero\"" (describeNumber 0 == "zero")
  runTest "describeNumber 100 == \"positive\"" (describeNumber 100 == "positive")
  runTest "describeNumber (-1) == \"negative\"" (describeNumber (-1) == "negative")

testCountElements :: IO ()
testCountElements = do
  putStrLn "\n--- Testing countElements ---"
  runTest "countElements [] == 0" (countElements [] == 0)
  runTest "countElements [1,2,3] == 3" (countElements [1,2,3] == 3)
  runTest "countElements \"hello\" == 5" (countElements "hello" == 5)
  runTest "countElements [1] == 1" (countElements [1] == 1)
  runTest "countElements [1..10] == 10" (countElements [1..10] == 10)

testMakeRange :: IO ()
testMakeRange = do
  putStrLn "\n--- Testing makeRange ---"
  runTest "makeRange 5 == [1,2,3,4,5]" (makeRange 5 == [1,2,3,4,5])
  runTest "makeRange 1 == [1]" (makeRange 1 == [1])
  runTest "makeRange 0 == []" (makeRange 0 == [])
  runTest "makeRange (-3) == []" (makeRange (-3) == [])
  runTest "makeRange 3 == [1,2,3]" (makeRange 3 == [1,2,3])

testDoubleAll :: IO ()
testDoubleAll = do
  putStrLn "\n--- Testing doubleAll ---"
  runTest "doubleAll [1,2,3] == [2,4,6]" (doubleAll [1,2,3] == [2,4,6])
  runTest "doubleAll [10] == [20]" (doubleAll [10] == [20])
  runTest "doubleAll [] == []" (doubleAll [] == [])
  runTest "doubleAll [0,5,-3] == [0,10,-6]" (doubleAll [0,5,-3] == [0,10,-6])

testKeepPositive :: IO ()
testKeepPositive = do
  putStrLn "\n--- Testing keepPositive ---"
  runTest "keepPositive [1,-2,3,-4,5] == [1,3,5]" (keepPositive [1,-2,3,-4,5] == [1,3,5])
  runTest "keepPositive [-1,-2,-3] == []" (keepPositive [-1,-2,-3] == [])
  runTest "keepPositive [1,2,3] == [1,2,3]" (keepPositive [1,2,3] == [1,2,3])
  runTest "keepPositive [] == []" (keepPositive [] == [])
  runTest "keepPositive [0,1,0,2] == [1,2]" (keepPositive [0,1,0,2] == [1,2])

-- ============================================================================
-- Run All Tests
-- ============================================================================

runAllTests :: IO ()
runAllTests = do
  putStrLn "========================================"
  putStrLn "  Week 1 Exercise Tests"
  putStrLn "========================================"

  testAdd
  testBoth
  testDescribeNumber
  testCountElements
  testMakeRange
  testDoubleAll
  testKeepPositive

  putStrLn "\n========================================"
  putStrLn "  Tests Complete!"
  putStrLn "========================================\n"
