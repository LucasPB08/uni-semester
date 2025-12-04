-- Tests for Week 2: List Comprehensions
-- Run with: ghci tests_list_comprehensions.hs
-- Then: runAllTests

module TestsListComprehensions where

import Exercises_List_Comprehensions

-- ============================================================================
-- Test Framework
-- ============================================================================

type TestResult = (String, Bool)

test :: (Eq a, Show a) => String -> a -> a -> TestResult
test name expected actual = (name, expected == actual)

runTest :: TestResult -> IO ()
runTest (name, passed) = do
  let status = if passed then "[PASS]" else "[FAIL]"
  putStrLn $ status ++ " " ++ name

runAllTests :: IO ()
runAllTests = do
  putStrLn "========================================"
  putStrLn "Week 2: List Comprehensions - Tests"
  putStrLn "========================================"
  putStrLn ""

  putStrLn "--- Exercise 1: doubleEvens ---"
  mapM_ runTest
    [ test "doubleEvens [1,2,3,4,5,6]" [4,8,12] (doubleEvens [1,2,3,4,5,6])
    , test "doubleEvens [1,3,5]" [] (doubleEvens [1,3,5])
    , test "doubleEvens [2,4,6]" [4,8,12] (doubleEvens [2,4,6])
    , test "doubleEvens []" [] (doubleEvens [])
    ]
  putStrLn ""

  putStrLn "--- Exercise 2: squaresUpTo ---"
  mapM_ runTest
    [ test "squaresUpTo 5" [1,4,9,16,25] (squaresUpTo 5)
    , test "squaresUpTo 3" [1,4,9] (squaresUpTo 3)
    , test "squaresUpTo 1" [1] (squaresUpTo 1)
    , test "squaresUpTo 0" [] (squaresUpTo 0)
    ]
  putStrLn ""

  putStrLn "--- Exercise 3: factors ---"
  mapM_ runTest
    [ test "factors 12" [1,2,3,4,6,12] (factors 12)
    , test "factors 7" [1,7] (factors 7)
    , test "factors 1" [1] (factors 1)
    , test "factors 24" [1,2,3,4,6,8,12,24] (factors 24)
    ]
  putStrLn ""

  putStrLn "--- Exercise 4: grid ---"
  mapM_ runTest
    [ test "grid 2 3" [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)] (grid 2 3)
    , test "grid 3 2" [(0,0),(0,1),(1,0),(1,1),(2,0),(2,1)] (grid 3 2)
    , test "grid 1 1" [(0,0)] (grid 1 1)
    , test "grid 0 5" [] (grid 0 5)
    ]
  putStrLn ""

  putStrLn "--- Exercise 5: pairsSumTo ---"
  mapM_ runTest
    [ test "pairsSumTo 5" [(1,4),(2,3)] (pairsSumTo 5)
    , test "pairsSumTo 10" [(1,9),(2,8),(3,7),(4,6),(5,5)] (pairsSumTo 10)
    , test "pairsSumTo 3" [(1,2)] (pairsSumTo 3)
    , test "pairsSumTo 2" [(1,1)] (pairsSumTo 2)
    ]
  putStrLn ""

  putStrLn "--- Exercise 6: concat' ---"
  mapM_ runTest
    [ test "concat' [[1,2], [3,4,5], [6]]" [1,2,3,4,5,6] (concat' [[1,2], [3,4,5], [6]])
    , test "concat' [[], [1], [], [2,3]]" [1,2,3] (concat' [[], [1], [], [2,3]])
    , test "concat' []" ([] :: [Int]) (concat' [])
    , test "concat' [[1,2,3]]" [1,2,3] (concat' [[1,2,3]])
    ]
  putStrLn ""

  putStrLn "--- Exercise 7: pyths ---"
  mapM_ runTest
    [ test "pyths 5" [(3,4,5)] (pyths 5)
    , test "pyths 10" [(3,4,5),(6,8,10)] (pyths 10)
    , test "pyths 15" [(3,4,5),(5,12,13),(6,8,10),(9,12,15)] (pyths 15)
    , test "pyths 2" [] (pyths 2)
    ]
  putStrLn ""

  putStrLn "--- Exercise 8: perfects ---"
  mapM_ runTest
    [ test "perfects 30" [6,28] (perfects 30)
    , test "perfects 500" [6,28,496] (perfects 500)
    , test "perfects 5" [] (perfects 5)
    ]
  putStrLn ""

  putStrLn "--- Exercise 9: positions ---"
  mapM_ runTest
    [ test "positions 3 [1,2,3,4,3,5,3]" [2,4,6] (positions 3 [1,2,3,4,3,5,3])
    , test "positions 'a' \"banana\"" [1,3,5] (positions 'a' "banana")
    , test "positions 5 [1,2,3,4]" [] (positions 5 [1,2,3,4])
    , test "positions 1 [1,1,1]" [0,1,2] (positions 1 [1,1,1])
    ]
  putStrLn ""

  putStrLn "--- Exercise 10: scalarProduct ---"
  mapM_ runTest
    [ test "scalarProduct [1,2,3] [4,5,6]" 32 (scalarProduct [1,2,3] [4,5,6])
    , test "scalarProduct [1,0,1] [1,1,1]" 2 (scalarProduct [1,0,1] [1,1,1])
    , test "scalarProduct [] []" 0 (scalarProduct [] [])
    , test "scalarProduct [2,3] [4,5]" 23 (scalarProduct [2,3] [4,5])
    ]
  putStrLn ""

  putStrLn "--- BONUS Exercise 11: qsort ---"
  mapM_ runTest
    [ test "qsort [3,1,4,1,5,9,2]" [1,1,2,3,4,5,9] (qsort [3,1,4,1,5,9,2])
    , test "qsort \"haskell\"" "aehklls" (qsort "haskell")
    , test "qsort []" ([] :: [Int]) (qsort [])
    , test "qsort [1]" [1] (qsort [1])
    ]
  putStrLn ""

  putStrLn "--- BONUS Exercise 12: replicateEach ---"
  mapM_ runTest
    [ test "replicateEach 3 [1,2,3]" [1,1,1,2,2,2,3,3,3] (replicateEach 3 [1,2,3])
    , test "replicateEach 2 \"ab\"" "aabb" (replicateEach 2 "ab")
    , test "replicateEach 0 [1,2,3]" [] (replicateEach 0 [1,2,3])
    , test "replicateEach 1 [5,6]" [5,6] (replicateEach 1 [5,6])
    ]
  putStrLn ""

  putStrLn "========================================"
  putStrLn "Tests complete!"
  putStrLn "========================================"


-- ============================================================================
-- Quick test for individual exercises
-- ============================================================================

testDoubleEvens :: IO ()
testDoubleEvens = do
  putStrLn "Testing doubleEvens..."
  mapM_ runTest
    [ test "doubleEvens [1,2,3,4,5,6]" [4,8,12] (doubleEvens [1,2,3,4,5,6])
    , test "doubleEvens [1,3,5]" [] (doubleEvens [1,3,5])
    ]

testFactors :: IO ()
testFactors = do
  putStrLn "Testing factors..."
  mapM_ runTest
    [ test "factors 12" [1,2,3,4,6,12] (factors 12)
    , test "factors 7" [1,7] (factors 7)
    ]

testPyths :: IO ()
testPyths = do
  putStrLn "Testing pyths..."
  mapM_ runTest
    [ test "pyths 5" [(3,4,5)] (pyths 5)
    , test "pyths 15" [(3,4,5),(5,12,13),(6,8,10),(9,12,15)] (pyths 15)
    ]
