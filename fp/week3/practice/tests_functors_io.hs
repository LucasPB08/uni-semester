-- Tests for Week 3: Functors and IO
-- Run with: ghci tests_functors_io.hs
-- Then: runAllTests

module Tests_Functors_IO where

import Exercises_Functors_IO
import Data.Char (toUpper)

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
  putStrLn "Week 3: Functors and IO - Tests"
  putStrLn "========================================"
  putStrLn ""

  putStrLn "--- Exercise 1: Functor Box ---"
  mapM_ runTest
    [ test "fmap (*2) (Box 5)" (Box 10) (fmap (*2) (Box 5))
    , test "fmap show (Box 42)" (Box "42") (fmap show (Box 42))
    , test "fmap length (Box \"hello\")" (Box 5) (fmap length (Box "hello"))
    , test "fmap (+1) (Box 0)" (Box 1) (fmap (+1) (Box 0))
    ]
  putStrLn ""

  putStrLn "--- Exercise 2: Functor Pair ---"
  mapM_ runTest
    [ test "fmap (*2) (Pair 3 5)" (Pair 6 10) (fmap (*2) (Pair 3 5))
    , test "fmap toUpper (Pair 'a' 'b')" (Pair 'A' 'B') (fmap toUpper (Pair 'a' 'b'))
    , test "fmap length (Pair \"hi\" \"hello\")" (Pair 2 5) (fmap length (Pair "hi" "hello"))
    , test "fmap (+10) (Pair 1 2)" (Pair 11 12) (fmap (+10) (Pair 1 2))
    ]
  putStrLn ""

  putStrLn "--- Exercise 3: Functor Result ---"
  mapM_ runTest
    [ test "fmap (*2) (Success 5)" (Success 10) (fmap (*2) (Success 5))
    , test "fmap (*2) (Failure \"error\")" (Failure "error" :: Result Int) (fmap (*2) (Failure "error" :: Result Int))
    , test "fmap show (Success 42)" (Success "42") (fmap show (Success 42))
    , test "fmap length (Failure \"no\")" (Failure "no" :: Result Int) (fmap length (Failure "no" :: Result String))
    , test "fmap (+1) (Success 0)" (Success 1) (fmap (+1) (Success 0))
    ]
  putStrLn ""

  putStrLn "--- Exercise 4: Functor Tree ---"
  mapM_ runTest
    [ test "fmap (*2) Empty" (Empty :: Tree Int) (fmap (*2) Empty)
    , test "fmap (*2) (Node 5 Empty Empty)" (Node 10 Empty Empty) (fmap (*2) (Node 5 Empty Empty))
    , test "fmap (*2) tree with children"
        (Node 10 (Node 6 Empty Empty) (Node 16 Empty Empty))
        (fmap (*2) (Node 5 (Node 3 Empty Empty) (Node 8 Empty Empty)))
    , test "fmap length (Node \"hi\" Empty Empty)" (Node 2 Empty Empty) (fmap length (Node "hi" Empty Empty))
    ]
  putStrLn ""

  putStrLn "--- Exercise 5: Using fmap ---"
  mapM_ runTest
    [ test "incrementAll [1,2,3]" [2,3,4] (incrementAll [1,2,3])
    , test "incrementAll []" ([] :: [Int]) (incrementAll [])
    , test "doubleIfPresent (Just 5)" (Just 10) (doubleIfPresent (Just 5))
    , test "doubleIfPresent Nothing" (Nothing :: Maybe Int) (doubleIfPresent Nothing)
    , test "uppercaseResult (Success \"hi\")" (Success "HI") (uppercaseResult (Success "hi"))
    , test "uppercaseResult (Failure \"x\")" (Failure "x" :: Result String) (uppercaseResult (Failure "x"))
    ]
  putStrLn ""

  putStrLn "--- Exercise 6: Functor Laws ---"
  mapM_ runTest
    [ test "Identity law for Box 5" True (checkIdentityBox (Box 5))
    , test "Identity law for Box \"hello\"" True (checkIdentityBox (Box "hello"))
    , test "Composition law for Just 3" True (checkCompositionMaybe (Just 3))
    , test "Composition law for Nothing" True (checkCompositionMaybe Nothing)
    , test "Composition law for Just 0" True (checkCompositionMaybe (Just 0))
    ]
  putStrLn ""

  putStrLn "========================================"
  putStrLn "Functor tests complete!"
  putStrLn ""
  putStrLn "IO tests must be run manually:"
  putStrLn "  greet           -- test greeting"
  putStrLn "  shoutBack       -- test uppercase echo"
  putStrLn "  askAndDouble    -- test number doubling"
  putStrLn "  askFullName     -- test full name"
  putStrLn "  askContinue     -- test yes/no"
  putStrLn "  sumNumbers      -- test number sum"
  putStrLn "  collectUntilDone -- test list collection"
  putStrLn "========================================"


-- ============================================================================
-- Individual test functions
-- ============================================================================

testFunctorBox :: IO ()
testFunctorBox = do
  putStrLn "Testing Functor Box..."
  mapM_ runTest
    [ test "fmap (*2) (Box 5)" (Box 10) (fmap (*2) (Box 5))
    , test "fmap show (Box 42)" (Box "42") (fmap show (Box 42))
    ]

testFunctorPair :: IO ()
testFunctorPair = do
  putStrLn "Testing Functor Pair..."
  mapM_ runTest
    [ test "fmap (*2) (Pair 3 5)" (Pair 6 10) (fmap (*2) (Pair 3 5))
    , test "fmap toUpper (Pair 'a' 'b')" (Pair 'A' 'B') (fmap toUpper (Pair 'a' 'b'))
    ]

testFunctorResult :: IO ()
testFunctorResult = do
  putStrLn "Testing Functor Result..."
  mapM_ runTest
    [ test "fmap (*2) (Success 5)" (Success 10) (fmap (*2) (Success 5))
    , test "fmap (*2) (Failure \"error\")" (Failure "error" :: Result Int) (fmap (*2) (Failure "error" :: Result Int))
    ]

testFunctorTree :: IO ()
testFunctorTree = do
  putStrLn "Testing Functor Tree..."
  mapM_ runTest
    [ test "fmap (*2) Empty" (Empty :: Tree Int) (fmap (*2) Empty)
    , test "fmap (*2) (Node 5 Empty Empty)" (Node 10 Empty Empty) (fmap (*2) (Node 5 Empty Empty))
    ]
