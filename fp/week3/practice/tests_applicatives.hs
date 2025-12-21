-- Tests for Week 3: Applicatives
-- Run with: ghci tests_applicatives.hs
-- Then: runAllTests

module Tests_Applicatives where

import Applicatives

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
  putStrLn "Week 3: Applicatives - Tests"
  putStrLn "========================================"
  putStrLn ""

  putStrLn "--- Exercise 1: Basic Applicative Operations ---"
  mapM_ runTest
    [ test "applyMaybe (Just (+1)) (Just 5)" (Just 6) (applyMaybe (Just (+1)) (Just 5))
    , test "applyMaybe Nothing (Just 5)" (Nothing :: Maybe Int) (applyMaybe Nothing (Just 5))
    , test "applyMaybe (Just (*2)) Nothing" (Nothing :: Maybe Int) (applyMaybe (Just (*2)) Nothing)
    , test "applyMaybe Nothing Nothing" (Nothing :: Maybe Int) (applyMaybe (Nothing :: Maybe (Int -> Int)) Nothing)
    , test "addMaybes (Just 3) (Just 5)" (Just 8) (addMaybes (Just 3) (Just 5))
    , test "addMaybes Nothing (Just 5)" Nothing (addMaybes Nothing (Just 5))
    , test "addMaybes (Just 3) Nothing" Nothing (addMaybes (Just 3) Nothing)
    , test "multiplyThree (Just 2) (Just 3) (Just 4)" (Just 24) (multiplyThree (Just 2) (Just 3) (Just 4))
    , test "multiplyThree (Just 2) Nothing (Just 4)" Nothing (multiplyThree (Just 2) Nothing (Just 4))
    , test "multiplyThree Nothing (Just 3) (Just 4)" Nothing (multiplyThree Nothing (Just 3) (Just 4))
    ]
  putStrLn ""

  putStrLn "--- Exercise 2: List Applicative ---"
  mapM_ runTest
    [ test "applyList [(+1), (*2)] [10, 20]" [11, 21, 20, 40] (applyList [(+1), (*2)] [10, 20])
    , test "applyList [] [1,2,3]" ([] :: [Int]) (applyList [] [1,2,3])
    , test "applyList [(+1)] []" ([] :: [Int]) (applyList [(+1)] [])
    , test "applyList [(*2)] [5]" [10] (applyList [(*2)] [5])
    , test "allPairs [1,2] [\"a\",\"b\"]" [(1,"a"),(1,"b"),(2,"a"),(2,"b")] (allPairs [1,2] ["a","b"])
    , test "allPairs [] [\"a\"]" ([] :: [(Int, String)]) (allPairs [] ["a"])
    , test "allPairs [1] []" ([] :: [(Int, String)]) (allPairs [1] [])
    , test "allSums [1,2] [10,20]" [11,21,12,22] (allSums [1,2] [10,20])
    , test "allSums [0] [5,6,7]" [5,6,7] (allSums [0] [5,6,7])
    , test "multiplicationTable 3" [1,2,3,2,4,6,3,6,9] (multiplicationTable 3)
    , test "multiplicationTable 2" [1,2,2,4] (multiplicationTable 2)
    , test "multiplicationTable 1" [1] (multiplicationTable 1)
    ]
  putStrLn ""

  putStrLn "--- Exercise 3: Combining with Applicative ---"
  mapM_ runTest
    [ test "makePerson (Just \"Alice\") (Just 30)" (Just (Person "Alice" 30)) (makePerson (Just "Alice") (Just 30))
    , test "makePerson Nothing (Just 30)" Nothing (makePerson Nothing (Just 30))
    , test "makePerson (Just \"Bob\") Nothing" Nothing (makePerson (Just "Bob") Nothing)
    , test "makePoint (Just 3) (Just 4)" (Just (Point 3 4)) (makePoint (Just 3) (Just 4))
    , test "makePoint Nothing (Just 4)" Nothing (makePoint Nothing (Just 4))
    , test "makePoint (Just 3) Nothing" Nothing (makePoint (Just 3) Nothing)
    , test "distanceSquared (Just (Point 3 4))" (Just 25) (distanceSquared (Just (Point 3 4)))
    , test "distanceSquared (Just (Point 0 0))" (Just 0) (distanceSquared (Just (Point 0 0)))
    , test "distanceSquared (Just (Point 1 1))" (Just 2) (distanceSquared (Just (Point 1 1)))
    , test "distanceSquared Nothing" Nothing (distanceSquared Nothing)
    ]
  putStrLn ""

  putStrLn "--- Exercise 4: Validation with Applicative ---"
  mapM_ runTest
    [ test "validateNonEmpty \"\"" Nothing (validateNonEmpty "")
    , test "validateNonEmpty \"hello\"" (Just "hello") (validateNonEmpty "hello")
    , test "validateNonEmpty \"a\"" (Just "a") (validateNonEmpty "a")
    , test "validatePositive 5" (Just 5) (validatePositive 5)
    , test "validatePositive 0" Nothing (validatePositive 0)
    , test "validatePositive (-3)" Nothing (validatePositive (-3))
    , test "validatePositive 1" (Just 1) (validatePositive 1)
    , test "validateRange 1 10 5" (Just 5) (validateRange 1 10 5)
    , test "validateRange 1 10 0" Nothing (validateRange 1 10 0)
    , test "validateRange 1 10 11" Nothing (validateRange 1 10 11)
    , test "validateRange 1 10 1" (Just 1) (validateRange 1 10 1)
    , test "validateRange 1 10 10" (Just 10) (validateRange 1 10 10)
    , test "validateUser \"alice\" 25 85" (Just (UserInput "alice" 25 85)) (validateUser "alice" 25 85)
    , test "validateUser \"\" 25 85" Nothing (validateUser "" 25 85)
    , test "validateUser \"alice\" (-1) 85" Nothing (validateUser "alice" (-1) 85)
    , test "validateUser \"alice\" 25 101" Nothing (validateUser "alice" 25 101)
    , test "validateUser \"alice\" 25 (-1)" Nothing (validateUser "alice" 25 (-1))
    , test "validateUser \"bob\" 1 0" (Just (UserInput "bob" 1 0)) (validateUser "bob" 1 0)
    , test "validateUser \"bob\" 1 100" (Just (UserInput "bob" 1 100)) (validateUser "bob" 1 100)
    ]
  putStrLn ""

  putStrLn "--- Exercise 5: Applicative with Either ---"
  mapM_ runTest
    [ test "safeDiv 10 2" (Right 5) (safeDiv 10 2)
    , test "safeDiv 10 0" (Left "Division by zero") (safeDiv 10 0)
    , test "safeDiv 0 5" (Right 0) (safeDiv 0 5)
    , test "safeDiv 7 2" (Right 3) (safeDiv 7 2)
    , test "addEithers (Right 3) (Right 5)" (Right 8) (addEithers (Right 3) (Right 5))
    , test "addEithers (Left \"error\") (Right 5)" (Left "error") (addEithers (Left "error") (Right 5))
    , test "addEithers (Right 3) (Left \"oops\")" (Left "oops") (addEithers (Right 3) (Left "oops"))
    , test "addEithers (Left \"first\") (Left \"second\")" (Left "first") (addEithers (Left "first") (Left "second"))
    , test "combineBoth (Right 1) (Right \"a\")" (Right (1, "a") :: Either String (Int, String)) (combineBoth (Right 1) (Right "a"))
    , test "combineBoth (Left \"err\") (Right \"a\")" (Left "err" :: Either String (Int, String)) (combineBoth (Left "err") (Right "a"))
    , test "combineBoth (Right 1) (Left \"err\")" (Left "err" :: Either String (Int, String)) (combineBoth (Right 1) (Left "err"))
    ]
  putStrLn ""

  putStrLn "--- Exercise 6: Applicative for Custom Types ---"
  mapM_ runTest
    [ test "fmap (*2) (Box 5)" (Box 10) (fmap (*2) (Box 5))
    , test "fmap show (Box 42)" (Box "42") (fmap show (Box 42))
    , test "pure 5 :: Box Int" (Box 5) (pure 5 :: Box Int)
    , test "pure \"hello\" :: Box String" (Box "hello") (pure "hello" :: Box String)
    , test "Box (+1) <*> Box 5" (Box 6) (Box (+1) <*> Box 5)
    , test "Box (*2) <*> Box 10" (Box 20) (Box (*2) <*> Box 10)
    , test "addBoxes (Box 3) (Box 5)" (Box 8) (addBoxes (Box 3) (Box 5))
    , test "addBoxes (Box 0) (Box 0)" (Box 0) (addBoxes (Box 0) (Box 0))
    , test "(+) <$> Box 3 <*> Box 4" (Box 7) ((+) <$> Box 3 <*> Box 4)
    ]
  putStrLn ""

  putStrLn "--- Exercise 7: Using liftA2 and liftA3 ---"
  mapM_ runTest
    [ test "addMaybesLift (Just 3) (Just 5)" (Just 8) (addMaybesLift (Just 3) (Just 5))
    , test "addMaybesLift Nothing (Just 5)" Nothing (addMaybesLift Nothing (Just 5))
    , test "addMaybesLift (Just 3) Nothing" Nothing (addMaybesLift (Just 3) Nothing)
    , test "concatMaybes (Just \"Hello, \") (Just \"World\")" (Just "Hello, World") (concatMaybes (Just "Hello, ") (Just "World"))
    , test "concatMaybes Nothing (Just \"World\")" Nothing (concatMaybes Nothing (Just "World"))
    , test "concatMaybes (Just \"Hello\") Nothing" Nothing (concatMaybes (Just "Hello") Nothing)
    , test "makeTriple (Just 1) (Just 2) (Just 3)" (Just (1, 2, 3)) (makeTriple (Just 1) (Just 2) (Just 3))
    , test "makeTriple Nothing (Just 2) (Just 3)" (Nothing :: Maybe (Int, Int, Int)) (makeTriple Nothing (Just 2) (Just 3))
    , test "makeTriple (Just 1) Nothing (Just 3)" (Nothing :: Maybe (Int, Int, Int)) (makeTriple (Just 1) Nothing (Just 3))
    , test "makeTriple (Just 1) (Just 2) Nothing" (Nothing :: Maybe (Int, Int, Int)) (makeTriple (Just 1) (Just 2) Nothing)
    ]
  putStrLn ""

  putStrLn "--- Exercise 8: Sequencing with *> and <* ---"
  mapM_ runTest
    [ test "keepFirst (Just 1) (Just 2)" (Just 1) (keepFirst (Just 1) (Just 2))
    , test "keepFirst Nothing (Just 2)" (Nothing :: Maybe Int) (keepFirst Nothing (Just 2))
    , test "keepFirst (Just 1) Nothing" (Nothing :: Maybe Int) (keepFirst (Just 1) (Nothing :: Maybe Int))
    , test "keepSecond (Just 1) (Just 2)" (Just 2) (keepSecond (Just 1) (Just 2))
    , test "keepSecond Nothing (Just 2)" (Nothing :: Maybe Int) (keepSecond (Nothing :: Maybe Int) (Just 2))
    , test "keepSecond (Just 1) Nothing" (Nothing :: Maybe Int) (keepSecond (Just 1) Nothing)
    ]
  putStrLn ""

  putStrLn "--- Exercise 9: Practical Application ---"
  mapM_ runTest
    [ test "parseInt \"42\"" (Just 42) (parseInt "42")
    , test "parseInt \"abc\"" Nothing (parseInt "abc")
    , test "parseInt \"\"" Nothing (parseInt "")
    , test "parseInt \"-5\"" (Just (-5)) (parseInt "-5")
    , test "parseInt \"0\"" (Just 0) (parseInt "0")
    , test "parseBool \"true\"" (Just True) (parseBool "true")
    , test "parseBool \"false\"" (Just False) (parseBool "false")
    , test "parseBool \"yes\"" Nothing (parseBool "yes")
    , test "parseBool \"TRUE\"" Nothing (parseBool "TRUE")
    , test "parseBool \"\"" Nothing (parseBool "")
    , test "parseConfig \"localhost\" \"8080\" \"true\"" (Just (Config "localhost" 8080 True)) (parseConfig "localhost" "8080" "true")
    , test "parseConfig \"\" \"8080\" \"true\"" Nothing (parseConfig "" "8080" "true")
    , test "parseConfig \"localhost\" \"abc\" \"true\"" Nothing (parseConfig "localhost" "abc" "true")
    , test "parseConfig \"localhost\" \"8080\" \"yes\"" Nothing (parseConfig "localhost" "8080" "yes")
    , test "parseConfig \"host\" \"80\" \"false\"" (Just (Config "host" 80 False)) (parseConfig "host" "80" "false")
    ]
  putStrLn ""

  putStrLn "--- Exercise 10: ZipList Applicative ---"
  mapM_ runTest
    [ test "ZipList [(+1), (*2)] <*> ZipList [10, 20]" (ZipList [11, 40]) (ZipList [(+1), (*2)] <*> ZipList [10, 20])
    , test "ZipList [(+)] <*> ZipList [1,2,3] <*> ZipList [10,20,30]" (ZipList [11,22,33]) (ZipList [(+)] <*> ZipList [1,2,3] <*> ZipList [10,20,30])
    , test "zipAdd (ZipList [1,2,3]) (ZipList [10,20,30])" (ZipList [11,22,33]) (zipAdd (ZipList [1,2,3]) (ZipList [10,20,30]))
    , test "zipAdd (ZipList [1,2]) (ZipList [10,20,30])" (ZipList [11,22]) (zipAdd (ZipList [1,2]) (ZipList [10,20,30]))
    , test "ZipList [(*2)] <*> ZipList [1,2,3]" (ZipList [2,4,6]) (ZipList [(*2)] <*> ZipList [1,2,3])
    , test "pure (*2) <*> ZipList [1,2,3]" (ZipList [2,4,6]) (pure (*2) <*> ZipList [1,2,3])
    ]
  putStrLn ""

  putStrLn "========================================"
  putStrLn "All Applicatives tests complete!"
  putStrLn "========================================"
