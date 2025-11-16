-- Week 2 Advanced Tests
-- Load this file in GHCi and run `runAllTests`

import Data.Char (toUpper)
import Data.List (sort, intercalate)

sumList :: [Int] -> Int
sumList = foldr (+) 0

lengthList :: [a] -> Int
lengthList = foldr (\x y -> 1 + y) 0

mapList :: (a -> b) -> [a] -> [b]
mapList fun = foldr (\el acc -> fun el : acc) []

filterList :: (a -> Bool) -> [a] -> [a]
filterList pred = foldr (\el acc -> if pred el then el : acc else acc) []

reverseList :: [a] -> [a]
reverseList = foldl (\acc el -> el : acc) []


maximumList :: [Int] -> Int
maximumList (x:xs) = foldl max x xs

-- Test Helper Functions
testCase :: (Eq a, Show a) => String -> a -> a -> IO ()
testCase name expected actual =
  if expected == actual
    then putStrLn $ "  [PASS] " ++ name
    else putStrLn $ "  [FAIL] " ++ name ++ "\n    Expected: " ++ show expected ++ "\n    Got: " ++ show actual

testHeader :: String -> IO ()
testHeader name = putStrLn $ "\n" ++ name ++ ":"

-- ============================================================================
-- Test Suite
-- ============================================================================

testSumList :: IO ()
testSumList = do
  testHeader "Exercise 1: sumList"
  testCase "sumList [1,2,3,4]" 10 (sumList [1,2,3,4])
  testCase "sumList []" 0 (sumList [])
  testCase "sumList [5]" 5 (sumList [5])
  testCase "sumList [-1,1]" 0 (sumList [-1,1])
  testCase "sumList [10,20,30]" 60 (sumList [10,20,30])

testLengthList :: IO ()
testLengthList = do
  testHeader "Exercise 2: lengthList"
  testCase "lengthList [1,2,3]" 3 (lengthList [1,2,3])
  testCase "lengthList []" 0 (lengthList ([] :: [Int]))
  testCase "lengthList \"hello\"" 5 (lengthList "hello")
  testCase "lengthList [1]" 1 (lengthList [1])
  testCase "lengthList [1..100]" 100 (lengthList [1..100])

testMapList :: IO ()
testMapList = do
  testHeader "Exercise 3: mapList"
  testCase "mapList (*2) [1,2,3]" [2,4,6] (mapList (*2) [1,2,3])
  testCase "mapList (+10) [1,2,3]" [11,12,13] (mapList (+10) [1,2,3])
  testCase "mapList reverse [\"ab\",\"cd\"]" ["ba","dc"] (mapList reverse ["ab","cd"])
  testCase "mapList (^2) []" [] (mapList (^2) ([] :: [Int]))
  testCase "mapList not [True,False,True]" [False,True,False] (mapList not [True,False,True])

testFilterList :: IO ()
testFilterList = do
  testHeader "Exercise 4: filterList"
  testCase "filterList even [1,2,3,4]" [2,4] (filterList even [1,2,3,4])
  testCase "filterList (> 5) [3,6,2,8,1]" [6,8] (filterList (> 5) [3,6,2,8,1])
  testCase "filterList (/= ' ') \"a b c\"" "abc" (filterList (/= ' ') "a b c")
  testCase "filterList (const True) [1,2,3]" [1,2,3] (filterList (const True) [1,2,3])
  testCase "filterList (const False) [1,2,3]" [] (filterList (const False) [1,2,3])

testReverseList :: IO ()
testReverseList = do
  testHeader "Exercise 5: reverseList"
  testCase "reverseList [1,2,3]" [3,2,1] (reverseList [1,2,3])
  testCase "reverseList \"hello\"" "olleh" (reverseList "hello")
  testCase "reverseList []" ([] :: [Int]) (reverseList [])
  testCase "reverseList [1]" [1] (reverseList [1])
  testCase "reverseList [1,2,3,4,5]" [5,4,3,2,1] (reverseList [1,2,3,4,5])

testMaximumList :: IO ()
testMaximumList = do
  testHeader "Exercise 6: maximumList"
  testCase "maximumList [3,7,2,9,1]" 9 (maximumList [3,7,2,9,1])
  testCase "maximumList [5]" 5 (maximumList [5])
  testCase "maximumList [-1,-5,-3]" (-1) (maximumList [-1,-5,-3])
  testCase "maximumList [1,2,3,4,5]" 5 (maximumList [1,2,3,4,5])
  testCase "maximumList [10,10,10]" 10 (maximumList [10,10,10])

testConcatLists :: IO ()
testConcatLists = do
  testHeader "Exercise 7: concatLists"
  testCase "concatLists [[1,2],[3,4],[5]]" [1,2,3,4,5] (concatLists [[1,2],[3,4],[5]])
  testCase "concatLists [[],[1],[],[2,3]]" [1,2,3] (concatLists [[],[1],[],[2,3]])
  testCase "concatLists [\"hello\",\"world\"]" "helloworld" (concatLists ["hello","world"])
  testCase "concatLists []" ([] :: [Int]) (concatLists [])
  testCase "concatLists [[1]]" [1] (concatLists [[1]])

testSumOfSquaresOfEvens :: IO ()
testSumOfSquaresOfEvens = do
  testHeader "Exercise 8: sumOfSquaresOfEvens"
  testCase "sumOfSquaresOfEvens [1,2,3,4,5]" 20 (sumOfSquaresOfEvens [1,2,3,4,5])
  testCase "sumOfSquaresOfEvens [1,3,5]" 0 (sumOfSquaresOfEvens [1,3,5])
  testCase "sumOfSquaresOfEvens [2,4,6]" 56 (sumOfSquaresOfEvens [2,4,6])
  testCase "sumOfSquaresOfEvens []" 0 (sumOfSquaresOfEvens [])
  testCase "sumOfSquaresOfEvens [10]" 100 (sumOfSquaresOfEvens [10])

testAnyList :: IO ()
testAnyList = do
  testHeader "Exercise 9: anyList"
  testCase "anyList even [1,3,5,4,7]" True (anyList even [1,3,5,4,7])
  testCase "anyList (> 10) [1,2,3]" False (anyList (> 10) [1,2,3])
  testCase "anyList (== 'a') \"hello\"" False (anyList (== 'a') "hello")
  testCase "anyList (== 'a') \"have\"" True (anyList (== 'a') "have")
  testCase "anyList (const True) []" False (anyList (const True) ([] :: [Int]))

testAllList :: IO ()
testAllList = do
  testHeader "Exercise 10: allList"
  testCase "allList even [2,4,6,8]" True (allList even [2,4,6,8])
  testCase "allList even [2,4,5,8]" False (allList even [2,4,5,8])
  testCase "allList (> 0) [1,2,3]" True (allList (> 0) [1,2,3])
  testCase "allList (> 0) [1,-2,3]" False (allList (> 0) [1,-2,3])
  testCase "allList (const False) []" True (allList (const False) ([] :: [Int]))

testTakeWhileList :: IO ()
testTakeWhileList = do
  testHeader "BONUS Exercise 11: takeWhileList"
  testCase "takeWhileList (< 5) [1,2,3,6,4,2]" [1,2,3] (takeWhileList (< 5) [1,2,3,6,4,2])
  testCase "takeWhileList even [2,4,6,1,8]" [2,4,6] (takeWhileList even [2,4,6,1,8])
  testCase "takeWhileList (/= ' ') \"hello world\"" "hello" (takeWhileList (/= ' ') "hello world")
  testCase "takeWhileList (const True) [1,2,3]" [1,2,3] (takeWhileList (const True) [1,2,3])
  testCase "takeWhileList (const False) [1,2,3]" [] (takeWhileList (const False) [1,2,3])

testZipWithList :: IO ()
testZipWithList = do
  testHeader "BONUS Exercise 12: zipWithList"
  testCase "zipWithList (+) [1,2,3] [10,20,30]" [11,22,33] (zipWithList (+) [1,2,3] [10,20,30])
  testCase "zipWithList (*) [1,2,3] [4,5,6]" [4,10,18] (zipWithList (*) [1,2,3] [4,5,6])
  testCase "zipWithList (,) [1,2] ['a','b','c']" [(1,'a'),(2,'b')] (zipWithList (,) [1,2] ['a','b','c'])
  testCase "zipWithList max [1,5,3] [4,2,6]" [4,5,6] (zipWithList max [1,5,3] [4,2,6])
  testCase "zipWithList (+) [] [1,2,3]" [] (zipWithList (+) [] [1,2,3])

testProcessWords :: IO ()
testProcessWords = do
  testHeader "BONUS Exercise 13: processWords"
  testCase "processWords [\"hi\",\"hello\",\"world\",\"bye\"]" "HELLO WORLD" (processWords ["hi","hello","world","bye"])
  testCase "processWords [\"a\",\"ab\",\"abc\",\"abcd\"]" "ABCD" (processWords ["a","ab","abc","abcd"])
  testCase "processWords [\"test\",\"data\"]" "DATA TEST" (processWords ["test","data"])
  testCase "processWords [\"hi\",\"yo\"]" "" (processWords ["hi","yo"])

testCountEvenOdd :: IO ()
testCountEvenOdd = do
  testHeader "BONUS Exercise 14: countEvenOdd"
  testCase "countEvenOdd [1,2,3,4,5,6]" (3,3) (countEvenOdd [1,2,3,4,5,6])
  testCase "countEvenOdd [2,4,6]" (3,0) (countEvenOdd [2,4,6])
  testCase "countEvenOdd [1,3,5]" (0,3) (countEvenOdd [1,3,5])
  testCase "countEvenOdd []" (0,0) (countEvenOdd [])
  testCase "countEvenOdd [10]" (1,0) (countEvenOdd [10])

testGroupConsecutive :: IO ()
testGroupConsecutive = do
  testHeader "BONUS Exercise 15: groupConsecutive"
  testCase "groupConsecutive [1,1,2,2,2,3,1,1]" [[1,1],[2,2,2],[3],[1,1]] (groupConsecutive [1,1,2,2,2,3,1,1])
  testCase "groupConsecutive \"aabbcca\"" ["aa","bb","cc","a"] (groupConsecutive "aabbcca")
  testCase "groupConsecutive [1,2,3]" [[1],[2],[3]] (groupConsecutive [1,2,3])
  testCase "groupConsecutive []" ([] :: [[Int]]) (groupConsecutive [])
  testCase "groupConsecutive [1,1,1,1]" [[1,1,1,1]] (groupConsecutive [1,1,1,1])

-- ============================================================================
-- Run All Tests
-- ============================================================================

runAllTests :: IO ()
runAllTests = do
  putStrLn "\n========================================="
  putStrLn "Running Week 2 Advanced Tests"
  putStrLn "=========================================\n"
  testSumList
  testLengthList
  testMapList
  testFilterList
  testReverseList
  testMaximumList
  testConcatLists
  testSumOfSquaresOfEvens
  testAnyList
  testAllList
  putStrLn "\n--- BONUS EXERCISES ---"
  testTakeWhileList
  testZipWithList
  testProcessWords
  testCountEvenOdd
  testGroupConsecutive
  putStrLn "\n========================================="
  putStrLn "Tests Complete!"
  putStrLn "=========================================\n"

-- ============================================================================
-- Usage Instructions
-- ============================================================================

{-
To run these tests:

1. Make sure exercises_advanced.hs is in the same directory

2. Start GHCi:
   ghci

3. Load this test file:
   :load tests_advanced.hs

4. Run all tests:
   runAllTests

5. Or run individual test suites:
   testSumList
   testMapList
   testFilterList
   etc.

6. If you modify your solutions:
   :reload
   runAllTests
-}
