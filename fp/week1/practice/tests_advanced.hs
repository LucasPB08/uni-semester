-- Test file for Week 1 Advanced exercises
-- No libraries needed - just load and run!

{-
  HOW TO USE:

  1. Implement your functions in exercises_advanced.hs
  2. Copy them to this file (replace undefined)
  3. Load this file in GHCi:
     :load tests_advanced.hs

  4. Run all tests:
     runAllTests

  5. After making changes:
     :reload
     runAllTests
-}

-- ============================================================================
-- Copy your solutions here
-- ============================================================================

-- Exercise 1: swapPair
swapPair :: (a, b) -> (b, a)
swapPair (x, y) = (y ,x)

-- Exercise 2: zipLists
zipLists :: [a] -> [b] -> [(a, b)]
zipLists _ [] = [] 
zipLists [] _ = []
zipLists (x : xs) (y : ys) = (x , y) : zipLists xs ys

-- Exercise 3: takeN
takeN :: Int -> [a] -> [a]
takeN _ [] = []
takeN n (x : xs) 
  | n <= 0 = []
  | otherwise = x : takeN (n-1) xs

-- Exercise 4: reverseList
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x : xs) = reverseList(xs) ++ [x]

-- Exercise 5: dropN
dropN :: Int -> [a] -> [a]
dropN _ [] = []
dropN n (x : xs) 
  | n <= 0 = x : xs
  | otherwise = drop (n-1) xs 

-- Exercise 6: unzipPairs
unzipPairs :: [(a, b)] -> ([a], [b])
unzipPairs [] = ([], [])
unzipPairs ((x, y) : rest) = (x : xs, y : ys)
  where (xs, ys) = unzipPairs rest

-- Exercise 7: mapPair
mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair fun1 fun2 (x, y) = (fun1 x, fun2 y)

-- Exercise 8: removeFirst
removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst el (x : xs) 
  | el == x = xs 
  | otherwise = x : removeFirst el xs 

-- Exercise 9: partitionBy
partitionBy :: (a -> Bool) -> [a] -> ([a], [a])
partitionBy fun [] = ([], [])
partitionBy fun (x : rest) 
  | fun x = (x : xs, ys) 
  | otherwise = (xs, x : ys) 
    where (xs, ys) = partitionBy fun rest

-- ============================================================================
-- Test Utilities
-- ============================================================================

runTest :: String -> Bool -> IO ()
runTest name result =
  putStrLn $ name ++ ": " ++ if result then "[PASS]" else "[FAIL]"

-- ============================================================================
-- Test Suites
-- ============================================================================

testSwapPair :: IO ()
testSwapPair = do
  putStrLn "\n--- Testing swapPair ---"
  runTest "swapPair (1, 2) == (2, 1)" (swapPair (1, 2) == (2, 1))
  runTest "swapPair (\"hello\", 5) == (5, \"hello\")" (swapPair ("hello", 5) == (5, "hello"))
  runTest "swapPair (True, 'a') == ('a', True)" (swapPair (True, 'a') == ('a', True))

testZipLists :: IO ()
testZipLists = do
  putStrLn "\n--- Testing zipLists ---"
  runTest "zipLists [1,2,3] ['a','b','c'] == [(1,'a'),(2,'b'),(3,'c')]"
    (zipLists [1,2,3] ['a','b','c'] == [(1,'a'),(2,'b'),(3,'c')])
  runTest "zipLists [1,2] ['a','b','c','d'] == [(1,'a'),(2,'b')]"
    (zipLists [1,2] ['a','b','c','d'] == [(1,'a'),(2,'b')])
  runTest "zipLists [1,2,3] [] == []"
    (zipLists [1,2,3] ([] :: [Char]) == [])
  runTest "zipLists [] [1,2,3] == []"
    (zipLists ([] :: [Int]) [1,2,3] == [])

testTakeN :: IO ()
testTakeN = do
  putStrLn "\n--- Testing takeN ---"
  runTest "takeN 3 [1,2,3,4,5] == [1,2,3]" (takeN 3 [1,2,3,4,5] == [1,2,3])
  runTest "takeN 2 ['a','b'] == ['a','b']" (takeN 2 ['a','b'] == ['a','b'])
  runTest "takeN 5 [1,2] == [1,2]" (takeN 5 [1,2] == [1,2])
  runTest "takeN 0 [1,2,3] == []" (takeN 0 [1,2,3] == [])
  runTest "takeN (-1) [1,2,3] == []" (takeN (-1) [1,2,3] == [])

testReverseList :: IO ()
testReverseList = do
  putStrLn "\n--- Testing reverseList ---"
  runTest "reverseList [1,2,3] == [3,2,1]" (reverseList [1,2,3] == [3,2,1])
  runTest "reverseList \"hello\" == \"olleh\"" (reverseList "hello" == "olleh")
  runTest "reverseList [1] == [1]" (reverseList [1] == [1])
  runTest "reverseList [] == []" (reverseList ([] :: [Int]) == [])

testDropN :: IO ()
testDropN = do
  putStrLn "\n--- Testing dropN ---"
  runTest "dropN 2 [1,2,3,4,5] == [3,4,5]" (dropN 2 [1,2,3,4,5] == [3,4,5])
  runTest "dropN 3 [1,2] == []" (dropN 3 [1,2] == [])
  runTest "dropN 0 [1,2,3] == [1,2,3]" (dropN 0 [1,2,3] == [1,2,3])
  runTest "dropN 5 [1,2,3] == []" (dropN 5 [1,2,3] == [])
  runTest "dropN (-1) [1,2,3] == [1,2,3]" (dropN (-1) [1,2,3] == [1,2,3])

testUnzipPairs :: IO ()
testUnzipPairs = do
  putStrLn "\n--- Testing unzipPairs ---"
  runTest "unzipPairs [(1,'a'),(2,'b'),(3,'c')] == ([1,2,3], ['a','b','c'])"
    (unzipPairs [(1,'a'),(2,'b'),(3,'c')] == ([1,2,3], ['a','b','c']))
  runTest "unzipPairs [(True, 0), (False, 1)] == ([True,False], [0,1])"
    (unzipPairs [(True, 0), (False, 1)] == ([True,False], [0,1]))
  runTest "unzipPairs [] == ([], [])"
    (unzipPairs ([] :: [(Int, Char)]) == ([], []))

testMapPair :: IO ()
testMapPair = do
  putStrLn "\n--- Testing mapPair ---"
  runTest "mapPair (+1) (*2) (5, 3) == (6, 6)" (mapPair (+1) (*2) (5, 3) == (6, 6))
  runTest "mapPair not length (True, \"hi\") == (False, 2)"
    (mapPair not length (True, "hi") == (False, 2))
  runTest "mapPair (*2) (+10) (3, 5) == (6, 15)" (mapPair (*2) (+10) (3, 5) == (6, 15))

testRemoveFirst :: IO ()
testRemoveFirst = do
  putStrLn "\n--- Testing removeFirst ---"
  runTest "removeFirst 3 [1,2,3,4,3] == [1,2,4,3]" (removeFirst 3 [1,2,3,4,3] == [1,2,4,3])
  runTest "removeFirst 'a' \"banana\" == \"bnana\"" (removeFirst 'a' "banana" == "bnana")
  runTest "removeFirst 5 [1,2,3] == [1,2,3]" (removeFirst 5 [1,2,3] == [1,2,3])
  runTest "removeFirst 1 [] == []" (removeFirst 1 ([] :: [Int]) == [])

testPartitionBy :: IO ()
testPartitionBy = do
  putStrLn "\n--- Testing partitionBy ---"
  runTest "partitionBy (> 5) [1,10,3,8,5,12] == ([10,8,12], [1,3,5])"
    (partitionBy (> 5) [1,10,3,8,5,12] == ([10,8,12], [1,3,5]))
  runTest "partitionBy even [1,2,3,4,5,6] == ([2,4,6], [1,3,5])"
    (partitionBy even [1,2,3,4,5,6] == ([2,4,6], [1,3,5]))
  runTest "partitionBy (== 'a') \"banana\" == (\"aaa\", \"bnn\")"
    (partitionBy (== 'a') "banana" == ("aaa", "bnn"))
  runTest "partitionBy (> 0) [] == ([], [])"
    (partitionBy (> 0) ([] :: [Int]) == ([], []))

-- ============================================================================
-- Run All Tests
-- ============================================================================

runAllTests :: IO ()
runAllTests = do
  putStrLn "========================================"
  putStrLn "  Week 1 Advanced Exercise Tests"
  putStrLn "========================================"

  testSwapPair
  testZipLists
  testTakeN
  testReverseList
  testDropN
  testUnzipPairs
  testMapPair
  testRemoveFirst
  testPartitionBy

  putStrLn "\n========================================"
  putStrLn "  Tests Complete!"
  putStrLn "========================================\n"
