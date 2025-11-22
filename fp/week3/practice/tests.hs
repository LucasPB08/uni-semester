-- Week 3 Tests: Advanced Recursion and Type System Fundamentals
-- Load this file in GHCi after implementing exercises.hs
-- Run: runAllTests

import Exercises

{-
To run tests:
1. In GHCi, simply run:
   :load tests.hs
   runAllTests

2. If you get import errors, make sure exercises.hs has "module Exercises where" at the top
-}

-- ============================================================================
-- Test Data
-- ============================================================================

-- Sample trees for testing
emptyTree :: Tree Int
emptyTree = Empty

singleNode :: Tree Int
singleNode = Node 5 Empty Empty

--      5
--     / \
--    3   8
smallTree :: Tree Int
smallTree = Node 5 (Node 3 Empty Empty) (Node 8 Empty Empty)

--      5
--     / \
--    3   8
--   /
--  1
leftHeavyTree :: Tree Int
leftHeavyTree = Node 5 (Node 3 (Node 1 Empty Empty) Empty) (Node 8 Empty Empty)

--      5
--     / \
--    3   8
--         \
--          10
rightHeavyTree :: Tree Int
rightHeavyTree = Node 5 (Node 3 Empty Empty) (Node 8 Empty (Node 10 Empty Empty))

--        5
--       / \
--      3   8
--     /   / \
--    1   6   10
balancedTree :: Tree Int
balancedTree = Node 5
                    (Node 3 (Node 1 Empty Empty) Empty)
                    (Node 8 (Node 6 Empty Empty) (Node 10 Empty Empty))


-- ============================================================================
-- Test Framework (Simple)
-- ============================================================================

-- Test result type
data TestResult = Pass | Fail String

-- Run a single test
test :: (Eq a, Show a) => String -> a -> a -> IO ()
test name actual expected =
  if actual == expected
  then putStrLn $ "[PASS] " ++ name
  else putStrLn $ "[FAIL] " ++ name ++ "\n  Expected: " ++ show expected ++ "\n  Got: " ++ show actual

-- Section header
section :: String -> IO ()
section name = putStrLn $ "\n=== " ++ name ++ " ==="


-- ============================================================================
-- Exercise 1: Quicksort Tests
-- ============================================================================

testQuicksort :: IO ()
testQuicksort = do
  section "Exercise 1: Quicksort"
  test "quicksort empty list" (quicksort ([] :: [Int])) []
  test "quicksort single element" (quicksort [5]) [5]
  test "quicksort already sorted" (quicksort [1,2,3,4,5]) [1,2,3,4,5]
  test "quicksort reverse sorted" (quicksort [5,4,3,2,1]) [1,2,3,4,5]
  test "quicksort unsorted" (quicksort [3,1,4,1,5,9,2,6]) [1,1,2,3,4,5,6,9]
  test "quicksort duplicates" (quicksort [5,2,5,2,5]) [2,2,5,5,5]
  test "quicksort string" (quicksort "haskell") "aehklls"
  test "quicksort negative" (quicksort [-3,1,-5,2,0]) [-5,-3,0,1,2]


-- ============================================================================
-- Exercise 2: Tree Depth Tests
-- ============================================================================

testTreeDepth :: IO ()
testTreeDepth = do
  section "Exercise 2: Tree Depth"
  test "depth of empty tree" (treeDepth emptyTree) 0
  test "depth of single node" (treeDepth singleNode) 1
  test "depth of small tree" (treeDepth smallTree) 2
  test "depth of left-heavy tree" (treeDepth leftHeavyTree) 3
  test "depth of right-heavy tree" (treeDepth rightHeavyTree) 3
  test "depth of balanced tree" (treeDepth balancedTree) 3


-- ============================================================================
-- Exercise 3: Tree Map Tests
-- ============================================================================

testTreeMap :: IO ()
testTreeMap = do
  section "Exercise 3: Tree Map"
  test "map on empty tree" (treeMap (+1) emptyTree) Empty
  test "map on single node" (treeMap (*2) singleNode) (Node 10 Empty Empty)
  test "map (*2) on small tree"
       (treeMap (*2) smallTree)
       (Node 10 (Node 6 Empty Empty) (Node 16 Empty Empty))
  test "map (+10) on small tree"
       (treeMap (+10) smallTree)
       (Node 15 (Node 13 Empty Empty) (Node 18 Empty Empty))
  test "map negate on small tree"
       (treeMap negate smallTree)
       (Node (-5) (Node (-3) Empty Empty) (Node (-8) Empty Empty))

  -- Test type transformation
  let strTree = Node "hi" (Node "hello" Empty Empty) (Node "bye" Empty Empty)
  test "map length on string tree"
       (treeMap length strTree)
       (Node 2 (Node 5 Empty Empty) (Node 3 Empty Empty))


-- ============================================================================
-- Exercise 4: Flatten Tree Tests
-- ============================================================================

testFlattenTree :: IO ()
testFlattenTree = do
  section "Exercise 4: Flatten Tree"
  test "flatten empty tree" (flattenTree emptyTree) []
  test "flatten single node" (flattenTree singleNode) [5]
  test "flatten small tree" (flattenTree smallTree) [3,5,8]
  test "flatten left-heavy tree" (flattenTree leftHeavyTree) [1,3,5,8]
  test "flatten right-heavy tree" (flattenTree rightHeavyTree) [3,5,8,10]
  test "flatten balanced tree" (flattenTree balancedTree) [1,3,5,6,8,10]


-- ============================================================================
-- Exercise 5: Lookup Tests
-- ============================================================================

testLookup :: IO ()
testLookup = do
  section "Exercise 5: Lookup"
  let phonebook = [("Alice", "555-1234"), ("Bob", "555-5678"), ("Charlie", "555-9999")]
  test "lookup existing key 1" (lookup' "Alice" phonebook) (Just "555-1234")
  test "lookup existing key 2" (lookup' "Bob" phonebook) (Just "555-5678")
  test "lookup existing key 3" (lookup' "Charlie" phonebook) (Just "555-9999")
  test "lookup non-existing key" (lookup' "Dave" phonebook) Nothing
  test "lookup in empty list" (lookup' "Alice" ([] :: [(String, String)])) Nothing

  let numMap = [(1, 'a'), (2, 'b'), (3, 'c')]
  test "lookup number key 1" (lookup' 1 numMap) (Just 'a')
  test "lookup number key 3" (lookup' 3 numMap) (Just 'c')
  test "lookup missing number" (lookup' 5 numMap) Nothing


-- ============================================================================
-- Exercise 6: Filter Maybe Tests
-- ============================================================================

testFilterMaybe :: IO ()
testFilterMaybe = do
  section "Exercise 6: Filter Maybe"
  test "filter mixed list" (filterMaybe [Just 1, Nothing, Just 3, Nothing, Just 5]) [1,3,5]
  test "filter all Nothing" (filterMaybe [Nothing, Nothing, Nothing]) ([] :: [Int])
  test "filter all Just" (filterMaybe [Just 1, Just 2, Just 3]) [1,2,3]
  test "filter empty list" (filterMaybe []) ([] :: [Int])
  test "filter chars" (filterMaybe [Just 'a', Just 'b', Nothing, Just 'c']) "abc"
  test "filter single Nothing" (filterMaybe [Nothing]) ([] :: [Int])
  test "filter single Just" (filterMaybe [Just 42]) [42]


-- ============================================================================
-- Exercise 7: Compare Length Tests
-- ============================================================================

testCompareLength :: IO ()
testCompareLength = do
  section "Exercise 7: Compare Length"
  test "compare equal length" (compareLength [1,2,3] [4,5,6]) EQ
  test "compare first shorter" (compareLength [1,2] [3,4,5]) LT
  test "compare first longer" (compareLength [1,2,3,4] [5,6]) GT
  test "compare both empty" (compareLength [] []) EQ
  test "compare first empty" (compareLength [] [1]) LT
  test "compare second empty" (compareLength [1] []) GT
  test "compare strings equal" (compareLength "hello" "world") EQ
  test "compare strings first shorter" (compareLength "hi" "hello") LT
  test "compare strings first longer" (compareLength "hello" "hi") GT


-- ============================================================================
-- Exercise 8: Tree Contains Tests
-- ============================================================================

testTreeContains :: IO ()
testTreeContains = do
  section "Exercise 8: Tree Contains"
  test "contains in empty tree" (treeContains 5 emptyTree) False
  test "contains in single node (found)" (treeContains 5 singleNode) True
  test "contains in single node (not found)" (treeContains 3 singleNode) False
  test "contains in small tree (root)" (treeContains 5 smallTree) True
  test "contains in small tree (left)" (treeContains 3 smallTree) True
  test "contains in small tree (right)" (treeContains 8 smallTree) True
  test "contains in small tree (not found)" (treeContains 10 smallTree) False
  test "contains in left-heavy tree" (treeContains 1 leftHeavyTree) True
  test "contains in right-heavy tree" (treeContains 10 rightHeavyTree) True
  test "contains in balanced tree" (treeContains 6 balancedTree) True
  test "contains not in balanced tree" (treeContains 7 balancedTree) False


-- ============================================================================
-- BONUS Exercise 9: Tree Insert Tests
-- ============================================================================

testTreeInsert :: IO ()
testTreeInsert = do
  section "BONUS Exercise 9: Tree Insert"
  test "insert into empty" (treeInsert 5 Empty) (Node 5 Empty Empty)

  let t1 = treeInsert 5 Empty
      t2 = treeInsert 3 t1
  test "insert smaller value" t2 (Node 5 (Node 3 Empty Empty) Empty)

  let t3 = treeInsert 8 t2
  test "insert larger value" t3 (Node 5 (Node 3 Empty Empty) (Node 8 Empty Empty))

  -- Build a tree and verify it's a valid BST by checking in-order traversal is sorted
  let t4 = treeInsert 1 t3
      t5 = treeInsert 6 t4
      t6 = treeInsert 10 t5
  test "BST property (in-order is sorted)" (flattenTree t6) [1,3,5,6,8,10]

  -- Test insertion order doesn't matter for final sorted result
  let tree1 = foldr treeInsert Empty [5,3,8,1,6,10]
      tree2 = foldr treeInsert Empty [1,3,5,6,8,10]
      tree3 = foldr treeInsert Empty [10,8,6,5,3,1]
  test "insert order independence 1" (flattenTree tree1) [1,3,5,6,8,10]
  test "insert order independence 2" (flattenTree tree2) [1,3,5,6,8,10]
  test "insert order independence 3" (flattenTree tree3) [1,3,5,6,8,10]


-- ============================================================================
-- BONUS Exercise 10: Merge Sort Tests
-- ============================================================================


-- ============================================================================
-- Run All Tests
-- ============================================================================

runAllTests :: IO ()
runAllTests = do
  putStrLn "\n=========================================="
  putStrLn "  Week 3: Advanced Recursion Tests"
  putStrLn "=========================================="

  testQuicksort
  testTreeDepth
  testTreeMap
  testFlattenTree
  testLookup
  testFilterMaybe
  testCompareLength
  testTreeContains
  testTreeInsert
  -- testSplit
  -- testMerge
  -- testMergeSort

  putStrLn "\nAll test categories completed!"
  putStrLn "(Check for [FAIL] marks above to see any failures)\n"


-- ============================================================================
-- Individual Test Runners (for targeted testing)
-- ============================================================================

-- You can run these individually in GHCi:
-- > testQuicksort
-- > testTreeDepth
-- etc.
