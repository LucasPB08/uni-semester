module Exercises where

-- Week 3: Advanced Recursion and Type System Fundamentals
-- Focus: Multiple recursion, tree structures, polymorphic types, type classes
-- Test your functions in GHCi by loading this file with `:load exercises.hs`

{-
  INSTRUCTIONS:
  1. Read the worked examples in README.md first
  2. Implement each function below
  3. Test with the provided examples in GHCi
  4. Run tests with `:load tests.hs` then `runAllTests`
-}

-- ============================================================================
-- Data Type Definitions (provided for you)
-- ============================================================================

-- Binary tree that holds values of type 'a'
data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)


-- ============================================================================
-- Exercise 1: Multiple Recursion - Quicksort
-- ============================================================================
-- Concepts: Multiple recursive calls, list comprehensions, divide-and-conquer
-- Difficulty: Medium-Hard
-- Scala Equivalent: Quicksort using filter and recursion

{-
Problem:
Implement quicksort algorithm using list comprehensions and recursion.

Algorithm:
1. Empty list is already sorted (base case)
2. Pick first element as pivot
3. Partition rest into smaller and larger elements
4. Recursively sort both partitions
5. Concatenate: sorted smaller + pivot + sorted larger

Examples:
  quicksort [3,1,4,1,5,9,2]  --> [1,1,2,3,4,5,9]
  quicksort [5,4,3,2,1]      --> [1,2,3,4,5]
  quicksort []               --> []
  quicksort [1]              --> [1]
  quicksort "haskell"        --> "aehklls"


Type Signature: Ord a => [a] -> [a]
-}

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let halves = foldr (\el acc -> if el <= x then (el : (fst acc), snd acc) else (fst acc, el : snd acc)) ([], []) xs
  in (quicksort (fst halves) ++ [x] ++ quicksort (snd halves))


-- ============================================================================
-- Exercise 2: Tree Depth
-- ============================================================================
-- Concepts: Recursive data types, multiple recursive calls
-- Difficulty: Medium
-- Scala Equivalent: Pattern matching on tree with recursive depth calculation

{-
Problem:
Calculate the depth (height) of a binary tree.
The depth is the longest path from root to any leaf.
- Empty tree has depth 0
- Single node has depth 1
- Otherwise: 1 + max(depth of left, depth of right)

Examples:
  treeDepth Empty  --> 0
  treeDepth (Node 5 Empty Empty)  --> 1

  Tree:    5        Depth: 2
          / \
         3   8
  treeDepth (Node 5 (Node 3 Empty Empty) (Node 8 Empty Empty))  --> 2

  Tree:      5      Depth: 3
            / \
           3   8
          /
         1
  treeDepth (Node 5 (Node 3 (Node 1 Empty Empty) Empty) (Node 8 Empty Empty))  --> 3

Hint: Use max function to find the deeper subtree
      treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)

Type Signature: Tree a -> Int
-}

treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)


-- ============================================================================
-- Exercise 3: Tree Map (Transform Tree Values)
-- ============================================================================
-- Concepts: Polymorphic functions, tree recursion, type variables
-- Difficulty: Medium
-- Scala Equivalent: tree.map(f) but manual implementation

{-
Problem:
Apply a function to every value in a tree, creating a new tree.
This is like 'map' for lists, but for trees.

Examples:
  treeMap (+1) Empty  --> Empty
  treeMap (*2) (Node 5 Empty Empty)  --> Node 10 Empty Empty

  Tree:    5              Result:    10
          / \                        / \
         3   8    (*2)  ==>         6   16
  treeMap (*2) (Node 5 (Node 3 Empty Empty) (Node 8 Empty Empty))
    --> Node 10 (Node 6 Empty Empty) (Node 16 Empty Empty)

  treeMap length (Node "hi" (Node "hello" Empty Empty) Empty)
    --> Node 2 (Node 5 Empty Empty) Empty

Hint: Pattern match on Empty and Node
      Apply f to the value, recursively map over subtrees
      Node val left right becomes Node (f val) (treeMap f left) (treeMap f right)

Type Signature: (a -> b) -> Tree a -> Tree b
-}

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Empty = Empty
treeMap fun (Node el left right) = (Node (fun el) (treeMap fun left) (treeMap fun right))


-- ============================================================================
-- Exercise 4: Flatten Tree (In-order Traversal)
-- ============================================================================
-- Concepts: Tree recursion, list concatenation
-- Difficulty: Medium
-- Scala Equivalent: In-order tree traversal to list

{-
Problem:
Convert a tree to a list using in-order traversal.
In-order means: left subtree, then root, then right subtree.

Examples:
  flattenTree Empty  --> []
  flattenTree (Node 5 Empty Empty)  --> [5]

  Tree:    5              Result: [3, 5, 8]
          / \             (left first: 3, then root: 5, then right: 8)
         3   8
  flattenTree (Node 5 (Node 3 Empty Empty) (Node 8 Empty Empty))  --> [3,5,8]

  Tree:      5            Result: [1, 3, 5, 8]
            / \           (leftmost first: 1, then 3, then 5, then 8)
           3   8
          /
         1
  flattenTree (Node 5 (Node 3 (Node 1 Empty Empty) Empty) (Node 8 Empty Empty))
    --> [1,3,5,8]

Hint: Pattern match on Empty and Node
      Concatenate: flattenTree left ++ [val] ++ flattenTree right

Type Signature: Tree a -> [a]
-}

flattenTree :: Tree a -> [a]
flattenTree Empty = []
flattenTree (Node el left right) = (flattenTree left) ++ (el : (flattenTree right))


-- ============================================================================
-- Exercise 5: Lookup in Association List
-- ============================================================================
-- Concepts: Maybe type, Eq type class, recursion
-- Difficulty: Medium
-- Scala Equivalent: list.find(_._1 == key).map(_._2)

{-
Problem:
Search for a key in an association list (list of key-value pairs).
Return Just value if found, Nothing if not found.

An association list is like a simple map: [(key, value)]

Examples:
  let phonebook = [("Alice", "555-1234"), ("Bob", "555-5678")]
  lookup' "Alice" phonebook  --> Just "555-1234"
  lookup' "Bob" phonebook    --> Just "555-5678"
  lookup' "Charlie" phonebook --> Nothing

  lookup' 1 [(1,'a'), (2,'b'), (3,'c')]  --> Just 'a'
  lookup' 5 [(1,'a'), (2,'b'), (3,'c')]  --> Nothing
  lookup' 2 []  --> Nothing

Hint: Pattern match on list
      - Empty list: Nothing
      - (k,v):rest: if k == key, return Just v, else recurse on rest
      Use guards to check equality

Note: There's a built-in 'lookup' function, so we call this 'lookup''

Type Signature: Eq k => k -> [(k, v)] -> Maybe v
-}

lookup' :: Eq k => k -> [(k, v)] -> Maybe v
lookup' _ [] = Nothing
lookup' el ((key, value) : rest)
  | key == el = Just value
  | otherwise = lookup' el rest


-- ============================================================================
-- Exercise 6: Filter Maybe (Keep Only Just Values)
-- ============================================================================
-- Concepts: Maybe type, pattern matching, list recursion
-- Difficulty: Medium
-- Scala Equivalent: list.flatten (for Option)

{-
Problem:
Given a list of Maybe values, extract all the Just values and discard Nothing.
This is called "catMaybes" in standard libraries.

Examples:
  filterMaybe [Just 1, Nothing, Just 3, Nothing, Just 5]  --> [1,3,5]
  filterMaybe [Nothing, Nothing]                          --> []
  filterMaybe [Just 'a', Just 'b', Nothing, Just 'c']     --> "abc"
  filterMaybe []                                          --> []

Hint: Pattern match on list and each element
      - [] base case: []
      - Just x : rest: cons x onto recursive result
      - Nothing : rest: skip it, just recurse

Type Signature: [Maybe a] -> [a]
-}

filterMaybe :: [Maybe a] -> [a]
filterMaybe [] = []
filterMaybe (Nothing : rest) = filterMaybe rest
filterMaybe (Just el : rest) = el : filterMaybe rest


-- ============================================================================
-- Exercise 7: Compare Lengths
-- ============================================================================
-- Concepts: Ord type class, polymorphic functions
-- Difficulty: Medium
-- Scala Equivalent: list1.length.compareTo(list2.length)

{-
Problem:
Compare two lists by length and return LT, EQ, or GT.
- LT if first list is shorter
- EQ if same length
- GT if first list is longer

Don't calculate lengths directly! Use pattern matching for efficiency.

Examples:
  compareLength [1,2] [3,4,5]      --> LT  (2 < 3)
  compareLength [1,2,3] [4,5,6]    --> EQ  (3 == 3)
  compareLength [1,2,3,4] [5,6]    --> GT  (4 > 2)
  compareLength [] []              --> EQ
  compareLength [] [1]             --> LT
  compareLength "hello" "world"    --> EQ  (both length 5)

Hint: Pattern match on BOTH lists simultaneously
      - [], []     --> EQ
      - [], _:_    --> LT (first is shorter)
      - _:_, []    --> GT (second is shorter)
      - _:xs, _:ys --> recurse with tails

Type Signature: [a] -> [b] -> Ordering
-}

compareLength :: [a] -> [b] -> Ordering
compareLength [] []= EQ 
compareLength _ [] = GT
compareLength [] _ = LT 
compareLength (x:xs) (y:ys) = compareLength xs ys


-- ============================================================================
-- Exercise 8: Tree Contains (Search)
-- ============================================================================
-- Concepts: Eq type class, tree recursion, boolean logic
-- Difficulty: Medium
-- Scala Equivalent: tree.exists(_ == target)

{-
Problem:
Check if a tree contains a specific value.
Return True if found, False otherwise.

Examples:
  treeContains 5 Empty  --> False
  treeContains 5 (Node 5 Empty Empty)  --> True
  treeContains 3 (Node 5 Empty Empty)  --> False

  Tree:    5
          / \
         3   8
  treeContains 3 (Node 5 (Node 3 Empty Empty) (Node 8 Empty Empty))  --> True
  treeContains 8 (Node 5 (Node 3 Empty Empty) (Node 8 Empty Empty))  --> True
  treeContains 7 (Node 5 (Node 3 Empty Empty) (Node 8 Empty Empty))  --> False

Hint: Pattern match on tree
      - Empty: False
      - Node val left right:
          True if val == target
          Otherwise: search left OR search right
      Use || for "or" operation

Type Signature: Eq a => a -> Tree a -> Bool
-}

treeContains :: Eq a => a -> Tree a -> Bool
treeContains _ Empty = False 
treeContains el (Node a left right) 
  | el == a = True
  | otherwise = (treeContains el left) || (treeContains el right)


-- ============================================================================
-- BONUS Exercise 9: Tree Insert (Binary Search Tree)
-- ============================================================================
-- Concepts: Ord type class, tree recursion, maintaining invariants
-- Difficulty: Hard
-- Scala Equivalent: BST insertion maintaining order

{-
Problem:
Insert a value into a binary search tree (BST) maintaining the BST property:
- All values in left subtree < node value
- All values in right subtree > node value

Examples:
  treeInsert 5 Empty  --> Node 5 Empty Empty

  Insert 3 into single node 5:
  treeInsert 3 (Node 5 Empty Empty)  --> Node 5 (Node 3 Empty Empty) Empty

  Insert 8 into single node 5:
  treeInsert 8 (Node 5 Empty Empty)  --> Node 5 Empty (Node 8 Empty Empty)

  Building a tree:
  let t1 = treeInsert 5 Empty          -- Node 5 Empty Empty
      t2 = treeInsert 3 t1             -- Node 5 (Node 3 Empty Empty) Empty
      t3 = treeInsert 8 t2             -- Node 5 (Node 3 Empty Empty) (Node 8 Empty Empty)
      t4 = treeInsert 1 t3             -- ...
  in flattenTree t4  --> [1,3,5,8]  (in-order gives sorted list!)

Hint: Pattern match on tree
      - Empty: create Node val Empty Empty
      - Node v left right:
          if val <= v: insert into left subtree
          if val > v: insert into right subtree

Type Signature: Ord a => a -> Tree a -> Tree a
-}

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert el Empty = Node el Empty Empty 
treeInsert el (Node a left right) 
  | el < a = Node a (treeInsert el left) right
  | otherwise = Node a left (treeInsert el right)


-- ============================================================================
-- BONUS Exercise 10: Merge Sort
-- ============================================================================
-- Concepts: Divide and conquer, multiple recursion, list splitting
-- Difficulty: Hard
-- Scala Equivalent: Merge sort implementation

{-
Problem:
Implement merge sort algorithm.

Algorithm:
1. Lists of 0 or 1 elements are already sorted (base cases)
2. Split list into two halves
3. Recursively sort each half
4. Merge the sorted halves

Examples:
  mergeSort [3,1,4,1,5,9,2]  --> [1,1,2,3,4,5,9]
  mergeSort []               --> []
  mergeSort [1]              --> [1]
  mergeSort [5,4,3,2,1]      --> [1,2,3,4,5]

Helper function (you'll need to implement this):
  split :: [a] -> ([a], [a])
  Split list into two halves: split [1,2,3,4,5] --> ([1,3,5], [2,4])

Then:
  merge :: Ord a => [a] -> [a] -> [a]  (from README.md example)

Hint for mergeSort:
  - Base cases: [] and [x] are already sorted
  - Recursive: split list, sort each half, merge results

Hint for split:
  - Use pattern matching to alternate elements
  - split [] = ([], [])
  - split [x] = ([x], [])
  - split (x:y:rest) = let (left, right) = split rest
                       in (x:left, y:right)

Type Signature: Ord a => [a] -> [a]
-}

-- Helper: split list into two halves
split :: [a] -> ([a], [a])
split = undefined

-- Helper: merge two sorted lists (see README.md for implementation)
merge :: Ord a => [a] -> [a] -> [a]
merge = undefined

-- Main merge sort function
mergeSort :: Ord a => [a] -> [a]
mergeSort = undefined


-- ============================================================================
-- TESTING YOUR SOLUTIONS
-- ============================================================================

{-
To test your functions:

1. Open GHCi:
   ghci

2. Load this file:
   :load exercises.hs

3. Test individual functions:
   quicksort [3,1,4,1,5,9,2]
   treeDepth (Node 5 (Node 3 Empty Empty) Empty)
   lookup' "Alice" [("Alice", "555-1234"), ("Bob", "555-5678")]

4. Load and run all tests:
   :load tests.hs
   runAllTests

5. If you make changes, reload:
   :reload

6. Check types:
   :type quicksort
   :type treeMap
   :info Ord
-}
