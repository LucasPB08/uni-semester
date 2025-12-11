-- Week 4: Tests for Monads and Lazy Evaluation
-- =============================================
-- Run with: :load tests.hs then main

module Main where

import Exercises
import Control.Monad (guard)
import Data.List (sort, nub)

-- Test framework
type TestResult = (String, Bool)

runTest :: String -> Bool -> TestResult
runTest name result = (name, result)

printResult :: TestResult -> IO ()
printResult (name, passed) =
  putStrLn $ (if passed then "[PASS] " else "[FAIL] ") ++ name

runTests :: [TestResult] -> IO ()
runTests tests = do
  mapM_ printResult tests
  let passed = length $ filter snd tests
  let total = length tests
  putStrLn $ "\n" ++ show passed ++ "/" ++ show total ++ " tests passed"

-- ============================================================================
-- PART 1: Maybe Monad Tests
-- ============================================================================

testSafeLast :: [TestResult]
testSafeLast =
  [ runTest "safeLast [1,2,3] = Just 3" (safeLast [1,2,3] == Just 3)
  , runTest "safeLast \"hello\" = Just 'o'" (safeLast "hello" == Just 'o')
  , runTest "safeLast [] = Nothing" (safeLast ([] :: [Int]) == Nothing)
  , runTest "safeLast [42] = Just 42" (safeLast [42] == Just 42)
  ]

testSafeIndex :: [TestResult]
testSafeIndex =
  [ runTest "safeIndex [1,2,3] 0 = Just 1" (safeIndex [1,2,3] 0 == Just 1)
  , runTest "safeIndex [1,2,3] 1 = Just 2" (safeIndex [1,2,3] 1 == Just 2)
  , runTest "safeIndex [1,2,3] 2 = Just 3" (safeIndex [1,2,3] 2 == Just 3)
  , runTest "safeIndex [1,2,3] 5 = Nothing" (safeIndex [1,2,3] 5 == Nothing)
  , runTest "safeIndex [] 0 = Nothing" (safeIndex ([] :: [Int]) 0 == Nothing)
  , runTest "safeIndex [1,2,3] (-1) = Nothing" (safeIndex [1,2,3] (-1) == Nothing)
  ]

testSafeDiv :: [TestResult]
testSafeDiv =
  [ runTest "safeDiv 10 2 = Just 5" (safeDiv 10 2 == Just 5)
  , runTest "safeDiv 10 3 = Just 3" (safeDiv 10 3 == Just 3)
  , runTest "safeDiv 10 0 = Nothing" (safeDiv 10 0 == Nothing)
  , runTest "safeDiv 0 5 = Just 0" (safeDiv 0 5 == Just 0)
  ]

testSafeSqrt :: [TestResult]
testSafeSqrt =
  [ runTest "safeSqrt 16 = Just 4" (safeSqrt 16.0 == Just 4.0)
  , runTest "safeSqrt 25 = Just 5" (safeSqrt 25.0 == Just 5.0)
  , runTest "safeSqrt 0 = Just 0" (safeSqrt 0.0 == Just 0.0)
  , runTest "safeSqrt (-1) = Nothing" (safeSqrt (-1) == Nothing)
  , runTest "safeSqrt (-100) = Nothing" (safeSqrt (-100) == Nothing)
  ]

testChainedDiv :: [TestResult]
testChainedDiv =
  [ runTest "chainedDiv 20 2 2 = Just 5" (chainedDiv 20 2 2 == Just 5)
  , runTest "chainedDiv 100 5 4 = Just 5" (chainedDiv 100 5 4 == Just 5)
  , runTest "chainedDiv 20 0 2 = Nothing" (chainedDiv 20 0 2 == Nothing)
  , runTest "chainedDiv 20 2 0 = Nothing" (chainedDiv 20 2 0 == Nothing)
  , runTest "chainedDiv 20 0 0 = Nothing" (chainedDiv 20 0 0 == Nothing)
  ]

testThirdElement :: [TestResult]
testThirdElement =
  [ runTest "thirdElement [1,2,3,4] = Just 3" (thirdElement [1,2,3,4] == Just 3)
  , runTest "thirdElement [1,2,3] = Just 3" (thirdElement [1,2,3] == Just 3)
  , runTest "thirdElement [1,2] = Nothing" (thirdElement [1,2] == Nothing)
  , runTest "thirdElement [1] = Nothing" (thirdElement [1] == Nothing)
  , runTest "thirdElement [] = Nothing" (thirdElement ([] :: [Int]) == Nothing)
  , runTest "thirdElement \"hello\" = Just 'l'" (thirdElement "hello" == Just 'l')
  ]

testParseAndAdd :: [TestResult]
testParseAndAdd =
  [ runTest "parseAndAdd \"3+5\" = Just 8" (parseAndAdd "3+5" == Just 8)
  , runTest "parseAndAdd \"9+1\" = Just 10" (parseAndAdd "9+1" == Just 10)
  , runTest "parseAndAdd \"0+0\" = Just 0" (parseAndAdd "0+0" == Just 0)
  , runTest "parseAndAdd \"a+b\" = Nothing" (parseAndAdd "a+b" == Nothing)
  , runTest "parseAndAdd \"12\" = Nothing" (parseAndAdd "12" == Nothing)
  , runTest "parseAndAdd \"\" = Nothing" (parseAndAdd "" == Nothing)
  , runTest "parseAndAdd \"1+\" = Nothing" (parseAndAdd "1+" == Nothing)
  ]

testGetFirstCourseGrade :: [TestResult]
testGetFirstCourseGrade =
  [ runTest "getFirstCourseGrade 1 = Just 'A'" (getFirstCourseGrade 1 == Just 'A')
  , runTest "getFirstCourseGrade 2 = Just 'C'" (getFirstCourseGrade 2 == Just 'C')
  , runTest "getFirstCourseGrade 3 = Nothing (no courses)" (getFirstCourseGrade 3 == Nothing)
  , runTest "getFirstCourseGrade 99 = Nothing (no student)" (getFirstCourseGrade 99 == Nothing)
  ]

-- ============================================================================
-- PART 2: List Monad Tests
-- ============================================================================

testAllPairs :: [TestResult]
testAllPairs =
  [ runTest "allPairs [1,2] ['a','b']" (sort (allPairs [1,2] ['a','b']) == sort [(1,'a'),(1,'b'),(2,'a'),(2,'b')])
  , runTest "allPairs [] [1,2] = []" (allPairs ([] :: [Int]) [1,2] == [])
  , runTest "allPairs [1] ['x'] = [(1,'x')]" (allPairs [1] ['x'] == [(1,'x')])
  , runTest "allPairs [1,2,3] [10]" (sort (allPairs [1,2,3] [10]) == [(1,10),(2,10),(3,10)])
  ]

testAllTriples :: [TestResult]
testAllTriples =
  [ runTest "allTriples [1] ['a'] [True]" (allTriples [1] ['a'] [True] == [(1,'a',True)])
  , runTest "length (allTriples [1,2] [3,4] [5,6]) = 8" (length (allTriples [1,2] [3,4] [5,6]) == 8)
  , runTest "allTriples [] [1] [2] = []" (allTriples ([] :: [Int]) [1] [2] == [])
  ]

testPairsThatSum :: [TestResult]
testPairsThatSum =
  [ runTest "pairsThatSum [1,2,3] [4,5,6] 7" (sort (pairsThatSum [1,2,3] [4,5,6] 7) == sort [(1,6),(2,5),(3,4)])
  , runTest "pairsThatSum [1,2] [1,2] 3" (sort (pairsThatSum [1,2] [1,2] 3) == sort [(1,2),(2,1)])
  , runTest "pairsThatSum [1,2] [3,4] 10 = []" (pairsThatSum [1,2] [3,4] 10 == [])
  ]

testFactors :: [TestResult]
testFactors =
  [ runTest "factors 12 = [(1,12),(2,6),(3,4)]" (sort (factors 12) == [(1,12),(2,6),(3,4)])
  , runTest "factors 7 = [(1,7)]" (factors 7 == [(1,7)])
  , runTest "factors 1 = [(1,1)]" (factors 1 == [(1,1)])
  , runTest "factors 16 = [(1,16),(2,8),(4,4)]" (sort (factors 16) == [(1,16),(2,8),(4,4)])
  , runTest "factors 100" (sort (factors 100) == [(1,100),(2,50),(4,25),(5,20),(10,10)])
  ]

testKnightMoves :: [TestResult]
testKnightMoves =
  [ runTest "knightMoves (1,1) has 2 moves" (length (knightMoves (1,1)) == 2)
  , runTest "knightMoves (1,1) contains (2,3)" ((2,3) `elem` knightMoves (1,1))
  , runTest "knightMoves (1,1) contains (3,2)" ((3,2) `elem` knightMoves (1,1))
  , runTest "knightMoves (4,4) has 8 moves" (length (knightMoves (4,4)) == 8)
  , runTest "knightMoves stays on board" (all (\(r,c) -> r >= 1 && r <= 8 && c >= 1 && c <= 8) (knightMoves (4,4)))
  ]

testCanReachIn :: [TestResult]
testCanReachIn =
  [ runTest "canReachIn 1 (1,1) (2,3)" (canReachIn 1 (1,1) (2,3))
  , runTest "canReachIn 1 (1,1) (3,2)" (canReachIn 1 (1,1) (3,2))
  , runTest "canReachIn 1 (1,1) (3,3) = False" (not (canReachIn 1 (1,1) (3,3)))
  , runTest "canReachIn 2 (1,1) (1,1)" (canReachIn 2 (1,1) (1,1))
  , runTest "canReachIn 0 (1,1) (1,1)" (canReachIn 0 (1,1) (1,1))
  , runTest "canReachIn 0 (1,1) (2,2) = False" (not (canReachIn 0 (1,1) (2,2)))
  ]

testPerms :: [TestResult]
testPerms =
  [ runTest "perms \"\" = [\"\"]" (perms "" == [""])
  , runTest "perms [1] = [[1]]" (perms [1] == [[1]])
  , runTest "length (perms [1,2]) = 2" (length (perms [1,2]) == 2)
  , runTest "perms [1,2] contains [1,2] and [2,1]" (sort (perms [1,2]) == [[1,2],[2,1]])
  , runTest "length (perms [1,2,3]) = 6" (length (perms [1,2,3]) == 6)
  , runTest "length (perms [1,2,3,4]) = 24" (length (perms [1,2,3,4]) == 24)
  , runTest "all perms are unique" (length (nub (perms [1,2,3])) == 6)
  ]

-- ============================================================================
-- PART 3: Lazy Evaluation Tests
-- ============================================================================

testPowersOfTwo :: [TestResult]
testPowersOfTwo =
  [ runTest "take 5 powersOfTwo" (take 5 powersOfTwo == [1,2,4,8,16])
  , runTest "take 10 powersOfTwo" (take 10 powersOfTwo == [1,2,4,8,16,32,64,128,256,512])
  , runTest "powersOfTwo !! 10 = 1024" (powersOfTwo !! 10 == 1024)
  , runTest "powersOfTwo !! 20 = 2^20" (powersOfTwo !! 20 == 2^20)
  ]

testTriangulars :: [TestResult]
testTriangulars =
  [ runTest "take 5 triangulars" (take 5 triangulars == [1,3,6,10,15])
  , runTest "take 10 triangulars" (take 10 triangulars == [1,3,6,10,15,21,28,36,45,55])
  , runTest "triangulars !! 99 = 5050" (triangulars !! 99 == 5050)
  ]

testFibs :: [TestResult]
testFibs =
  [ runTest "take 10 fibs" (take 10 fibs == [0,1,1,2,3,5,8,13,21,34])
  , runTest "fibs !! 20 = 6765" (fibs !! 20 == 6765)
  , runTest "fibs !! 30 = 832040" (fibs !! 30 == 832040)
  ]

testLucas :: [TestResult]
testLucas =
  [ runTest "take 7 lucas" (take 7 lucas == [2,1,3,4,7,11,18])
  , runTest "lucas !! 10 = 123" (lucas !! 10 == 123)
  , runTest "lucas follows recurrence" (all (\i -> lucas !! i == lucas !! (i-1) + lucas !! (i-2)) [2..15])
  ]

testPrimes :: [TestResult]
testPrimes =
  [ runTest "take 10 primes" (take 10 primes == [2,3,5,7,11,13,17,19,23,29])
  , runTest "primes !! 100 = 547" (primes !! 100 == 547)
  , runTest "primes are actually prime" (all isPrimeCheck (take 50 primes))
  ]
  where
    isPrimeCheck n = n > 1 && null [x | x <- [2..n-1], n `mod` x == 0]

testIsPrime :: [TestResult]
testIsPrime =
  [ runTest "isPrime 2" (isPrime 2)
  , runTest "isPrime 17" (isPrime 17)
  , runTest "isPrime 97" (isPrime 97)
  , runTest "isPrime 1 = False" (not (isPrime 1))
  , runTest "isPrime 4 = False" (not (isPrime 4))
  , runTest "isPrime 100 = False" (not (isPrime 100))
  ]

testCollatz :: [TestResult]
testCollatz =
  [ runTest "collatz 6" (collatz 6 == [6,3,10,5,16,8,4,2,1])
  , runTest "collatz 1" (collatz 1 == [1])
  , runTest "collatz 27 has 112 elements" (length (collatz 27) == 112)
  , runTest "all collatz sequences end in 1" (all (\n -> last (collatz n) == 1) [1..100])
  ]

testCollatzLength :: [TestResult]
testCollatzLength =
  [ runTest "collatzLength 6 = 9" (collatzLength 6 == 9)
  , runTest "collatzLength 1 = 1" (collatzLength 1 == 1)
  , runTest "collatzLength 27 = 112" (collatzLength 27 == 112)
  ]

testInterleave :: [TestResult]
testInterleave =
  [ runTest "take 8 (interleave [1,3..] [2,4..])" (take 8 (interleave [1,3..] [2,4..]) == [1,2,3,4,5,6,7,8])
  , runTest "interleave \"abc\" \"123\"" (interleave "abc" "123" == "a1b2c3")
  , runTest "interleave [1,2] [3,4,5,6]" (interleave [1,2] [3,4,5,6] == [1,3,2,4,5,6])
  , runTest "interleave [] [1,2,3]" (interleave [] [1,2,3] == [1,2,3])
  ]

testMerge :: [TestResult]
testMerge =
  [ runTest "take 10 (merge [1,3..] [2,4..])" (take 10 (merge [1,3..] [2,4..]) == [1,2,3,4,5,6,7,8,9,10])
  , runTest "take 10 (merge [2,4..] [1,3..])" (take 10 (merge [2,4..] [1,3..]) == [1,2,3,4,5,6,7,8,9,10])
  , runTest "take 6 (merge [1,4,7..] [2,5,8..])" (take 6 (merge [1,4..] [2,5..]) == [1,2,4,5,7,8])
  ]

-- ============================================================================
-- PART 4: Combined Tests
-- ============================================================================

testFindFirst :: [TestResult]
testFindFirst =
  [ runTest "findFirst (> 100) [1..200]" (findFirst (> 100) [1..200] == Just 101)
  , runTest "findFirst even [1,3,5,6,7]" (findFirst even [1,3,5,6,7] == Just 6)
  , runTest "findFirst (> 5) [1..10]" (findFirst (> 5) [1..10] == Just 6)
  , runTest "findFirst (> 100) [1..10]" (findFirst (> 100) [1..10] == Nothing)
  , runTest "findFirst (const True) []" (findFirst (const True) ([] :: [Int]) == Nothing)
  ]

testNthPrime :: [TestResult]
testNthPrime =
  [ runTest "nthPrime 0 = Just 2" (nthPrime 0 == Just 2)
  , runTest "nthPrime 4 = Just 11" (nthPrime 4 == Just 11)
  , runTest "nthPrime 99 = Just 541" (nthPrime 99 == Just 541)
  , runTest "nthPrime (-1) = Nothing" (nthPrime (-1) == Nothing)
  ]

testHamming :: [TestResult]
testHamming =
  [ runTest "take 15 hamming" (take 15 hamming == [1,2,3,4,5,6,8,9,10,12,15,16,18,20,24])
  , runTest "hamming has no duplicates (first 100)" (take 100 hamming == nub (take 100 hamming))
  , runTest "all hamming numbers have only 2,3,5 as factors" (all isHamming (take 50 hamming))
  ]
  where
    isHamming 1 = True
    isHamming n
      | n `mod` 2 == 0 = isHamming (n `div` 2)
      | n `mod` 3 == 0 = isHamming (n `div` 3)
      | n `mod` 5 == 0 = isHamming (n `div` 5)
      | otherwise = False

-- ============================================================================
-- Main
-- ============================================================================

main :: IO ()
main = do
  putStrLn "=== Week 4: Monads and Lazy Evaluation Tests ===\n"

  putStrLn "--- Part 1: Maybe Monad ---"
  putStrLn "\nExercise 1: Safe List Operations"
  runTests testSafeLast
  runTests testSafeIndex

  putStrLn "\nExercise 2: Safe Arithmetic"
  runTests testSafeDiv
  runTests testSafeSqrt

  putStrLn "\nExercise 3: Chaining Maybe"
  runTests testChainedDiv
  runTests testThirdElement

  putStrLn "\nExercise 4: Maybe with Do-Notation"
  runTests testParseAndAdd

  putStrLn "\nExercise 5: User Lookup Chain"
  runTests testGetFirstCourseGrade

  putStrLn "\n--- Part 2: List Monad ---"
  putStrLn "\nExercise 6: Cartesian Products"
  runTests testAllPairs
  runTests testAllTriples

  putStrLn "\nExercise 7: List Monad with Guards"
  runTests testPairsThatSum
  runTests testFactors

  putStrLn "\nExercise 8: Chess Knight Moves"
  runTests testKnightMoves
  runTests testCanReachIn

  putStrLn "\nExercise 9: Permutations"
  -- runTests testPerms

  putStrLn "\n--- Part 3: Lazy Evaluation ---"
  putStrLn "\nExercise 10: Infinite Sequences"
  runTests testPowersOfTwo
  runTests testTriangulars

  putStrLn "\nExercise 11: Fibonacci Variations"
  runTests testFibs
  runTests testLucas

  putStrLn "\nExercise 12: Prime Generation"
  runTests testPrimes
  runTests testIsPrime

  putStrLn "\nExercise 13: Collatz Sequence"
  runTests testCollatz
  runTests testCollatzLength

  putStrLn "\nExercise 14: Interleaving Streams"
  runTests testInterleave
  runTests testMerge

  putStrLn "\n--- Part 4: Combined ---"
  putStrLn "\nExercise 15: Safe Operations on Infinite Structures"
  runTests testFindFirst
  runTests testNthPrime

  putStrLn "\nExercise 16: Hamming Numbers (Bonus)"
  runTests testHamming

  putStrLn "\n=== All tests complete ==="
