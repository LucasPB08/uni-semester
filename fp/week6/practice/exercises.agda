-- Week 6: Agda Basics - Exercises
-- =================================
--
-- Instructions:
-- 1. Load the file with C-c C-l to typecheck
-- 2. Replace each ? (hole) with your implementation
-- 3. Use C-c C-, to see the goal type
-- 4. Use C-c C-c to case split on a variable
-- 5. Use C-c C-r to refine a hole
--
-- If it typechecks, it's correct!

module exercises where

------------------------------------------------------------------------
-- PART 1: Basic Data Types and Functions
------------------------------------------------------------------------

-- Natural numbers (given)
data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

-- Convenient shorthand (BUILTIN pragma for numeric literals)
{-# BUILTIN NATURAL Nat #-}

-- Booleans (given)
data Bool : Set where
  false : Bool
  true  : Bool


-- Exercise 1: Boolean NOT
-- Concepts: Basic pattern matching, data types
-- Difficulty: Easy
-- Haskell equivalent: not False = True; not True = False

not : Bool → Bool
not true = false
not false = true


-- Exercise 2: Boolean AND
-- Concepts: Pattern matching on two arguments
-- Difficulty: Easy

_∧_ : Bool → Bool → Bool
true ∧ true = true
false ∧ _ = false 
_ ∧ false = false


-- Exercise 3: Boolean OR
-- Concepts: Pattern matching
-- Difficulty: Easy

_∨_ : Bool → Bool → Bool
true ∨ _ = true
_ ∨ true = true
_ ∨ _ = false 


-- Exercise 4: Addition of natural numbers
-- Concepts: Recursion, pattern matching
-- Difficulty: Easy
--
-- Hint: Recurse on the first argument
-- zero + m should return m
-- (suc n) + m should return suc (n + m)

_+_ : Nat → Nat → Nat
zero + m = m 
(suc n) + m = suc (n + m)

-- Exercise 5: Multiplication
-- Concepts: Recursion using addition
-- Difficulty: Easy-Medium

_*_ : Nat → Nat → Nat
zero * _ = zero
_ * zero = zero
n * 1 = n
1 * n = n 
n * (suc m) = n + (n * m)


-- Exercise 6: Predecessor (subtract 1, but zero stays zero)
-- Concepts: Pattern matching
-- Difficulty: Easy

pred : Nat → Nat
pred zero = zero
pred (suc n) = n 


-- Exercise 7: Monus (subtraction that floors at zero)
-- Concepts: Recursion on two arguments
-- Difficulty: Medium
--
-- Examples: 5 ∸ 3 = 2
--           3 ∸ 5 = 0 (floors at zero)
--

_∸_ : Nat → Nat → Nat
n ∸ zero = n
zero ∸ n = zero
(suc n) ∸ (suc m) = n ∸ m

------------------------------------------------------------------------
-- PART 2: Polymorphic Data Types
------------------------------------------------------------------------

-- Lists (given)
data List (A : Set) : Set where
  []  : List A
  _∷_ : A → List A → List A

infixr 5 _∷_


-- Exercise 8: Length of a list
-- Concepts: Polymorphic functions, implicit arguments
-- Difficulty: Easy
--
-- The {A : Set} is an implicit argument - Agda infers it

length : {A : Set} → List A → Nat
length [] = zero
length (x ∷ xs) = 1 + (length xs)  


-- Exercise 9: Append two lists
-- Concepts: Polymorphic recursion
-- Difficulty: Easy

_++_ : {A : Set} → List A → List A → List A
[] ++ ys = ys 
(x ∷ xs) ++ ys = x ∷ (xs ++ ys) 


-- Exercise 10: Map over a list
-- Concepts: Higher-order functions
-- Difficulty: Medium

map : {A B : Set} → (A → B) → List A → List B
map f [] = [] 
map f (x ∷ xs) = (f x) ∷ (map f xs)


-- Exercise 11: Reverse a list
-- Concepts: Recursion, accumulator pattern (or simple recursion)
-- Difficulty: Medium
--
-- Hint: You can use _++_ in your solution

reverse : {A : Set} → List A → List A
reverse [] = []
reverse (x ∷ xs) = (reverse xs) ++ (x ∷ [])


------------------------------------------------------------------------
-- PART 3: Maybe Type (handling missing values)
------------------------------------------------------------------------

-- Maybe type (given)
data Maybe (A : Set) : Set where
  nothing : Maybe A
  just    : A → Maybe A


-- Exercise 12: Safe head (returns Maybe)
-- Concepts: Handling empty case safely
-- Difficulty: Easy
--
-- Unlike Haskell's head which crashes, this returns nothing for []

safeHead : {A : Set} → List A → Maybe A
safeHead [] = nothing 
safeHead (x ∷ xs) = just x 


-- Exercise 13: Safe lookup by index
-- Concepts: Double recursion
-- Difficulty: Medium
--
-- Return the element at index n, or nothing if index out of bounds
-- Indices are zero-based: lookup 0 [a,b,c] = just a

lookup : {A : Set} → Nat → List A → Maybe A
lookup _ [] = nothing
lookup zero (x ∷ xs) = just x
lookup (suc n) (x ∷ xs) = lookup n xs 


-- Exercise 14: map for Maybe (functor-like behavior)
-- Concepts: Pattern matching on Maybe
-- Difficulty: Easy

mapMaybe : {A B : Set} → (A → B) → Maybe A → Maybe B
mapMaybe f nothing = nothing 
mapMaybe f (just a) = just (f a)


------------------------------------------------------------------------
-- PART 4: Dependent Types - Vectors
------------------------------------------------------------------------

-- Vector: a list that knows its length at the type level!
data Vec (A : Set) : Nat → Set where
  []  : Vec A zero
  _∷_ : {n : Nat} → A → Vec A n → Vec A (suc n)


-- Exercise 15: Safe head for vectors
-- Concepts: Dependent types, guaranteed non-empty
-- Difficulty: Easy
--
-- Note: The type (suc n) guarantees the vector is non-empty
-- You don't need to handle the empty case!

vhead : {A : Set} {n : Nat} → Vec A (suc n) → A
vhead (x ∷ xs) = x


-- Exercise 16: Safe tail for vectors
-- Concepts: Length decreases by 1
-- Difficulty: Easy

vtail : {A : Set} {n : Nat} → Vec A (suc n) → Vec A n
vtail (x ∷ xs) = xs


-- Exercise 17: Replicate (create vector of n copies)
-- Concepts: Building vectors with correct length
-- Difficulty: Medium
--
-- Note: The output length matches the input Nat

replicate : {A : Set} (n : Nat) → A → Vec A n
replicate zero x = []
replicate (suc n) x = x ∷ replicate n x 

-- Exercise 18: Append vectors (lengths add up!)
-- Concepts: Dependent types, length arithmetic
-- Difficulty: Medium
--
-- Note the type: Vec A m → Vec A n → Vec A (m + n)
-- The output length is provably m + n

vappend : {A : Set} {m n : Nat} → Vec A m → Vec A n → Vec A (m + n)
vappend [] ys = ys 
vappend (x ∷ xs) ys = x ∷ vappend xs ys


-- Exercise 19: Map over vectors (length preserved)
-- Concepts: Type-preserving operations
-- Difficulty: Easy

vmap : {A B : Set} {n : Nat} → (A → B) → Vec A n → Vec B n
vmap f [] = [] 
vmap f (x ∷ xs) = (f x) ∷ vmap f xs


------------------------------------------------------------------------
-- PART 5: Finite Numbers
------------------------------------------------------------------------

-- Fin n represents the set {0, 1, ..., n-1}
-- Fin zero is empty (there are no naturals less than zero)
data Fin : Nat → Set where
  zero : {n : Nat} → Fin (suc n)
  suc  : {n : Nat} → Fin n → Fin (suc n)


-- Exercise 20: Safe vector lookup using Fin
-- Concepts: Dependent types guarantee bounds
-- Difficulty: Medium
--
-- Because Fin n only contains values 0..n-1, and the vector has length n,
-- the index is always in bounds. No Maybe needed!

vlookup : {A : Set} {n : Nat} → Vec A n → Fin n → A
vlookup (x ∷ xs) zero = x 
vlookup (x ∷ xs) (suc n) = vlookup xs n


------------------------------------------------------------------------
-- VERIFICATION (these should typecheck once exercises are complete)
------------------------------------------------------------------------

-- Some test values
one : Nat
one = 1

two : Nat
two = 2

three : Nat
three = 3

exampleList : List Nat
exampleList = 1 ∷ 2 ∷ 3 ∷ []

exampleVec : Vec Nat 3
exampleVec = 1 ∷ 2 ∷ 3 ∷ []

-- Uncomment these after completing exercises to verify:

-- test-not : not true ≡ false
-- test-add : (2 + 3) ≡ 5
-- test-mult : (2 * 3) ≡ 6
-- test-length : length exampleList ≡ 3
-- test-vhead : vhead exampleVec ≡ 1

-- (We'll define ≡ for proofs in the next exercises or week 7!)
