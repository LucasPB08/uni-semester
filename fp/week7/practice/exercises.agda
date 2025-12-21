-- Week 7: Propositions as Types - Exercises
-- ==========================================
--
-- In this file, you'll prove properties about your programs.
-- Remember: if it typechecks, it's correct!
--
-- Instructions:
-- 1. Load with C-c C-l
-- 2. Use C-c C-, to see goal type and context
-- 3. Use C-c C-c to case split
-- 4. Use C-c C-r to refine
-- 5. Replace each ? with your proof

module exercises where

------------------------------------------------------------------------
-- SETUP: Data types from Week 6 (given)
------------------------------------------------------------------------

-- Natural numbers
data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

{-# BUILTIN NATURAL Nat #-}

-- Addition
_+_ : Nat → Nat → Nat
zero    + m = m
suc n   + m = suc (n + m)

infixl 6 _+_

-- Multiplication
_*_ : Nat → Nat → Nat
zero    * m = zero
suc n   * m = m + (n * m)

infixl 7 _*_

-- Lists
data List (A : Set) : Set where
  []  : List A
  _∷_ : A → List A → List A

infixr 5 _∷_

-- List append
_++_ : {A : Set} → List A → List A → List A
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

infixr 5 _++_

-- List length
length : {A : Set} → List A → Nat
length []       = zero
length (_ ∷ xs) = suc (length xs)

-- List map
map : {A B : Set} → (A → B) → List A → List B
map f []       = []
map f (x ∷ xs) = f x ∷ map f xs

-- List reverse (using ++)
reverse : {A : Set} → List A → List A
reverse []       = []
reverse (x ∷ xs) = reverse xs ++ (x ∷ [])

-- Vectors
data Vec (A : Set) : Nat → Set where
  []  : Vec A zero
  _∷_ : {n : Nat} → A → Vec A n → Vec A (suc n)


------------------------------------------------------------------------
-- PART 1: The Identity Type
------------------------------------------------------------------------

-- Propositional equality
-- This is the most important definition: a ≡ b means "a equals b"
-- The only way to prove equality is refl: every value equals itself

data _≡_ {A : Set} : A → A → Set where
  refl : {x : A} → x ≡ x

infix 4 _≡_

{-# BUILTIN EQUALITY _≡_ #-}


-- Exercise 1: Reflexivity proof
-- Concepts: Using refl
-- Difficulty: Trivial
--
-- Prove that 5 equals 5

five-equals-five : 5 ≡ 5
five-equals-five = ?


-- Exercise 2: Computation in types
-- Concepts: Agda normalizes before comparing
-- Difficulty: Easy
--
-- Prove that 2 + 3 equals 5
-- Hint: Agda computes both sides, so refl should work!

two-plus-three : (2 + 3) ≡ 5
two-plus-three = ?


-- Exercise 3: Another computation
-- Concepts: More complex computation
-- Difficulty: Easy

six-is-two-times-three : 6 ≡ (2 * 3)
six-is-two-times-three = ?


------------------------------------------------------------------------
-- PART 2: Basic Equality Combinators
------------------------------------------------------------------------

-- Exercise 4: Symmetry
-- Concepts: Pattern matching on equality proof
-- Difficulty: Easy
--
-- If x ≡ y, then y ≡ x
-- Hint: Pattern match on the equality proof. After matching refl,
--       Agda knows x and y are the same!

sym : {A : Set} {x y : A} → x ≡ y → y ≡ x
sym p = ?


-- Exercise 5: Transitivity
-- Concepts: Chaining equalities
-- Difficulty: Easy
--
-- If x ≡ y and y ≡ z, then x ≡ z

trans : {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans p q = ?


-- Exercise 6: Congruence
-- Concepts: Applying functions to equalities
-- Difficulty: Easy
--
-- If x ≡ y, then f x ≡ f y
-- This is crucial for building larger proofs!

cong : {A B : Set} {x y : A} → (f : A → B) → x ≡ y → f x ≡ f y
cong f p = ?


-- Exercise 7: Using congruence
-- Concepts: Applying cong
-- Difficulty: Easy
--
-- Prove: if n ≡ m, then suc n ≡ suc m

suc-cong : {n m : Nat} → n ≡ m → suc n ≡ suc m
suc-cong p = ?


------------------------------------------------------------------------
-- PART 3: Properties of Addition
------------------------------------------------------------------------

-- Exercise 8: Right identity of addition
-- Concepts: Proof by induction
-- Difficulty: Medium
--
-- Prove: n + 0 ≡ n
--
-- Hint: This requires induction (pattern match on n)
-- Base case: 0 + 0 ≡ 0 (computes directly)
-- Inductive case: suc n + 0 ≡ suc n
--   - suc n + 0 computes to suc (n + 0)
--   - Use IH: n + 0 ≡ n
--   - Apply cong suc

+-identityʳ : (n : Nat) → n + 0 ≡ n
+-identityʳ n = ?


-- Exercise 9: Left identity of addition
-- Concepts: Definitional equality
-- Difficulty: Easy
--
-- Prove: 0 + n ≡ n
-- Hint: Look at how _+_ is defined. This should be trivial!

+-identityˡ : (n : Nat) → 0 + n ≡ n
+-identityˡ n = ?


-- Exercise 10: Successor on the right
-- Concepts: Key lemma for commutativity
-- Difficulty: Medium
--
-- Prove: m + suc n ≡ suc (m + n)
--
-- This requires induction on m

+-suc : (m n : Nat) → m + suc n ≡ suc (m + n)
+-suc m n = ?


-- Exercise 11: Addition is commutative
-- Concepts: Using lemmas, induction
-- Difficulty: Medium-Hard
--
-- Prove: m + n ≡ n + m
--
-- Hints:
-- - Base case (m = 0): need to show 0 + n ≡ n + 0
--   Use +-identityʳ and sym
-- - Inductive case: need to show suc m + n ≡ n + suc m
--   Use IH and +-suc

+-comm : (m n : Nat) → m + n ≡ n + m
+-comm m n = ?


-- Exercise 12: Addition is associative
-- Concepts: Induction
-- Difficulty: Medium
--
-- Prove: (m + n) + p ≡ m + (n + p)

+-assoc : (m n p : Nat) → (m + n) + p ≡ m + (n + p)
+-assoc m n p = ?


------------------------------------------------------------------------
-- PART 4: Properties of Lists
------------------------------------------------------------------------

-- Exercise 13: Length of append
-- Concepts: Induction on lists
-- Difficulty: Medium
--
-- Prove: length (xs ++ ys) ≡ length xs + length ys

length-++ : {A : Set} (xs ys : List A)
          → length (xs ++ ys) ≡ length xs + length ys
length-++ xs ys = ?


-- Exercise 14: Append is associative
-- Concepts: Induction on lists
-- Difficulty: Medium
--
-- Prove: (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)

++-assoc : {A : Set} (xs ys zs : List A)
         → (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
++-assoc xs ys zs = ?


-- Exercise 15: Right identity of append
-- Concepts: Induction
-- Difficulty: Medium
--
-- Prove: xs ++ [] ≡ xs

++-identityʳ : {A : Set} (xs : List A) → xs ++ [] ≡ xs
++-identityʳ xs = ?


-- Exercise 16: Map preserves length
-- Concepts: Induction
-- Difficulty: Medium
--
-- Prove: length (map f xs) ≡ length xs

length-map : {A B : Set} (f : A → B) (xs : List A)
           → length (map f xs) ≡ length xs
length-map f xs = ?


-- Exercise 17: Map distributes over append
-- Concepts: Induction
-- Difficulty: Medium
--
-- Prove: map f (xs ++ ys) ≡ map f xs ++ map f ys

map-++ : {A B : Set} (f : A → B) (xs ys : List A)
       → map f (xs ++ ys) ≡ map f xs ++ map f ys
map-++ f xs ys = ?


------------------------------------------------------------------------
-- PART 5: Logic as Types
------------------------------------------------------------------------

-- The unit type (True - always provable)
data ⊤ : Set where
  tt : ⊤

-- The empty type (False - never provable)
data ⊥ : Set where
  -- no constructors!


-- Exercise 18: Ex falso quodlibet
-- Concepts: Empty type elimination
-- Difficulty: Easy
--
-- From false, anything follows
-- Hint: Pattern match on the ⊥ - use absurd pattern ()

⊥-elim : {A : Set} → ⊥ → A
⊥-elim x = ?


-- Negation: "not A" means "A implies false"
¬_ : Set → Set
¬ A = A → ⊥

infix 3 ¬_


-- Exercise 19: Zero is not successor
-- Concepts: Absurd patterns
-- Difficulty: Easy
--
-- Prove: 0 is not equal to suc n (for any n)
-- Hint: What happens when you pattern match on 0 ≡ suc n?

0≢suc : {n : Nat} → ¬ (0 ≡ suc n)
0≢suc p = ?


-- Exercise 20: Successor is injective
-- Concepts: Pattern matching on equality
-- Difficulty: Easy
--
-- If suc m ≡ suc n, then m ≡ n

suc-injective : {m n : Nat} → suc m ≡ suc n → m ≡ n
suc-injective p = ?


-- Product type (And)
data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

infixr 4 _,_
infixr 2 _×_


-- Exercise 21: First projection
-- Concepts: Pattern matching on pairs
-- Difficulty: Easy

proj₁ : {A B : Set} → A × B → A
proj₁ p = ?


-- Exercise 22: Second projection
-- Concepts: Pattern matching on pairs
-- Difficulty: Easy

proj₂ : {A B : Set} → A × B → B
proj₂ p = ?


-- Exercise 23: And is commutative
-- Concepts: Constructing pairs
-- Difficulty: Easy
--
-- If we have A × B, we can get B × A

×-comm : {A B : Set} → A × B → B × A
×-comm p = ?


-- Sum type (Or)
data _⊎_ (A B : Set) : Set where
  inj₁ : A → A ⊎ B
  inj₂ : B → A ⊎ B

infixr 1 _⊎_


-- Exercise 24: Or is commutative
-- Concepts: Pattern matching on sums
-- Difficulty: Easy
--
-- If we have A ⊎ B, we can get B ⊎ A

⊎-comm : {A B : Set} → A ⊎ B → B ⊎ A
⊎-comm x = ?


------------------------------------------------------------------------
-- PART 6: Equational Reasoning (Optional/Challenge)
------------------------------------------------------------------------

-- These combinators let us write proofs in a readable step-by-step style

begin_ : {A : Set} {x y : A} → x ≡ y → x ≡ y
begin p = p

_≡⟨_⟩_ : {A : Set} (x : A) {y z : A} → x ≡ y → y ≡ z → x ≡ z
x ≡⟨ p ⟩ q = trans p q

_≡⟨⟩_ : {A : Set} (x : A) {y : A} → x ≡ y → x ≡ y
x ≡⟨⟩ p = p

_∎ : {A : Set} (x : A) → x ≡ x
x ∎ = refl

infix  1 begin_
infixr 2 _≡⟨_⟩_ _≡⟨⟩_
infix  3 _∎


-- Exercise 25: Proof with equational reasoning
-- Concepts: Using the chain notation
-- Difficulty: Medium
--
-- Prove: (a + 0) + 0 ≡ a
-- Write it step by step using begin ... ∎

double-zero : (a : Nat) → (a + 0) + 0 ≡ a
double-zero a = ?


-- Exercise 26: Rearranging addition
-- Concepts: Using commutativity and associativity
-- Difficulty: Hard
--
-- Prove: a + (b + c) ≡ b + (a + c)
-- Hint: Use +-comm and +-assoc creatively

+-rearrange : (a b c : Nat) → a + (b + c) ≡ b + (a + c)
+-rearrange a b c = ?


------------------------------------------------------------------------
-- BONUS: Decidable Equality
------------------------------------------------------------------------

-- A decision: either we have a proof, or a refutation
data Dec (A : Set) : Set where
  yes : A → Dec A
  no  : ¬ A → Dec A


-- Exercise 27: Nat equality is decidable
-- Concepts: Recursion, decidability
-- Difficulty: Hard
--
-- For any m and n, we can decide whether m ≡ n

_≟_ : (m n : Nat) → Dec (m ≡ n)
m ≟ n = ?


------------------------------------------------------------------------
-- VERIFICATION
------------------------------------------------------------------------

-- If all exercises are complete, these should typecheck:

test-sym : 5 ≡ 5
test-sym = sym (sym refl)

test-trans : 3 ≡ 3
test-trans = trans refl refl

-- Uncomment after completing relevant exercises:
-- test-comm : 2 + 3 ≡ 3 + 2
-- test-comm = +-comm 2 3
