# Week 6: Agda Basics

## What is Agda?

Agda is a **dependently typed** programming language and proof assistant. Think of it as Haskell with a much more powerful type system where:

- **Types can depend on values** (not just other types)
- **The typechecker is a theorem prover** - if your code compiles, it's correct by construction
- **Totality is enforced** - all functions must terminate and handle all cases

### Why Learn Agda After Haskell?

| Haskell | Agda |
|---------|------|
| Types and values are separate worlds | Types and values live in the same world |
| Runtime errors possible (`head []`) | No runtime errors - rejected at compile time |
| Partial functions allowed | Total functions required |
| Type inference is powerful | More explicit type annotations needed |
| `undefined` exists | No escape hatches |

---

## Installation

### Option 1: Stack (Recommended for Windows)
```bash
# If you have Stack from Haskell
stack install Agda

# Verify installation
agda --version
```

### Option 2: Cabal
```bash
cabal update
cabal install Agda
```

### Standard Library Setup
```bash
# Clone the standard library
git clone https://github.com/agda/agda-stdlib.git

# Create ~/.agda/libraries file containing path to standard-library.agda-lib
# Create ~/.agda/defaults file containing: standard-library
```

### Editor Setup (VS Code Recommended)
1. Install "agda-mode" extension in VS Code
2. Key commands (after opening .agda file):
   - `C-c C-l` - Load/typecheck file
   - `C-c C-c` - Case split on variable
   - `C-c C-r` - Refine hole (fill in obvious parts)
   - `C-c C-a` - Auto-fill hole
   - `C-c C-,` - Show goal type and context
   - `C-c C-.` - Show goal type and inferred type of expression

---

## Basic Syntax: Haskell vs Agda

### Module Declaration

**Haskell:**
```haskell
module MyModule where
```

**Agda:**
```agda
module MyModule where
```
Same! But in Agda, the filename must match the module name.

### Data Types

**Haskell:**
```haskell
data Bool = False | True

data Nat = Zero | Succ Nat

data List a = Nil | Cons a (List a)
```

**Agda:**
```agda
data Bool : Set where
  false : Bool
  true  : Bool

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

data List (A : Set) : Set where
  []  : List A
  _∷_ : A → List A → List A
```

Key differences:
- Constructors have **explicit type signatures**
- `Set` is Agda's equivalent of Haskell's `*` (the type of types)
- Underscores `_` mark where arguments go (mixfix operators)
- Unicode is common: `→` instead of `->`, `∷` instead of `:`

### Functions

**Haskell:**
```haskell
not :: Bool -> Bool
not False = True
not True  = False

add :: Nat -> Nat -> Nat
add Zero     m = m
add (Succ n) m = Succ (add n m)
```

**Agda:**
```agda
not : Bool → Bool
not false = true
not true  = false

_+_ : Nat → Nat → Nat
zero    + m = m
(suc n) + m = suc (n + m)
```

Key differences:
- Single colon `:` for type signatures (not `::`)
- Unicode arrow `→` (or you can use `->`)
- Mixfix operators with `_` placeholders
- Constructors are lowercase by convention

### Pattern Matching

**Haskell:**
```haskell
length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs
```

**Agda:**
```agda
length : {A : Set} → List A → Nat
length []       = zero
length (_ ∷ xs) = suc (length xs)
```

The `{A : Set}` is an **implicit argument** - Agda will infer it from context.

---

## Dependent Types: The Big Idea

In Haskell, types can depend on other types:
```haskell
data Maybe a = Nothing | Just a  -- 'a' is a type parameter
```

In Agda, types can depend on **values**:
```agda
-- A vector with its length in the type!
data Vec (A : Set) : Nat → Set where
  []  : Vec A zero
  _∷_ : {n : Nat} → A → Vec A n → Vec A (suc n)
```

This means:
- `Vec Bool zero` is the type of empty boolean vectors
- `Vec Bool (suc (suc zero))` is the type of boolean vectors with exactly 2 elements
- You literally **cannot** call `head` on an empty vector - the types prevent it!

### Safe Head Function

**Haskell (unsafe):**
```haskell
head :: [a] -> a
head (x:_) = x
head []    = error "empty list"  -- Runtime crash!
```

**Agda (safe by construction):**
```agda
head : {A : Set} {n : Nat} → Vec A (suc n) → A
head (x ∷ _) = x
-- No empty case needed - Vec A (suc n) can never be empty!
```

The type `Vec A (suc n)` guarantees at least one element. The typechecker won't even let you try to handle the empty case.

---

## Totality

Agda requires all functions to be **total**:

1. **Coverage** - All possible inputs must be handled
2. **Termination** - The function must terminate on all inputs

### Termination Checking

Agda checks that recursive calls are on **structurally smaller** arguments:

```agda
-- This terminates: n gets smaller each call
_+_ : Nat → Nat → Nat
zero    + m = m
(suc n) + m = suc (n + m)  -- 'n' is smaller than 'suc n'

-- This would be REJECTED:
bad : Nat → Nat
bad n = bad n  -- Not getting smaller!
```

---

## Implicit vs Explicit Arguments

**Explicit arguments:** `(x : A)` - must be provided
**Implicit arguments:** `{x : A}` - Agda infers them

```agda
-- Both A and n are implicit - Agda figures them out
length : {A : Set} {n : Nat} → Vec A n → Nat
length []       = zero
length (_ ∷ xs) = suc (length xs)

-- Usage: just call it, Agda infers the rest
example : Nat
example = length (true ∷ false ∷ [])  -- Returns: suc (suc zero)
```

To explicitly provide an implicit argument, use braces:
```agda
length {Bool} {suc (suc zero)} (true ∷ false ∷ [])
```

---

## Holes: Interactive Development

Agda's killer feature is **holes** - placeholders that the typechecker helps you fill:

```agda
myFunc : Nat → Nat
myFunc n = ?  -- This is a hole!
```

When you load the file (`C-c C-l`), Agda will tell you:
- The **goal type** (what type the hole needs to be)
- The **context** (what variables are in scope and their types)

You can then:
- `C-c C-c n` - Case split on variable `n`
- `C-c C-r` - Refine (if there's an obvious next step)
- `C-c C-a` - Auto-solve (if Agda can figure it out)

This is like pair programming with the typechecker!

---

## Worked Example 1: Natural Numbers

Let's define natural numbers and basic operations:

```agda
module Nat-Example where

-- Natural numbers
data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

-- Addition
_+_ : Nat → Nat → Nat
zero    + m = m
(suc n) + m = suc (n + m)

-- Multiplication
_*_ : Nat → Nat → Nat
zero    * m = zero
(suc n) * m = m + (n * m)

-- Example values
one : Nat
one = suc zero

two : Nat
two = suc one

three : Nat
three = suc two

-- Test: 2 + 2 = 4
test-add : Nat
test-add = two + two  -- Evaluates to: suc (suc (suc (suc zero)))
```

---

## Worked Example 2: Length-Indexed Vectors

```agda
module Vec-Example where

open import Nat-Example  -- Use our Nat

-- Vector: a list that knows its length
data Vec (A : Set) : Nat → Set where
  []  : Vec A zero
  _∷_ : {n : Nat} → A → Vec A n → Vec A (suc n)

-- Safe head - only works on non-empty vectors
head : {A : Set} {n : Nat} → Vec A (suc n) → A
head (x ∷ _) = x

-- Safe tail
tail : {A : Set} {n : Nat} → Vec A (suc n) → Vec A n
tail (_ ∷ xs) = xs

-- Concatenation - lengths add up in the type!
_++_ : {A : Set} {m n : Nat} → Vec A m → Vec A n → Vec A (m + n)
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

-- Example
example-vec : Vec Nat three
example-vec = one ∷ two ∷ three ∷ []

-- This typechecks: head of non-empty vector
first : Nat
first = head example-vec  -- Returns: one

-- This would NOT typecheck:
-- bad = head []  -- Error: Vec A (suc n) ≠ Vec A zero
```

---

## Worked Example 3: Finite Numbers (Bounded Naturals)

```agda
module Fin-Example where

open import Nat-Example

-- Fin n represents numbers 0, 1, ..., n-1
-- Fin zero is empty (no natural is less than zero)
-- Fin (suc n) has one more element than Fin n
data Fin : Nat → Set where
  zero : {n : Nat} → Fin (suc n)           -- 0 < suc n
  suc  : {n : Nat} → Fin n → Fin (suc n)   -- if i < n, then suc i < suc n

-- Example: Fin three has elements 0, 1, 2
fin-zero : Fin three
fin-zero = zero

fin-one : Fin three
fin-one = suc zero

fin-two : Fin three
fin-two = suc (suc zero)

-- This would NOT typecheck:
-- fin-three : Fin three
-- fin-three = suc (suc (suc zero))  -- Error! Would need Fin (suc three)

-- Safe indexing into vectors!
lookup : {A : Set} {n : Nat} → Vec A n → Fin n → A
lookup (x ∷ _)  zero    = x
lookup (_ ∷ xs) (suc i) = lookup xs i
-- No case for [] needed - Fin zero is empty!
```

---

## Common Gotchas Moving from Haskell

1. **Unicode matters:** Use `→` not `->`, `∷` not `:` for cons (though ASCII works too)

2. **Lowercase constructors:** Convention is `zero`, `suc`, `true`, `false`

3. **No type inference for everything:** You'll write more type signatures

4. **Termination checker:** Your recursive calls must obviously decrease

5. **No laziness:** Agda is strict by default

6. **Holes are your friend:** Use `?` liberally and let Agda guide you

---

## Unicode Input (VS Code with agda-mode)

| Symbol | Input Sequence |
|--------|----------------|
| `→` | `\to` or `\->` or `\r` |
| `∷` | `\::` |
| `λ` | `\Gl` or `\lambda` |
| `∀` | `\all` or `\forall` |
| `≡` | `\==` |
| `ℕ` | `\bN` |
| `₁` | `\_1` |
| `¹` | `\^1` |

---

## Next Steps

1. Make sure Agda is installed and working
2. Try the exercises in `exercises.agda`
3. Use holes (`?`) extensively - let Agda guide you!
4. Focus on getting things to typecheck - that's the goal

The exercises start simple and gradually introduce dependent types.
