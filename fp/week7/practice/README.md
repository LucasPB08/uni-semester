# Week 7: Propositions as Types

## The Curry-Howard Correspondence

The **Curry-Howard correspondence** (also called "propositions as types" or "proofs as programs") is one of the most profound ideas in computer science:

| Logic | Type Theory |
|-------|-------------|
| Proposition | Type |
| Proof | Program (term) |
| Proposition is true | Type is inhabited (has a value) |
| Proposition is false | Type is empty (no values) |
| A implies B | Function A → B |
| A and B | Product type (A × B) or (A , B) |
| A or B | Sum type (A ⊎ B) |
| For all x, P(x) | Dependent function (x : A) → P x |
| There exists x, P(x) | Dependent pair Σ A P |
| False | Empty type ⊥ |
| True | Unit type ⊤ |

**The key insight:** To prove a proposition, you construct a program of the corresponding type. If you can write the program and it typechecks, you've proven the theorem!

---

## The Identity Type (Propositional Equality)

The most fundamental proposition is **equality**. In Agda, we express "a equals b" as `a ≡ b`.

### Defining Equality

```agda
-- The identity type: "a equals b"
data _≡_ {A : Set} : A → A → Set where
  refl : {x : A} → x ≡ x
```

This says:
- `_≡_` is a type that takes two values of the same type
- The **only** way to construct a proof of equality is `refl` (reflexivity)
- `refl` proves that any value equals itself: `x ≡ x`

### Reading Equality Types

| Type | Meaning |
|------|---------|
| `3 ≡ 3` | "3 equals 3" (provable with `refl`) |
| `(1 + 2) ≡ 3` | "1 + 2 equals 3" (provable - Agda computes both sides) |
| `n + 0 ≡ n` | "n + 0 equals n" (requires proof by induction) |
| `0 ≡ 1` | "0 equals 1" (unprovable - type is empty) |

### Why `refl` Works for `(1 + 2) ≡ 3`

Agda **normalizes** (computes) both sides before comparing:
- Left side: `1 + 2` → `suc (0 + 2)` → `suc 2` → `3`
- Right side: `3`
- Both are `3`, so `refl` applies!

```agda
-- This typechecks because Agda computes 1 + 2 = 3
proof₁ : (1 + 2) ≡ 3
proof₁ = refl

-- This also works - Agda computes both sides
proof₂ : (2 * 3) ≡ (1 + 5)
proof₂ = refl
```

---

## Basic Equality Proofs

### Symmetry: If a ≡ b, then b ≡ a

```agda
sym : {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl
```

**How does this work?**
- We pattern match on the proof `x ≡ y`
- The only constructor is `refl`, which requires `x` and `y` to be the same
- After matching `refl`, Agda knows `x` and `y` are identical
- So we need to prove `x ≡ x`, which is just `refl`!

### Transitivity: If a ≡ b and b ≡ c, then a ≡ c

```agda
trans : {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl
```

After matching both `refl`s, Agda knows `x = y = z`, so we prove `x ≡ x`.

### Congruence: If a ≡ b, then f(a) ≡ f(b)

```agda
cong : {A B : Set} {x y : A} → (f : A → B) → x ≡ y → f x ≡ f y
cong f refl = refl
```

This is crucial! It lets us apply functions to both sides of an equality.

**Example usage:**
```agda
-- If n ≡ m, then suc n ≡ suc m
example : {n m : Nat} → n ≡ m → suc n ≡ suc m
example eq = cong suc eq
```

---

## Equational Reasoning

For complex proofs, we chain equalities step by step. Agda's standard library provides nice syntax for this, but here's a simple version:

```agda
-- Begin a chain
begin_ : {A : Set} {x y : A} → x ≡ y → x ≡ y
begin p = p

-- Chain: x ≡ y, then show y ≡ z
_≡⟨_⟩_ : {A : Set} (x : A) {y z : A} → x ≡ y → y ≡ z → x ≡ z
x ≡⟨ p ⟩ q = trans p q

-- End the chain (just reflexivity)
_∎ : {A : Set} (x : A) → x ≡ x
x ∎ = refl
```

### Example: Proving (a + 0) + 0 ≡ a

Assuming we have `+-identity` : `n + 0 ≡ n`:

```agda
example : (a : Nat) → (a + 0) + 0 ≡ a
example a =
  begin
    (a + 0) + 0
  ≡⟨ cong (_+ 0) (+-identity a) ⟩
    a + 0
  ≡⟨ +-identity a ⟩
    a
  ∎
```

Each step shows what we're transforming and why.

---

## Proving Properties by Induction

For properties involving recursively-defined data (like Nat or List), we use **induction** - which in Agda is just **recursion**!

### Example: n + 0 ≡ n (Right Identity of Addition)

```agda
-- Recall how + is defined:
-- zero  + m = m
-- suc n + m = suc (n + m)

+-identityʳ : (n : Nat) → n + 0 ≡ n
+-identityʳ zero = refl                    -- Base: 0 + 0 ≡ 0 ✓
+-identityʳ (suc n) = cong suc (+-identityʳ n)  -- Step: use IH
```

**Breaking down the inductive case:**
- We need to prove: `suc n + 0 ≡ suc n`
- By definition of `+`: `suc n + 0` computes to `suc (n + 0)`
- So we need: `suc (n + 0) ≡ suc n`
- By induction hypothesis: `n + 0 ≡ n`
- Apply `cong suc`: `suc (n + 0) ≡ suc n` ✓

### Example: 0 + n ≡ n (Left Identity)

This one is trivial because of how `+` is defined:

```agda
+-identityˡ : (n : Nat) → 0 + n ≡ n
+-identityˡ n = refl  -- 0 + n computes to n by definition!
```

---

## Worked Example 1: Addition is Commutative

Let's prove `m + n ≡ n + m`. This requires a helper lemma.

### Lemma: suc distributes right

```agda
+-suc : (m n : Nat) → m + suc n ≡ suc (m + n)
+-suc zero n = refl                     -- 0 + suc n = suc n = suc (0 + n)
+-suc (suc m) n = cong suc (+-suc m n)  -- IH + cong
```

### Main Theorem: Addition is Commutative

```agda
+-comm : (m n : Nat) → m + n ≡ n + m
+-comm zero n = sym (+-identityʳ n)     -- 0 + n ≡ n ≡ n + 0
+-comm (suc m) n =
  begin
    suc m + n
  ≡⟨ refl ⟩                             -- by definition
    suc (m + n)
  ≡⟨ cong suc (+-comm m n) ⟩            -- IH
    suc (n + m)
  ≡⟨ sym (+-suc n m) ⟩                  -- use lemma backwards
    n + suc m
  ∎
```

---

## Worked Example 2: List Properties

### Length distributes over append

```agda
length-++ : {A : Set} (xs ys : List A)
          → length (xs ++ ys) ≡ length xs + length ys
length-++ [] ys = refl
length-++ (x ∷ xs) ys = cong suc (length-++ xs ys)
```

### Map preserves length

```agda
length-map : {A B : Set} (f : A → B) (xs : List A)
           → length (map f xs) ≡ length xs
length-map f [] = refl
length-map f (x ∷ xs) = cong suc (length-map f xs)
```

---

## Worked Example 3: Vector Properties

With vectors, some properties are "free" due to the types!

```agda
-- The TYPE already guarantees the length is correct
-- No proof needed - it's in the type signature
vmap-length : {A B : Set} {n : Nat} (f : A → B) (xs : Vec A n)
            → Vec B n  -- Output has same length n - guaranteed!
vmap-length = vmap
```

But we can still prove equalities between vectors:

```agda
-- Map with identity function returns the same vector
vmap-id : {A : Set} {n : Nat} (xs : Vec A n) → vmap (λ x → x) xs ≡ xs
vmap-id [] = refl
vmap-id (x ∷ xs) = cong (x ∷_) (vmap-id xs)
```

---

## Logic in Agda

### True and False

```agda
-- True: has exactly one proof
data ⊤ : Set where
  tt : ⊤

-- False: has no proofs (empty type)
data ⊥ : Set where
  -- no constructors!

-- From false, anything follows (ex falso quodlibet)
⊥-elim : {A : Set} → ⊥ → A
⊥-elim ()  -- absurd pattern: no way to construct ⊥
```

### Negation

```agda
-- "Not A" means "A implies False"
¬_ : Set → Set
¬ A = A → ⊥

-- Example: 0 is not equal to 1
0≢1 : ¬ (0 ≡ 1)
0≢1 ()  -- No way to construct 0 ≡ 1, so pattern is absurd
```

### Conjunction (And)

```agda
-- Product type represents "and"
data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

-- To prove A × B, prove both A and B
example-and : (1 ≡ 1) × (2 ≡ 2)
example-and = refl , refl
```

### Disjunction (Or)

```agda
-- Sum type represents "or"
data _⊎_ (A B : Set) : Set where
  inj₁ : A → A ⊎ B
  inj₂ : B → A ⊎ B

-- To prove A ⊎ B, prove either A or B
example-or : (1 ≡ 1) ⊎ (1 ≡ 2)
example-or = inj₁ refl  -- We prove the left side
```

---

## The Absurd Pattern

When pattern matching reveals an impossible case, use `()`:

```agda
-- A vector of length zero cannot have a head
no-head : {A : Set} → Vec A 0 → A
no-head ()  -- No constructor matches Vec A 0 with a non-empty pattern

-- Zero is not the successor of anything
zero-not-suc : {n : Nat} → 0 ≡ suc n → ⊥
zero-not-suc ()  -- 0 ≡ suc n is uninhabited
```

---

## Summary: Proof Strategies

| Goal | Strategy |
|------|----------|
| Prove `x ≡ x` | Use `refl` |
| Prove `a ≡ b` where both compute to same value | Use `refl` |
| Prove `f x ≡ f y` given `x ≡ y` | Use `cong f` |
| Prove `y ≡ x` given `x ≡ y` | Use `sym` |
| Prove `x ≡ z` given `x ≡ y` and `y ≡ z` | Use `trans` |
| Prove property for all Nat | Induction (recursion on Nat) |
| Prove property for all List | Induction (recursion on List) |
| Handle impossible case | Absurd pattern `()` |
| Chain multiple steps | Equational reasoning `begin ... ∎` |

---

## Unicode Input Reference

| Symbol | Input |
|--------|-------|
| `≡` | `\==` |
| `⟨` | `\<` |
| `⟩` | `\>` |
| `∎` | `\qed` |
| `⊤` | `\top` |
| `⊥` | `\bot` |
| `¬` | `\neg` |
| `×` | `\times` or `\x` |
| `⊎` | `\uplus` or `\u+` |
| `ʳ` | `\^r` |
| `ˡ` | `\^l` |
| `₁` | `\_1` |
| `₂` | `\_2` |

---

## Next Steps

1. Complete the exercises in `exercises.agda`
2. Focus on understanding how pattern matching on `refl` works
3. Practice inductive proofs - they're just recursive functions!
4. Use holes (`?`) liberally - let Agda guide you

Remember: **If it typechecks, it's correct!**
