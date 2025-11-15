# Functional Programming - Syllabus

**Course Code:** CSE3100
**Academic Year:** 2024-2025
**Credits:** 5 ECTS
**Institution:** TU Delft

## Course Description

This course provides a comprehensive introduction to functional programming, focusing on two major functional programming languages: Haskell and Agda. Students will learn the principles of functional programming, including immutability, pure functions, higher-order functions, and advanced type systems. The course emphasizes both practical programming skills and theoretical foundations.

## Philosophy

Functional programming represents a fundamentally different paradigm from imperative and object-oriented programming. This course aims to shift students' thinking from "how to do something" (imperative) to "what something is" (declarative). Students will learn to think in terms of transformations, compositions, and mathematical functions rather than sequential state changes.

### Why Study Functional Programming?

- **High-level abstraction:** Model the real world at a high level to develop software that is clear, concise, and correct
- **Better programming skills:** Learning FP teaches new ways to think about programs, making you a better programmer in any language
- **Growing relevance:** FP languages are increasingly commonplace, and mainstream languages are integrating more functional features (lambdas, higher-order functions, pattern matching, immutable data)
- **Program correctness:** Pure functional programming makes it easy to reason about correctness using equational reasoning, property-based testing, and formal verification
- **Confidence in refactoring:** Static typing and immutability allow quick changes and refactoring with confidence
- **Future of programming:** FP languages like Haskell and Agda include recent developments from PL research that typically take 10-30 years to reach mainstream languages

## Learning Objectives

### Haskell
By the end of this course, students will be able to:
- Write idiomatic Haskell programs using functional programming principles
- **Manipulate and return functions as first-class data** (first-class functions)
- Understand and apply core functional concepts:
  - Immutability and referential transparency
  - Pure functions and side effect management
  - Pattern matching and recursion
  - Higher-order functions (map, filter, fold, etc.)
  - Lazy evaluation
- **Define and manipulate custom data structures** using algebraic datatypes, pattern matching, and recursion
- Work effectively with Haskell's type system:
  - Type inference and type signatures
  - Algebraic data types
  - Type classes and polymorphism
  - Type parameters for generic programming
- **Automatically test important properties** of programs using QuickCheck (property-based testing)
- **Avoid code duplication** and write code at higher levels of abstraction using type parameters and type classes
- **Program with infinite data structures** by relying on lazy evaluation and avoiding unnecessary computation
- Apply functional design patterns:
  - Functors and Applicatives
  - Monads for managing effects
  - Function composition and point-free style
- **Distinguish between pure and impure computations** and use monads to make this difference explicit
- Understand list comprehensions and their applications

### Agda
Students will also learn to:
- **Express logical properties as types** through the Curry-Howard correspondence
- **Develop Agda programs interactively** with the typechecker and detect errors while writing the program
- **Enforce invariants at the type level** by using indexed datatypes and dependent pattern matching
- **Formally prove properties** of purely functional programs using the identity type and equational reasoning
- Work with dependent type systems
- Apply proof-driven development techniques
- Verify program correctness through types

## Teaching Methods

### Lectures
- **Format:** In-person lectures covering theoretical concepts and practical demonstrations
- **Focus:** Core functional programming principles, language features, and design patterns
- **Interactive:** Live coding sessions and problem-solving exercises

### Lab Sessions
- **Hands-on Practice:** Guided programming exercises in Haskell and Agda
- **Problem Sets:** Weekly exercises reinforcing lecture concepts
- **Code Reviews:** Peer review and instructor feedback on solutions

### Self-Study
- **Textbook Readings:** Systematic progression through course materials
- **Programming Practice:** Regular coding to build functional thinking muscle memory
- **Online Resources:** Supplementary materials and documentation

## Contact Hours

- **Lectures:** 2 hours per week
- **Lab Sessions:** 2 hours per week
- **Self-Study:** Approximately 10-12 hours per week
- **Total Workload:** ~140 hours (5 ECTS × 28 hours)

## Assessment

### Project (35%)
- **Type:** Programming project (nature TBD - may be individual or group)
- **Objective:** Design and implement a significant functional program
- **Skills Assessed:**
  - Practical application of functional programming concepts
  - Code quality and style
  - Problem-solving approach
  - Documentation and testing

### Written Exam (65%)
- **Format:** Closed-book exam
- **Content:**
  - Theoretical concepts and definitions
  - Code reading and comprehension
  - Problem-solving (write functions to specifications)
  - Type inference and type system questions
  - Understanding of functional design patterns
- **Skills Assessed:**
  - Deep understanding of functional programming principles
  - Ability to reason about functional code
  - Knowledge of Haskell syntax and semantics

**Total:** 100%

## Required Literature

### Primary Textbook
**Graham Hutton** - *Programming in Haskell*, 2nd Edition
- Comprehensive introduction to Haskell programming
- Clear explanations with numerous examples
- Progressive difficulty with exercises

### Key Chapters (Recommended Preparation Sequence)
1. **Chapter 1:** Introduction
2. **Chapter 2:** First steps
3. **Chapter 3:** Types and classes
4. **Chapter 4:** Defining functions
5. **Chapter 5:** List comprehensions
6. **Chapter 6:** Recursive functions
7. **Chapter 7:** Higher-order functions
8. **Chapter 8:** Declaring types and classes
9. **Chapter 9-15:** Advanced topics (monads, functors, lazy evaluation, etc.)

### Additional Resources
- Haskell documentation and Hoogle (function search)
- Learn You a Haskell for Great Good! (online resource)
- Real World Haskell (for practical applications)
- Agda documentation and tutorials

## Prerequisites

### Recommended Background
- **Programming Experience:** Solid foundation in at least one programming language
- **Data Structures:** Understanding of lists, trees, and basic algorithms
- **Mathematical Maturity:** Comfort with mathematical notation and recursive thinking
- **Logical Reasoning:** Ability to think abstractly and reason about program behavior

### Specific Skills
- Basic understanding of:
  - Variables and functions
  - Data types and type systems
  - Recursion (helpful but will be covered extensively)
  - Algorithm analysis (basic complexity)

## Course Topics

### Part 1: Foundations (Weeks 1-3)
- Introduction to functional programming paradigm
- Haskell basics: expressions, functions, types
- List operations and pattern matching
- Recursion fundamentals

### Part 2: Core Concepts (Weeks 4-5)
- Higher-order functions
- List comprehensions
- Custom type definitions
- Type classes and polymorphism

### Part 3: Advanced Functional Programming (Weeks 6-7)
- Functors and Applicatives
- Monads and do-notation
- Error handling in functional style
- Lazy evaluation and infinite structures
- Property-based testing with QuickCheck

### Part 4: Dependent Types and Agda (Week 8)
- Introduction to Agda
- Dependent type systems
- Curry-Howard correspondence
- Interactive development with the Agda typechecker
- Indexed datatypes and dependent pattern matching
- Identity type and equational reasoning
- Proofs as programs

### Recommended Setup
- Linux, macOS, or WSL on Windows for best compatibility
- Command-line familiarity for GHCi (interactive Haskell)

## Study Recommendations

### For Q2 Preparation
1. **Install Haskell environment early** (GHC + tooling)
2. **Work through Hutton chapters 1-8** systematically
3. **Code regularly** (minimum 2x per week) to build functional thinking
4. **Complete end-of-chapter exercises** - they're essential
5. **Focus on understanding, not memorizing** - this is about paradigm shift
6. **Practice recursion extensively** - it's fundamental to FP
7. **Don't create flashcards** - this course requires doing, not memorization

### Success Tips
- **Think in transformations:** What is the result, not how to compute it
- **Start small:** Build complex functions from simple, composable pieces
- **Use types:** Let the type system guide your implementation
- **Embrace recursion:** It replaces loops in functional programming
- **Read code:** Understanding others' Haskell improves your own
- **Ask for help:** Functional programming requires a mental shift - seek support when stuck
