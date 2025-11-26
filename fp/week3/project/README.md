# Mini-Project: Recipe Book Manager

## Overview
Build a recipe book management system that allows you to store recipes, search for them, calculate ingredient quantities, and generate shopping lists. This project integrates concepts from weeks 1-3: custom data types, pattern matching, recursion, list operations, and pure functional thinking.

## Problem Description

You're building a command-line recipe book manager. The system should:
1. Store recipes with their ingredients and instructions
2. Search recipes by name or ingredient
3. Scale recipes (adjust ingredient quantities for different serving sizes)
4. Generate a shopping list from multiple recipes
5. Filter recipes by various criteria (cooking time, difficulty, dietary restrictions)
6. Calculate total preparation time for a meal plan

## Custom Data Types

You'll need to define your own data types. Think about:
- **Recipe**: What information defines a recipe?
  - Name, ingredients list, instructions, servings, prep time, difficulty level, dietary tags
- **Ingredient**: How do you represent an ingredient with quantity?
  - Name, amount, unit (cups, tablespoons, grams, etc.)
- **Difficulty**: Easy, Medium, Hard
- **DietaryTag**: Vegetarian, Vegan, GlutenFree, DairyFree, etc.

Consider deriving useful type classes like `Show` and `Eq` for your types.

## Suggested Structure

Here's a suggested breakdown of functions you might want to implement. **You decide the exact type signatures and implementation details.**

### Core Data Operations
1. **Recipe creation helper** - Make it easy to construct recipes
2. **Add ingredient to recipe** - Add a new ingredient to an existing recipe
3. **Scale ingredient** - Multiply ingredient quantity by a factor
4. **Scale recipe** - Scale all ingredients in a recipe for different servings

### Search and Filter Functions
5. **Find recipe by name** - Search through a list of recipes for a specific name (case-insensitive)
6. **Find recipes with ingredient** - Get all recipes that contain a specific ingredient
7. **Filter by difficulty** - Get recipes matching a difficulty level
8. **Filter by dietary tags** - Get recipes matching ALL specified dietary requirements
9. **Filter by max time** - Get recipes that can be made within a time limit

### Shopping List Functions
10. **Extract ingredients** - Get all ingredients from a single recipe
11. **Combine ingredients** - Merge ingredient lists, combining quantities of the same ingredient
12. **Generate shopping list** - Take multiple recipes and produce a consolidated shopping list

### Utility Functions
13. **Recipe summary** - Generate a one-line summary of a recipe
14. **Total meal time** - Calculate combined prep time for a list of recipes
15. **Count recipes** - Count how many recipes match certain criteria

## Implementation Tips

### Pattern Matching
- Use pattern matching to destructure your custom data types
- Handle empty lists vs non-empty lists explicitly
- Consider using guards for conditional logic

### Recursion
- Most list operations will be recursive
- Think about base cases (empty list) and recursive cases (x:xs)
- For combining/merging operations, you'll need to compare elements

### List Operations
- You can use basic list operations: `map`, `filter`, `concat`, `null`, `elem`, etc.
- String operations: `words`, `unwords`, comparison functions
- For case-insensitive string matching, consider converting to lowercase

### Type Thinking
- Keep functions pure - no IO in the core logic
- Make type signatures explicit to help the compiler help you
- Think about what types naturally belong together

## Scala Comparison

In Scala, you might use:
```scala
case class Recipe(name: String, ingredients: List[Ingredient], ...)
sealed trait Difficulty
case object Easy extends Difficulty
```

In Haskell, this becomes:
```haskell
data Recipe = Recipe { name :: String, ingredients :: [Ingredient], ... }
data Difficulty = Easy | Medium | Hard deriving (Show, Eq)
```

Key difference: Haskell's record syntax with named fields vs Scala's case class parameters.

## Example Test Scenarios

Your implementation should handle:
1. **Scaling**: A recipe for 4 servings scaled to 6 servings
2. **Search**: Finding "Pasta Carbonara" in a list of 10 recipes
3. **Shopping list**: Combining 3 recipes where some share ingredients (e.g., multiple recipes need "eggs")
4. **Filtering**: Finding all vegetarian recipes that take less than 30 minutes
5. **Edge cases**: Empty recipe lists, recipes with no ingredients, scaling by factor 0

## Getting Started

1. Start by defining your data types in `project.hs`
2. Implement helper functions for creating recipes and ingredients
3. Build up the search and filter functions
4. Tackle the shopping list generation (most complex part)
5. Run tests frequently with `ghci` and the test file

## File Structure

```
week3/project/
├── README.md (this file)
├── project.hs (your implementation)
└── tests.hs (test suite)
```

Load your code in GHCi:
```bash
ghci project.hs
```

Run tests (after implementing the test functions):
```bash
ghci tests.hs
```

Good luck! Remember: this is about bringing together pattern matching, recursion, custom types, and list operations. Take your time to think through the design.
