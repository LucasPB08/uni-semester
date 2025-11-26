-- Test suite for Recipe Book Manager
-- Load this file after implementing project.hs
-- Run tests in GHCi with: runAllTests

module RecipeTests where

import RecipeBook

-- Test data
eggs = Ingredient "eggs" 2.0 "whole"
flour = Ingredient "flour" 200.0 "grams"
milk = Ingredient "milk" 250.0 "ml"
sugar = Ingredient "sugar" 50.0 "grams"
butter = Ingredient "butter" 30.0 "grams"
cheese = Ingredient "cheese" 100.0 "grams"
pasta = Ingredient "pasta" 300.0 "grams"
tomatoes = Ingredient "tomatoes" 4.0 "whole"
garlic = Ingredient "garlic" 2.0 "cloves"
basil = Ingredient "basil" 10.0 "leaves"

pancakes = Recipe
    "Pancakes"
    [eggs, flour, milk, sugar, butter]
    ["Mix dry ingredients", "Add wet ingredients", "Cook on griddle"]
    4
    15
    Easy
    [Vegetarian]

pastaCarbonara = Recipe
    "Pasta Carbonara"
    [pasta, eggs, cheese, garlic]
    ["Boil pasta", "Mix eggs and cheese", "Combine with hot pasta"]
    2
    25
    Medium
    []

tomatoSalad = Recipe
    "Tomato Basil Salad"
    [tomatoes, basil, Ingredient "olive oil" 2.0 "tbsp"]
    ["Chop tomatoes", "Add basil", "Drizzle with oil"]
    2
    10
    Easy
    [Vegetarian, Vegan, GlutenFree]

veganPasta = Recipe
    "Vegan Pasta Primavera"
    [pasta, tomatoes, garlic, basil, Ingredient "olive oil" 3.0 "tbsp"]
    ["Boil pasta", "Sauté vegetables", "Combine"]
    3
    30
    Medium
    [Vegan, Vegetarian]

allRecipes = [pancakes, pastaCarbonara, tomatoSalad, veganPasta]

-- Test utilities
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual testName expected actual =
    if expected == actual
    then putStrLn $ "✓ " ++ testName
    else putStrLn $ "✗ " ++ testName ++ "\n  Expected: " ++ show expected ++ "\n  Got: " ++ show actual

assertTrue :: String -> Bool -> IO ()
assertTrue testName condition =
    if condition
    then putStrLn $ "✓ " ++ testName
    else putStrLn $ "✗ " ++ testName ++ " - Expected True, got False"

-- Ingredient Tests
testScaleIngredient :: IO ()
testScaleIngredient = do
    putStrLn "\n=== Testing Ingredient Scaling ==="
    let scaled = scaleIngredient 2.0 eggs
    assertEqual "Scale eggs by 2.0" (Ingredient "eggs" 4.0 "whole") scaled

    let halfScaled = scaleIngredient 0.5 flour
    assertEqual "Scale flour by 0.5" (Ingredient "flour" 100.0 "grams") halfScaled

-- Recipe Tests
testScaleRecipe :: IO ()
testScaleRecipe = do
    putStrLn "\n=== Testing Recipe Scaling ==="
    let scaledPancakes = scaleRecipe 2.0 pancakes
    assertEqual "Scaled recipe servings" 8 (servings scaledPancakes)
    assertEqual "Scaled recipe name unchanged" "Pancakes" (recipeName scaledPancakes)

    -- Check that first ingredient (eggs) is scaled correctly
    case recipeIngredients scaledPancakes of
        (firstIng:_) -> assertEqual "First ingredient scaled" (Ingredient "eggs" 4.0 "whole") firstIng
        [] -> putStrLn "✗ Scaled recipe has no ingredients"

-- Search Tests
testFindRecipeByName :: IO ()
testFindRecipeByName = do
    putStrLn "\n=== Testing Recipe Search by Name ==="
    let found = findRecipeByName "Pancakes" allRecipes
    case found of
        Just r -> assertEqual "Found recipe name" "Pancakes" (recipeName r)
        Nothing -> putStrLn "✗ Should find Pancakes"

    let notFound = findRecipeByName "Pizza" allRecipes
    assertEqual "Recipe not in list" Nothing notFound

    -- Test case-insensitive search
    let foundLower = findRecipeByName "pancakes" allRecipes
    case foundLower of
        Just r -> assertEqual "Case-insensitive search" "Pancakes" (recipeName r)
        Nothing -> putStrLn "✗ Should find Pancakes (case-insensitive)"

testFindRecipesWithIngredient :: IO ()
testFindRecipesWithIngredient = do
    putStrLn "\n=== Testing Recipe Search by Ingredient ==="
    let withEggs = findRecipesWithIngredient "eggs" allRecipes
    assertEqual "Recipes with eggs count" 2 (length withEggs)

    let withPasta = findRecipesWithIngredient "pasta" allRecipes
    assertEqual "Recipes with pasta count" 2 (length withPasta)

    let withChicken = findRecipesWithIngredient "chicken" allRecipes
    assertEqual "Recipes with chicken count" 0 (length withChicken)

-- Filter Tests
testFilterByDifficulty :: IO ()
testFilterByDifficulty = do
    putStrLn "\n=== Testing Filter by Difficulty ==="
    let easyRecipes = filterByDifficulty Easy allRecipes
    assertEqual "Easy recipes count" 2 (length easyRecipes)

    let mediumRecipes = filterByDifficulty Medium allRecipes
    assertEqual "Medium recipes count" 2 (length mediumRecipes)

    let hardRecipes = filterByDifficulty Hard allRecipes
    assertEqual "Hard recipes count" 0 (length hardRecipes)

testFilterByDietaryTags :: IO ()
testFilterByDietaryTags = do
    putStrLn "\n=== Testing Filter by Dietary Tags ==="
    let vegetarianRecipes = filterByDietaryTags [Vegetarian] allRecipes
    assertEqual "Vegetarian recipes count" 3 (length vegetarianRecipes)

    let veganRecipes = filterByDietaryTags [Vegan] allRecipes
    assertEqual "Vegan recipes count" 2 (length veganRecipes)

    -- Test multiple tags (must have ALL)
    let veganAndGlutenFree = filterByDietaryTags [Vegan, GlutenFree] allRecipes
    assertEqual "Vegan AND GlutenFree count" 1 (length veganAndGlutenFree)

testFilterByMaxTime :: IO ()
testFilterByMaxTime = do
    putStrLn "\n=== Testing Filter by Max Time ==="
    let quick = filterByMaxTime 20 allRecipes
    assertEqual "Recipes under 20 min" 2 (length quick)

    let all30 = filterByMaxTime 30 allRecipes
    assertEqual "Recipes under 30 min" 4 (length all30)

    let none = filterByMaxTime 5 allRecipes
    assertEqual "Recipes under 5 min" 0 (length none)

-- Shopping List Tests
testCombineIngredients :: IO ()
testCombineIngredients = do
    putStrLn "\n=== Testing Combine Ingredients ==="
    let list1 = [eggs, flour]
    let list2 = [Ingredient "eggs" 3.0 "whole", sugar]
    let combined = combineIngredients list1 list2

    -- Should have 3 unique ingredients: eggs (combined), flour, sugar
    assertEqual "Combined list length" 3 (length combined)

    -- Find the eggs ingredient and check quantity
    let eggsInList = filter (\i -> ingredientName i == "eggs") combined
    case eggsInList of
        [combinedEggs] -> assertEqual "Eggs combined quantity" 5.0 (quantity combinedEggs)
        _ -> putStrLn "✗ Should have exactly one eggs entry"

testGenerateShoppingList :: IO ()
testGenerateShoppingList = do
    putStrLn "\n=== Testing Generate Shopping List ==="
    let recipes = [pancakes, pastaCarbonara]
    let shoppingList = generateShoppingList recipes

    -- Eggs appear in both recipes (2 + 2 = 4)
    let eggsInList = filter (\i -> ingredientName i == "eggs") shoppingList
    case eggsInList of
        [combinedEggs] -> assertEqual "Shopping list eggs" 4.0 (quantity combinedEggs)
        _ -> putStrLn "✗ Should have exactly one eggs entry in shopping list"

    -- Pasta appears in only one recipe
    let pastaInList = filter (\i -> ingredientName i == "pasta") shoppingList
    assertEqual "Pasta in shopping list" 1 (length pastaInList)

-- Utility Tests
testTotalMealTime :: IO ()
testTotalMealTime = do
    putStrLn "\n=== Testing Total Meal Time ==="
    let total = totalMealTime [pancakes, tomatoSalad]
    assertEqual "Total time for 2 recipes" 25 total

    let zeroTime = totalMealTime []
    assertEqual "Total time for no recipes" 0 zeroTime

testCountRecipes :: IO ()
testCountRecipes = do
    putStrLn "\n=== Testing Count Recipes ==="
    assertEqual "Count all recipes" 4 (countRecipes allRecipes)
    assertEqual "Count empty list" 0 (countRecipes [])

-- Edge Cases
testEdgeCases :: IO ()
testEdgeCases = do
    putStrLn "\n=== Testing Edge Cases ==="

    -- Scaling by 0
    let scaledZero = scaleRecipe 0 pancakes
    assertEqual "Scale by 0 servings" 0 (servings scaledZero)

    -- Empty recipe list searches
    assertEqual "Search empty list" Nothing (findRecipeByName "test" [])
    assertEqual "Filter empty list" 0 (length $ filterByDifficulty Easy [])

    -- Shopping list from empty recipes
    assertEqual "Shopping list from empty" 0 (length $ generateShoppingList [])

-- Run all tests
runAllTests :: IO ()
runAllTests = do
    putStrLn "=========================================="
    putStrLn "  Recipe Book Manager - Test Suite"
    putStrLn "=========================================="

    testScaleIngredient
    testScaleRecipe
    testFindRecipeByName
    testFindRecipesWithIngredient
    testFilterByDifficulty
    testFilterByDietaryTags
    testFilterByMaxTime
    testCombineIngredients
    testGenerateShoppingList
    testTotalMealTime
    testCountRecipes
    testEdgeCases

    putStrLn "\n=========================================="
    putStrLn "  Tests Complete!"
    putStrLn "=========================================="

-- Quick test runner for individual test groups
runSearchTests :: IO ()
runSearchTests = do
    testFindRecipeByName
    testFindRecipesWithIngredient

runFilterTests :: IO ()
runFilterTests = do
    testFilterByDifficulty
    testFilterByDietaryTags
    testFilterByMaxTime

runShoppingTests :: IO ()
runShoppingTests = do
    testCombineIngredients
    testGenerateShoppingList
