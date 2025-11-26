-- Recipe Book Manager
-- Your implementation goes here
-- Define your data types and implement all functions

module RecipeBook where

-- Define your custom data types here
-- Examples to get you started:

data Ingredient = Ingredient
    { ingredientName :: String
    , quantity :: Double
    , unit :: String
    } deriving (Show, Eq)

data Difficulty = Easy | Medium | Hard
    deriving (Show, Eq)

data DietaryTag = Vegetarian | Vegan | GlutenFree | DairyFree
    deriving (Show, Eq)

data Recipe = Recipe
    { recipeName :: String
    , recipeIngredients :: [Ingredient]
    , instructions :: [String]
    , servings :: Int
    , prepTime :: Int
    , difficulty :: Difficulty
    , dietaryTags :: [DietaryTag]
    } deriving (Show, Eq)

-- Implement your functions below
-- The test suite expects these function names:

-- 1. Scale an ingredient by a factor
scaleIngredient :: Double -> Ingredient -> Ingredient
scaleIngredient factor ing = Ingredient{quantity = quantity ing * factor}

-- 2. Scale a recipe (all ingredients and servings) by a factor
scaleRecipe :: Double -> Recipe -> Recipe
scaleRecipe factor recipe = recipe{recipeIngredients = map (\el -> scaleIngredient factor el) (recipeIngredients recipe), servings = servings recipe * factor }

-- 3. Find a recipe by name (case-insensitive)
findRecipeByName :: String -> [Recipe] -> Maybe Recipe
findRecipeByName name [] = Nothing
findRecipeByName name (x:xs) 
    | (recipeName x) == name = Just x
    | otherwise = findRecipeByName name xs 

-- 4. Find all recipes that contain a specific ingredient
findRecipesWithIngredient :: String -> [Recipe] -> [Recipe]
findRecipesWithIngredient ingredientName recipes = filter (\recipe -> elem ingredientName $ map (\ing -> ingredientName ing) (recipeIngredients recipe))

-- 5. Filter recipes by difficulty level
filterByDifficulty :: Difficulty -> [Recipe] -> [Recipe]
filterByDifficulty diff recipes = filter (\recipe -> difficulty recipe == diff) recipes

-- 6. Filter recipes that have ALL the specified dietary tags
filterByDietaryTags :: [DietaryTag] -> [Recipe] -> [Recipe]
filterByDietaryTags tags recipes = filter (\recipe -> dietaryTags recipe == tags) recipes

-- 7. Filter recipes by maximum preparation time
filterByMaxTime :: Int -> [Recipe] -> [Recipe]
filterByMaxTime maxTime recipes = filter (\recipe -> prepTime recipe <= maxTime) recipes 

-- 8. Combine two ingredient lists, merging quantities of the same ingredient
combineIngredients :: [Ingredient] -> [Ingredient] -> [Ingredient]
combineIngredients list1 list2 = let fullList = list1 ++ list2 
    in map (\list -> foldr (\el acc -> )) $ groupBy (\ing1 ing2 -> ingredientName ing1 == ingredientName ing2) $ sortOn (\ing -> ingredientName ing) fullList

-- 9. Generate a shopping list from multiple recipes (consolidated ingredients)
generateShoppingList :: [Recipe] -> [Ingredient]
generateShoppingList recipes = undefined -- TODO: implement

-- 10. Calculate total meal preparation time
totalMealTime :: [Recipe] -> Int
totalMealTime recipes = undefined -- TODO: implement

-- 11. Count recipes in a list (simple utility)
countRecipes :: [Recipe] -> Int
countRecipes recipes = undefined -- TODO: implement

-- Helper functions you might want to implement:
-- (These are not tested directly, but will help you)

-- Convert string to lowercase for case-insensitive comparison
-- You can use: import Data.Char (toLower)
-- toLowerCase :: String -> String

-- Check if an ingredient is in a recipe
-- hasIngredient :: String -> Recipe -> Bool

-- Check if a recipe has all specified dietary tags
-- hasAllTags :: [DietaryTag] -> Recipe -> Bool

-- Add more helper functions as needed!
