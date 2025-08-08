module Main where

import OptimumSlice
import Data.Vector qualified as V

samplePreference :: Preference
samplePreference = Preference
  { favoriteToppings = V.fromList ["pepperoni", "mushroom"]
  , dislikedToppings = V.fromList ["pineapple", "olive"]
  , restrictedToppings = V.fromList ["seafood"]
  }

samplePie :: Pie
samplePie = Pie (HalfSection (Leaf "pepperoni") (Leaf "mushroom")) 
              (HalfSection (Leaf "cheese") (Leaf "pineapple"))

test :: IO ()
test = do
  putStrLn "Testing the scorer with sample data:"
  putStrLn $ "Sample preference: " ++ show samplePreference
  putStrLn $ "Sample pie: " ++ show samplePie
  putStrLn $ "Score: " ++ show (scorePie samplePreference samplePie)

main :: IO ()
main = test -- Replace this with: optimumSlice =<< optimumSliceCli
