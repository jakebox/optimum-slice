module Main where

import OptimumSlice
import Data.Vector qualified as V

-- Example group preferences
person1 :: Preference
person1 = Preference
  { favoriteToppings = V.fromList ["onion"]
  , dislikedToppings = V.fromList ["olive", "pepperoni", "sausage"]
  , restrictedToppings = V.fromList []
  }

person2 :: Preference
person2 = Preference
  { favoriteToppings = V.fromList ["green pepper", "mushroom", "olive", "pepperoni"]
  , dislikedToppings = V.fromList ["pepperoni"]
  , restrictedToppings = V.fromList []
  }

person3 :: Preference
person3 = Preference
  { favoriteToppings = V.fromList ["pepperoni", "sausage"]
  , dislikedToppings = V.fromList ["mushroom", "onion"]
  , restrictedToppings = V.fromList ["pineapple"]
  }

groupPrefs :: [Preference]
groupPrefs = [person1, person2, person3]

main :: IO ()
main = do
  putStrLn "Finding optimal pizza for the group..."
  putStrLn $ "Group preferences: " ++ show groupPrefs
  putStrLn ""
  
  let optimalPie = optimumSlice groupPrefs
      totalScore = scorePieGroup groupPrefs optimalPie
      
  putStrLn $ "Optimal pizza: " ++ show optimalPie
  putStrLn $ "Total group score: " ++ show totalScore
