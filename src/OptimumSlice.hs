{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module OptimumSlice where

import Data.List
import Data.Vector qualified as V

type Topping = String

type Toppings = V.Vector Topping

-- | Rating for how optimal a given configuration is
type Score = Int

-- | Representation of a half of a pizza, which itself
-- is made of either two more halves, or a single element.
-- A section can contain multiple toppings.
data Half a = Section a | HalfSection (Half a) (Half a)
    deriving (Show)

instance Eq a => Eq (Half a) where
    HalfSection a b == HalfSection c d = [a, b] == [c, d] || [a, b] == [d, c]
    Section x == Section y = x == y
    _ == _ = False

data Pie = Pie (Half Toppings) (Half Toppings)
    deriving (Show)

instance Eq Pie where
    Pie a b == Pie c d = [a, b] == [c, d] || [a, b] == [d, c]

data Preference = Preference
    { favoriteToppings :: Toppings
    , dislikedToppings :: Toppings
    , restrictedToppings :: Toppings
    }
    deriving (Show, Eq)

optimumSliceCli :: IO optimumSliceCli
optimumSliceCli = undefined

optimumSlice :: [Preference] -> Pie
optimumSlice = undefined

scoreTopping :: Preference -> Topping -> Score
scoreTopping pref topping
    | V.elem topping pref.favoriteToppings = 10
    | V.elem topping pref.dislikedToppings = -10
    | V.elem topping pref.restrictedToppings = -100
    | topping == "cheese" = 0
    | otherwise = 5

scoreHalf :: Preference -> Half Toppings -> Score
scoreHalf pref (Section s) = V.foldl' (\acc t -> acc + scoreTopping pref t) 0 s
scoreHalf pref (HalfSection left right) = scoreHalf pref left + scoreHalf pref right

scorePie :: Preference -> Pie -> Score
scorePie pref (Pie left right) = max (score left) (score right)
    where
        score = scoreHalf pref

scorePieGroup :: [Preference] -> Pie -> Score
scorePieGroup prefs pie = foldl' (\acc p -> acc + scorePie p pie) 0 prefs

toppingDomain :: [Preference] -> Toppings
toppingDomain = foldl' (\acc p -> acc V.++ p.favoriteToppings) V.empty

-- | Given a pie and a topping domain, generate pies that are one topping away.
generateNeighbors :: Pie -> Toppings -> [Pie]
generateNeighbors pie toppings = undefined

toppingReplace :: Half Toppings -> Toppings -> Half Toppings
toppingReplace half toppings = undefined

toppingRemove :: Half Toppings -> Toppings -> Half Toppings
toppingRemove half toppings = undefined
