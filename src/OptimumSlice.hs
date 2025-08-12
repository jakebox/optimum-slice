{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}

module OptimumSlice where

import Data.Bool
import Data.List
import Data.Ord (comparing)
import Data.Vector qualified as V

type Topping = String

type Toppings = V.Vector Topping

-- | Rating for how optimal a given configuration is
type Score = Int

-- | Representation of a half of a pizza, which itself
-- is made of either two more halves, or a single element.
-- A section can contain multiple toppings.
data Half a = Section a | HalfSection (Half a) (Half a)
  deriving Show

instance Eq a => Eq (Half a) where
  HalfSection a b == HalfSection c d = [a, b] == [c, d] || [a, b] == [d, c]
  Section x == Section y = x == y
  _ == _ = False

data Pie = Pie (Half Toppings) (Half Toppings)
  deriving Show

instance Eq Pie where
  Pie a b == Pie c d = [a, b] == [c, d] || [a, b] == [d, c]

data Preference = Preference
  { favoriteToppings :: Toppings
  , dislikedToppings :: Toppings
  , restrictedToppings :: Toppings
  }
  deriving (Show, Eq)

toppingLimitPerSection :: Int
toppingLimitPerSection = 3

optimumSliceCli :: IO optimumSliceCli
optimumSliceCli = undefined

optimumSlice :: [Preference] -> Pie
optimumSlice prefs = hillClimb startPie domain
  where
    domain = toppingDomain prefs
    startPie = Pie (Section $ V.singleton "cheese") (Section $ V.singleton "cheese")
    
    hillClimb current toppings =
      let neighbors = generateNeighbors current toppings
          currentScore = scorePieGroup prefs current
          neighborScores = map (\p -> (p, scorePieGroup prefs p)) neighbors
          bestNeighbor = maximumBy (comparing snd) neighborScores
      in if null neighbors || snd bestNeighbor <= currentScore
         then current
         else hillClimb (fst bestNeighbor) toppings

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
toppingDomain prefs = noDup $ foldl' (\acc p -> acc V.++ p.favoriteToppings) V.empty prefs 
  where
    noDup = V.fromList . nub . V.toList

-- | Apply a half transformation to either side of a pie
applyToPie :: (Half Toppings -> [Half Toppings]) -> Pie -> [Pie]
applyToPie f (Pie left right) = leftMods ++ rightMods
  where
    leftMods = map (\l' -> Pie l' right) (f left)
    rightMods = map (\r' -> Pie left r') (f right)

-- | Given a pie and a topping domain, generate pies that are one topping away.
generateNeighbors :: Pie -> Toppings -> [Pie]
generateNeighbors pie toppings = addNeighbors ++ removeNeighbors ++ replaceNeighbors
  where
    toppingsList = V.toList toppings
    
    -- Add each topping to either half
    addNeighbors = concatMap (\t -> applyToPie (`toppingAddAllSections` t) pie) toppingsList
    
    -- Remove each topping from individual sections
    removeNeighbors = concatMap (\t -> applyToPie (`toppingRemoveAllSections` t) pie) toppingsList
    
    -- Replace each topping with each other topping on both halves (only if different)
    replaceNeighbors = concatMap (\new -> concatMap (\old -> 
      if old == new then []
      else let newPie = Pie (toppingReplace left old new) (toppingReplace right old new)
           in if newPie == pie then [] else [newPie]) toppingsList) toppingsList
      where (Pie left right) = pie

-- | Replace a topping with another topping.
toppingReplace :: Half Toppings -> Topping -> Topping -> Half Toppings
toppingReplace (HalfSection l r) t t' = HalfSection (toppingReplace l t t') (toppingReplace r t t')
toppingReplace (Section s) t t' = Section $ V.map (\x -> bool x t' (x == t)) s

-- | Generate all possible modifications to a half by adding a new topping,
-- unless the topping limit has been reached.
toppingAddAllSections :: Half Toppings -> Topping -> [Half Toppings]
toppingAddAllSections (HalfSection l r) t = left ++ right
  where
    -- reconstruct halves with one modified side and one existing side
    left = map (\l' -> HalfSection l' r) (toppingAddAllSections l t) -- modified versions of left
    right = map (\r' -> HalfSection l r') (toppingAddAllSections r t) -- modified versions of right
toppingAddAllSections (Section s) t
  | V.elem t s = []
  | V.length s < toppingLimitPerSection = [Section (s `V.snoc` t)] -- Add the topping into the section
  | otherwise = []

-- | Remove a given topping from a half.
toppingRemove :: Half Toppings -> Topping -> Half Toppings
toppingRemove (HalfSection l r) t = HalfSection (toppingRemove l t) (toppingRemove r t)
toppingRemove (Section s) t = Section . ensureCheese . V.filter (/= t) $ s
  where
    ensureCheese v
      | V.null v = V.singleton "cheese"
      | otherwise = v

-- | Generate all possible modifications to a half by removing a topping from
-- individual sections (if the topping exists in that section).
toppingRemoveAllSections :: Half Toppings -> Topping -> [Half Toppings]
toppingRemoveAllSections (HalfSection l r) t = left ++ right
  where
    left = map (\l' -> HalfSection l' r) (toppingRemoveAllSections l t)
    right = map (\r' -> HalfSection l r') (toppingRemoveAllSections r t)
toppingRemoveAllSections (Section s) t
  | V.elem t s = [Section . ensureCheese . V.filter (/= t) $ s]
  | otherwise = []
  where
    ensureCheese v
      | V.null v = V.singleton "cheese"
      | otherwise = v
