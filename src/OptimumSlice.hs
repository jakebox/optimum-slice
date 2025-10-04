{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}

module OptimumSlice where

import Data.List (foldl', intercalate, maximumBy, nub, sort, sortBy, subsequences)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Vector qualified as V

type Topping = String

-- | Vector of toppings (for preferences and domains)
type Toppings = V.Vector Topping

-- | Set of unique toppings for a quarter
type ToppingSet = S.Set Topping

-- | Rating for how optimal a given configuration is
type Score = Int

-- | A quarter of a pizza with unique toppings
newtype Quarter = Quarter ToppingSet
  deriving (Show, Eq, Ord)

-- | A pizza with exactly 4 quarters
data Pizza = Pizza Quarter Quarter Quarter Quarter
  deriving (Show, Eq)

data Preference = Preference
  { favoriteToppings :: Toppings
  , dislikedToppings :: Toppings
  , restrictedToppings :: Toppings
  , priority :: Int  -- Priority level for this person (higher = more important)
  }
  deriving (Show, Eq)

-- | Pizza division mode
data PizzaDivision = Halves | Quarters
  deriving (Show, Eq)

-- | Configuration for tuning the optimization algorithm
data OptimizationConfig = OptimizationConfig
  { -- Pizza structure
    pizzaDivision :: PizzaDivision  -- How to divide the pizza (default: Quarters)
  
  -- Scoring parameters
  , favoriteBonus :: Score          -- Points for favorite toppings (default: 10)
  , dislikePenalty :: Score         -- Penalty for disliked toppings (default: -10)  
  , restrictedPenalty :: Score      -- Penalty for restricted toppings (default: -100)
  , neutralBonus :: Score           -- Points for neutral toppings (default: 5)
  , cheeseBonus :: Score            -- Points for cheese (default: 0)
  
  -- Multi-section scoring
  , runnerUpBonus :: Double         -- Fraction of runner-up section score to add (default: 0.5)
  , thirdPlaceBonus :: Double       -- Fraction of third-best section score to add (default: 0.25)
  
  -- Diversity parameters  
  , diversityBonus :: Score         -- Bonus for having different sections (default: 0)
  , simplicityBonus :: Score        -- Bonus per section with fewer toppings (default: 0)
  , plainCheeseBonus :: Score       -- Bonus for having plain cheese options (default: 0)
  
  -- Algorithm parameters
  , bruteForceThreshold :: Int      -- Domain size limit for brute force (default: 6)
  , hillClimbRestarts :: Int        -- Random restarts for hill climbing (default: 5)
  , showTopK :: Int                 -- Number of top solutions to display (default: 1)
  }
  deriving (Show, Eq)

-- | Default optimization configuration
defaultConfig :: OptimizationConfig  
defaultConfig = OptimizationConfig
  { pizzaDivision = Quarters
  , favoriteBonus = 10
  , dislikePenalty = -10
  , restrictedPenalty = -100
  , neutralBonus = 5
  , cheeseBonus = 0
  , runnerUpBonus = 0.5
  , thirdPlaceBonus = 0.25
  , diversityBonus = 0
  , simplicityBonus = 0
  , plainCheeseBonus = 0
  , bruteForceThreshold = 6
  , hillClimbRestarts = 5
  , showTopK = 1
  }

toppingLimitPerQuarter :: Int
toppingLimitPerQuarter = 3

prettyQuarter :: Quarter -> String
prettyQuarter (Quarter toppingSet) = 
  let nonCheeseToppings = filter (/= "cheese") $ sort $ S.toList toppingSet
  in case nonCheeseToppings of
       [] -> "Plain cheese"
       _ -> intercalate ", " nonCheeseToppings

prettyPizza :: Pizza -> String
prettyPizza = prettyPizzaWithConfig defaultConfig

prettyPizzaWithConfig :: OptimizationConfig -> Pizza -> String
prettyPizzaWithConfig config (Pizza q1 q2 q3 q4) = 
  case pizzaDivision config of
    Quarters -> prettyPizzaQuarters q1 q2 q3 q4
    Halves -> prettyPizzaHalves q1 q2 q3 q4

-- | Pretty print pizza in quarters mode
prettyPizzaQuarters :: Quarter -> Quarter -> Quarter -> Quarter -> String
prettyPizzaQuarters q1 q2 q3 q4 = 
  let q1Text = prettyQuarter q1
      q2Text = prettyQuarter q2
      q3Text = prettyQuarter q3
      q4Text = prettyQuarter q4
      -- Calculate width based on longest topping list
      maxLen = maximum [length q1Text, length q2Text, length q3Text, length q4Text, 12]
      width = max 25 (maxLen + 3)  -- Ensure minimum width and padding
  in unlines
  [ "┌" ++ replicate width '─' ++ "┬" ++ replicate width '─' ++ "┐"
  , "│ Quarter 1" ++ replicate (width - 9) ' ' ++ "│ Quarter 2" ++ replicate (width - 9) ' ' ++ "│"
  , "│ " ++ padTo (width - 2) q1Text ++ " │ " ++ padTo (width - 2) q2Text ++ " │"
  , "├" ++ replicate width '─' ++ "┼" ++ replicate width '─' ++ "┤"
  , "│ Quarter 3" ++ replicate (width - 9) ' ' ++ "│ Quarter 4" ++ replicate (width - 9) ' ' ++ "│"
  , "│ " ++ padTo (width - 2) q3Text ++ " │ " ++ padTo (width - 2) q4Text ++ " │"
  , "└" ++ replicate width '─' ++ "┴" ++ replicate width '─' ++ "┘"
  ]
  where
    padTo n s = take n (s ++ repeat ' ')

-- | Pretty print pizza in halves mode
prettyPizzaHalves :: Quarter -> Quarter -> Quarter -> Quarter -> String
prettyPizzaHalves q1 q2 q3 q4 = 
  let leftHalfText = prettyHalf q1 q2
      rightHalfText = prettyHalf q3 q4
      -- Calculate width based on longest half text
      maxLen = maximum [length leftHalfText, length rightHalfText, 12]
      width = max 30 (maxLen + 3)  -- Ensure minimum width and padding
  in unlines
  [ "┌" ++ replicate width '─' ++ "┬" ++ replicate width '─' ++ "┐"
  , "│ Left Half" ++ replicate (width - 9) ' ' ++ "│ Right Half" ++ replicate (width - 10) ' ' ++ "│"
  , "│ " ++ padTo (width - 2) leftHalfText ++ " │ " ++ padTo (width - 2) rightHalfText ++ " │"
  , "└" ++ replicate width '─' ++ "┴" ++ replicate width '─' ++ "┘"
  ]
  where
    padTo n s = take n (s ++ repeat ' ')

prettyHalf :: Quarter -> Quarter -> String
prettyHalf (Quarter set1) (Quarter set2) = 
  let nonCheeseToppings = filter (/= "cheese") $ S.toList $ S.union set1 set2
  in case nonCheeseToppings of
       [] -> "Plain cheese"
       _ -> intercalate ", " nonCheeseToppings

prettyPreference :: Int -> Preference -> String
prettyPreference n pref = unlines
  [ "Person " ++ show n ++ " (Priority: " ++ show (priority pref) ++ "):"
  , "  Favorites: " ++ showToppings (favoriteToppings pref)
  , "  Dislikes:  " ++ showToppings (dislikedToppings pref)
  , "  Restricted:" ++ showToppings (restrictedToppings pref)
  ]
  where
    showToppings toppings 
      | V.null toppings = "none"
      | otherwise = intercalate ", " (V.toList toppings)

optimumSliceCli :: IO ()
optimumSliceCli = putStrLn "Pizza optimization CLI - not yet implemented"

optimumSliceBruteForce :: [Preference] -> Pizza
optimumSliceBruteForce = optimumSliceBruteForceWithConfig defaultConfig

optimumSliceBruteForceWithConfig :: OptimizationConfig -> [Preference] -> Pizza
optimumSliceBruteForceWithConfig config prefs = 
  case getTopKSolutions config prefs of
    [] -> error "No solutions found"
    (pizza, _):_ -> pizza

getTopKSolutions :: OptimizationConfig -> [Preference] -> [(Pizza, Score)]
getTopKSolutions config prefs = 
  let domain = toppingDomain prefs
      scoreWithPizza pizza = (pizza, scorePizzaGroup config prefs pizza)
      allPizzas = generateAllPizzas domain
  in take (showTopK config) $ sortBy (comparing (negate . snd)) $ map scoreWithPizza allPizzas

generateAllQuarters :: Toppings -> [Quarter]
generateAllQuarters toppings =
  let toppingsList = V.toList toppings
      validSubsets = [subset | subset <- subsequences toppingsList, 
                      length subset <= toppingLimitPerQuarter - 1]
  in map (Quarter . S.fromList . ("cheese":)) validSubsets

generateAllPizzas :: Toppings -> [Pizza]
generateAllPizzas toppings = 
  let quarters = generateAllQuarters toppings
  in [Pizza q1 q2 q3 q4 | q1 <- quarters, q2 <- quarters, q3 <- quarters, q4 <- quarters]

-- | Hill climbing optimization (original algorithm)
optimumSliceHillClimb :: [Preference] -> Pizza
optimumSliceHillClimb = optimumSliceHillClimbWithConfig defaultConfig

optimumSliceHillClimbWithConfig :: OptimizationConfig -> [Preference] -> Pizza
optimumSliceHillClimbWithConfig config prefs = bestResult
  where
    domain = toppingDomain prefs
    cheeseQuarter = Quarter $ S.singleton "cheese"
    startPizza = Pizza cheeseQuarter cheeseQuarter cheeseQuarter cheeseQuarter
    
    -- Try multiple starting points to avoid local optima
    candidateStarts = startPizza : generateInitialCandidates domain
    results = map (\start -> hillClimb start domain) candidateStarts
    bestResult = maximumBy (comparing (scorePizzaGroup config prefs)) results
    
    -- Generate some diverse starting points
    generateInitialCandidates toppings =
      let toppingsList = V.toList toppings
          numRestarts = hillClimbRestarts config
      in take numRestarts $ map createDiversePizza (subsequences toppingsList)
    
    createDiversePizza toppings = 
      let quarters = map (\t -> Quarter $ S.fromList ["cheese", t]) (take 4 toppings ++ repeat "cheese")
      in case quarters of
        (q1:q2:q3:q4:_) -> Pizza q1 q2 q3 q4
        _ -> startPizza
    
    hillClimb current toppings =
      let neighbors = generateNeighbors current toppings
          currentScore = scorePizzaGroup config prefs current
          neighborScores = map (\p -> (p, scorePizzaGroup config prefs p)) neighbors
          bestNeighbor = maximumBy (comparing snd) neighborScores
      in if null neighbors || snd bestNeighbor <= currentScore
         then current
         else hillClimb (fst bestNeighbor) toppings

optimumSlice :: [Preference] -> Pizza
optimumSlice = optimumSliceBruteForce

scoreTopping :: OptimizationConfig -> Preference -> Topping -> Score
scoreTopping config pref topping
  | V.elem topping pref.favoriteToppings = favoriteBonus config
  | V.elem topping pref.dislikedToppings = dislikePenalty config
  | V.elem topping pref.restrictedToppings = restrictedPenalty config
  | topping == "cheese" = cheeseBonus config
  | otherwise = neutralBonus config

scoreQuarter :: OptimizationConfig -> Preference -> Quarter -> Score
scoreQuarter config pref (Quarter toppingSet) = 
  let baseScore = sum $ map (scoreTopping config pref) $ S.toList toppingSet
      simplicityBonusPoints = if S.size toppingSet == 1 then simplicityBonus config else 0
      plainCheeseBonusPoints = if toppingSet == S.singleton "cheese" then plainCheeseBonus config else 0
  in baseScore + simplicityBonusPoints + plainCheeseBonusPoints

scorePizza :: OptimizationConfig -> Preference -> Pizza -> Score
scorePizza config pref (Pizza q1 q2 q3 q4) = 
  let sectionScores = case pizzaDivision config of
        Quarters -> [scoreQuarter config pref q1, scoreQuarter config pref q2, 
                    scoreQuarter config pref q3, scoreQuarter config pref q4]
        Halves -> [ -- Score halves as combined sections, not sum of quarters
                   scoreHalf config pref q1 q2,  -- Left half
                   scoreHalf config pref q3 q4   -- Right half
                  ]
      sortedScores = sortBy (comparing negate) sectionScores  -- Sort descending
      
      -- Calculate multi-section score: best + fraction of runner-up + fraction of third
      baseScore = case sortedScores of
        [] -> 0
        [best] -> best
        [best, second] -> best + round (runnerUpBonus config * fromIntegral second)
        [best, second, third] -> best + round (runnerUpBonus config * fromIntegral second) 
                                      + round (thirdPlaceBonus config * fromIntegral third)
        (best:second:third:_) -> best + round (runnerUpBonus config * fromIntegral second) 
                                      + round (thirdPlaceBonus config * fromIntegral third)
  in baseScore

scoreHalf :: OptimizationConfig -> Preference -> Quarter -> Quarter -> Score
scoreHalf config pref (Quarter set1) (Quarter set2) = 
  let combinedSet = S.union set1 set2
      baseScore = sum $ map (scoreTopping config pref) $ S.toList combinedSet
      simplicityBonusPoints = if S.size combinedSet == 1 then simplicityBonus config else 0
      plainCheeseBonusPoints = if combinedSet == S.singleton "cheese" then plainCheeseBonus config else 0
  in baseScore + simplicityBonusPoints + plainCheeseBonusPoints

-- | Calculate diversity bonus for a pizza
calculateDiversityBonus :: OptimizationConfig -> Pizza -> Score
calculateDiversityBonus config (Pizza q1 q2 q3 q4) =
  case pizzaDivision config of
    Quarters -> 
      let quarters = [q1, q2, q3, q4]
          uniqueQuarters = length $ nub quarters
          maxUnique = 4
      in (uniqueQuarters * diversityBonus config) `div` maxUnique
    Halves ->
      let leftHalf = [q1, q2]   -- Represent half as list of quarters
          rightHalf = [q3, q4]
          uniqueHalves = if leftHalf == rightHalf then 1 else 2
          maxUnique = 2
      in (uniqueHalves * diversityBonus config) `div` maxUnique

scorePizzaGroup :: OptimizationConfig -> [Preference] -> Pizza -> Score
scorePizzaGroup config prefs pizza = 
  let baseScore = foldl' (\acc p -> acc + (priority p * scorePizza config p pizza)) 0 prefs
      diversityBonusPoints = calculateDiversityBonus config pizza
  in baseScore + diversityBonusPoints

toppingDomain :: [Preference] -> Toppings
toppingDomain prefs = V.fromList $ nub $ concatMap (V.toList . favoriteToppings) prefs

-- | Given a pizza and a topping domain, generate pizzas that are one topping away.
generateNeighbors :: Pizza -> Toppings -> [Pizza]
generateNeighbors pizza toppings = addNeighbors ++ removeNeighbors
  where
    toppingsList = V.toList toppings
    
    -- Add each topping to each quarter (if not already present and under limit)
    addNeighbors = concatMap (\t -> addToppingToAllQuarters pizza t) toppingsList
    
    -- Remove each topping from each quarter (if present)
    removeNeighbors = concatMap (\t -> removeToppingFromAllQuarters pizza t) toppingsList

-- | Add a topping to each possible quarter, returning list of modified pizzas
addToppingToAllQuarters :: Pizza -> Topping -> [Pizza]
addToppingToAllQuarters (Pizza q1 q2 q3 q4) topping =
  catMaybes [addToQ1, addToQ2, addToQ3, addToQ4]
  where
    addToQ1 = (\q -> Pizza q q2 q3 q4) <$> addToppingToQuarter q1 topping
    addToQ2 = (\q -> Pizza q1 q q3 q4) <$> addToppingToQuarter q2 topping
    addToQ3 = (\q -> Pizza q1 q2 q q4) <$> addToppingToQuarter q3 topping
    addToQ4 = (\q -> Pizza q1 q2 q3 q) <$> addToppingToQuarter q4 topping

-- | Remove a topping from each possible quarter, returning list of modified pizzas
removeToppingFromAllQuarters :: Pizza -> Topping -> [Pizza]
removeToppingFromAllQuarters (Pizza q1 q2 q3 q4) topping =
  catMaybes [removeFromQ1, removeFromQ2, removeFromQ3, removeFromQ4]
  where
    removeFromQ1 = (\q -> Pizza q q2 q3 q4) <$> removeToppingFromQuarter q1 topping
    removeFromQ2 = (\q -> Pizza q1 q q3 q4) <$> removeToppingFromQuarter q2 topping
    removeFromQ3 = (\q -> Pizza q1 q2 q q4) <$> removeToppingFromQuarter q3 topping
    removeFromQ4 = (\q -> Pizza q1 q2 q3 q) <$> removeToppingFromQuarter q4 topping

-- | Add a topping to a quarter if possible (not present, under limit)
addToppingToQuarter :: Quarter -> Topping -> Maybe Quarter
addToppingToQuarter (Quarter toppingSet) topping
  | S.member topping toppingSet = Nothing  -- Already present
  | S.size toppingSet >= toppingLimitPerQuarter = Nothing  -- At limit
  | otherwise = Just $ Quarter $ S.insert topping toppingSet

-- | Remove a topping from a quarter if present, ensuring cheese remains
removeToppingFromQuarter :: Quarter -> Topping -> Maybe Quarter
removeToppingFromQuarter (Quarter toppingSet) topping
  | not (S.member topping toppingSet) = Nothing  -- Not present
  | otherwise = 
      let newSet = S.delete topping toppingSet
          finalSet = if S.null newSet then S.singleton "cheese" else newSet
      in Just $ Quarter finalSet

