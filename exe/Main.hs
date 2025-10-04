module Main where

import OptimumSlice
import Options.Applicative
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Vector qualified as V

-- Example group preferences
person1 :: Preference
person1 = Preference
  { favoriteToppings = V.fromList ["onion"]
  , dislikedToppings = V.fromList ["black olive", "pepperoni", "sausage", "mushroom"]
  , restrictedToppings = V.fromList []
  , priority = 1
  }

person2 :: Preference
person2 = Preference
  { favoriteToppings = V.fromList ["black olive", "mushroom", "onion", "pepperoni", "giardinara"]
  , dislikedToppings = V.fromList ["sausage", "green olive", "anchovies", "pineapple", "ham"]
  , restrictedToppings = V.fromList []
  , priority = 1
  }

person3 :: Preference
person3 = Preference
  { favoriteToppings = V.fromList ["onion", "mushroom", "pepperoni", "sausage"]
  , dislikedToppings = V.fromList []
  , restrictedToppings = V.fromList []
  , priority = 1
  }

person4 :: Preference
person4 = Preference
  { favoriteToppings = V.fromList ["black olive", "green olive"]
  , dislikedToppings = V.fromList ["onion", "mushroom", "peppers", "sausage", "ham", "anchovies", "pineapple"]
  , restrictedToppings = V.fromList []
  , priority = 1
  }

groupPrefs :: [Preference]
groupPrefs = [person1, person2, person3, person4]

-- Command line options
data Options = Options
  { optDivision :: PizzaDivision
  , optDiversityBonus :: Int
  , optSimplicityBonus :: Int
  , optRunnerUpBonus :: Double
  , optBruteForceThreshold :: Int
  , optShowTopK :: Int
  } deriving Show

optionsParser :: Parser Options
optionsParser = Options
  <$> option readDivision
      ( long "division"
     <> short 'd'
     <> metavar "DIVISION"
     <> value Halves
     <> help "Pizza division (quarters|halves)" )
  <*> option auto
      ( long "diversity"
     <> metavar "INT"
     <> value 9
     <> help "Diversity bonus points" )
  <*> option auto
      ( long "simplicity"
     <> metavar "INT"
     <> value 5
     <> help "Simplicity bonus points" )
  <*> option auto
      ( long "runner-up"
     <> metavar "DOUBLE"
     <> value 0.8
     <> help "Runner-up bonus multiplier" )
  <*> option auto
      ( long "threshold"
     <> short 't'
     <> metavar "INT"
     <> value 12
     <> help "Brute force threshold" )
  <*> option auto
      ( long "top-k"
     <> short 'k'
     <> metavar "INT"
     <> value 1
     <> help "Number of top solutions to show" )

readDivision :: ReadM PizzaDivision
readDivision = eitherReader $ \s -> case s of
  "quarters" -> Right Quarters
  "halves" -> Right Halves
  _ -> Left "Invalid division. Use 'quarters' or 'halves'"

toConfig :: Options -> OptimizationConfig
toConfig opts = defaultConfig
  { pizzaDivision = optDivision opts
  , diversityBonus = optDiversityBonus opts
  , simplicityBonus = optSimplicityBonus opts
  , runnerUpBonus = optRunnerUpBonus opts
  , bruteForceThreshold = optBruteForceThreshold opts
  , showTopK = optShowTopK opts
  }

main :: IO ()
main = do
  opts <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "Optimize pizza toppings for a group"
   <> header "optimum-slice - pizza optimization calculator" )
  
  let config = toConfig opts
  runOptimization config

runOptimization :: OptimizationConfig -> IO ()
runOptimization config = do
  putStrLn "OPTIMUM SLICE - Pizza Optimization Calculator"
  putStrLn "=============================================="
  putStrLn ""
  
  putStrLn "Group Preferences:"
  putStrLn "------------------"
  mapM_ putStrLn $ zipWith prettyPreference [1..] groupPrefs
  
  showConfig config
  
  let domain = toppingDomain groupPrefs
      domainSize = V.length domain
      threshold = bruteForceThreshold config
      
  putStrLn $ "Domain size: " ++ show domainSize ++ " toppings"
  
  if domainSize <= threshold
    then runBruteForce config
    else runHillClimbing config

showConfig :: OptimizationConfig -> IO ()
showConfig config = do
  putStrLn "Configuration:"
  putStrLn "--------------"
  putStrLn $ "Pizza division: " ++ show (pizzaDivision config)
  putStrLn $ "Diversity bonus: " ++ show (diversityBonus config)
  putStrLn $ "Simplicity bonus: " ++ show (simplicityBonus config)
  putStrLn $ "Runner-up bonus: " ++ show (runnerUpBonus config) ++ "x"
  putStrLn ""

runBruteForce :: OptimizationConfig -> IO ()
runBruteForce config = do
  putStrLn "Algorithm: Brute Force (Global Optimum)"
  putStrLn "Optimizing pizza..."
  let topSolutions = getTopKSolutions config groupPrefs
  putStrLn "OPTIMAL PIZZAS FOUND!"
  putStrLn "========================"
  showTopKSolutions config topSolutions

runHillClimbing :: OptimizationConfig -> IO ()
runHillClimbing config = do
  putStrLn "Algorithm: Hill Climbing (Local Optimum)"
  putStrLn "Optimizing pizza..."
  let optimalPizza = optimumSliceHillClimbWithConfig config groupPrefs
      totalScore = scorePizzaGroup config groupPrefs optimalPizza
  putStrLn "OPTIMAL PIZZA FOUND!"
  putStrLn "======================"
  putStr $ prettyPizzaWithConfig config optimalPizza
  putStrLn $ "Total Group Score: " ++ show totalScore
  showIndividualScores config optimalPizza

showTopKSolutions :: OptimizationConfig -> [(Pizza, Score)] -> IO ()
showTopKSolutions config solutions = 
  mapM_ showSolution $ zip [1..] solutions
  where
    showSolution (i, (pizza, score)) = do
      putStrLn $ "SOLUTION " ++ show (i :: Int) ++ " (Score: " ++ show score ++ ")"
      putStrLn $ replicate 40 '-'
      putStr $ prettyPizzaWithConfig config pizza
      showIndividualScores config pizza
      putStrLn ""

showIndividualScores :: OptimizationConfig -> Pizza -> IO ()
showIndividualScores config pizza = do
  let divisionName = case pizzaDivision config of
        Quarters -> "quarters"
        Halves -> "halves"
  putStrLn $ "People can eat multiple " ++ divisionName ++ "!"
  putStrLn ""
  putStrLn $ "Individual Analysis (Multi-" ++ divisionName ++ " Scoring):"
  putStrLn "--------------------------------------------"
  mapM_ (showPersonScore config pizza) $ zip [1..] groupPrefs

showPersonScore :: OptimizationConfig -> Pizza -> (Int, Preference) -> IO ()
showPersonScore config pizza (i, pref) = do
  let totalScore = scorePizza config pref pizza
      weightedScore = priority pref * totalScore
      rankings = getSectionRankings config pref pizza
  putStrLn $ "Person " ++ show i ++ ": " ++ show totalScore ++ " total × " ++ show (priority pref) ++ " = " ++ show weightedScore ++ " points"
  mapM_ showRanking rankings
  putStrLn ""
  where
    showRanking (rank, desc, score, bonus) = 
      putStrLn $ "  " ++ rank ++ ": " ++ desc ++ " = " ++ show score ++ bonus

getSectionRankings :: OptimizationConfig -> Preference -> Pizza -> [(String, String, Score, String)]
getSectionRankings config pref (Pizza q1 q2 q3 q4) =
  case pizzaDivision config of
    Quarters -> rankSections quarterChoices quarterRankNames
      where
        quarterChoices = [("Quarter " ++ show (i :: Int) ++ " (" ++ prettyQuarter q ++ ")", scoreQuarter config pref q) 
                         | (i, q) <- zip [1..] [q1, q2, q3, q4]]
        quarterRankNames = ["1st choice", "2nd choice", "3rd choice", "4th choice"]
    
    Halves -> rankSections halfChoices halfRankNames
      where
        halfChoices = [("Left Half (" ++ prettyHalfLocal q1 q2 ++ ")", scoreHalf config pref q1 q2),
                      ("Right Half (" ++ prettyHalfLocal q3 q4 ++ ")", scoreHalf config pref q3 q4)]
        halfRankNames = ["1st choice", "2nd choice"]
  where
    rankSections choices rankNames = 
      let sorted = sortBy (comparing (negate . snd)) choices
          addBonus i (desc, score) = 
            let bonus = case i of
                  0 -> " points"
                  1 -> " points + " ++ show (runnerUpBonus config) ++ "x bonus"
                  2 -> " points + " ++ show (thirdPlaceBonus config) ++ "x bonus"
                  _ -> " points (no bonus)"
                rank = if i < length rankNames then rankNames !! i else "Choice"
            in (rank, desc, score, bonus)
      in zipWith addBonus [0..] sorted

prettyHalfLocal :: Quarter -> Quarter -> String
prettyHalfLocal (Quarter set1) (Quarter set2) = 
  let combinedSet = S.union set1 set2
      nonCheeseToppings = filter (/= "cheese") $ S.toList combinedSet
  in case nonCheeseToppings of
       [] -> "Plain cheese"
       _ -> intercalate ", " nonCheeseToppings
