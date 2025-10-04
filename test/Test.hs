module Main where

import Test.Tasty
import Test.Tasty.HUnit

import OptimumSlice
import Data.Set qualified as S
import Data.Vector qualified as V

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, typeTests, neighborTests]

neighborTests :: TestTree
neighborTests = testGroup "Neighbor tests"
  [ testCase "add topping to quarter" $
      addToppingToQuarter (Quarter $ S.singleton "cheese") "pepperoni"
      @?= Just (Quarter $ S.fromList ["cheese", "pepperoni"])
  , testCase "add duplicate topping to quarter" $
      addToppingToQuarter (Quarter $ S.fromList ["cheese", "pepperoni"]) "pepperoni"
      @?= Nothing
  , testCase "add topping to full quarter" $
      addToppingToQuarter (Quarter $ S.fromList ["cheese", "pepperoni", "mushroom"]) "onion"
      @?= Nothing
  , testCase "remove topping from quarter" $
      removeToppingFromQuarter (Quarter $ S.fromList ["cheese", "pepperoni"]) "pepperoni"
      @?= Just (Quarter $ S.singleton "cheese")
  , testCase "remove non-existent topping from quarter" $
      removeToppingFromQuarter (Quarter $ S.singleton "cheese") "pepperoni"
      @?= Nothing
  , testCase "remove last topping ensures cheese remains" $
      removeToppingFromQuarter (Quarter $ S.singleton "pepperoni") "pepperoni"
      @?= Just (Quarter $ S.singleton "cheese")
  , testCase "generateNeighbors basic" $
      length (generateNeighbors simplePizza (V.fromList ["pepperoni", "mushroom"])) @?= 8
  , testCase "generateNeighbors add neighbors" $
      take 4 (generateNeighbors simplePizza (V.fromList ["pepperoni"]))
      @?= [ Pizza (Quarter $ S.fromList ["cheese", "pepperoni"]) cheeseQuarter cheeseQuarter cheeseQuarter
          , Pizza cheeseQuarter (Quarter $ S.fromList ["cheese", "pepperoni"]) cheeseQuarter cheeseQuarter
          , Pizza cheeseQuarter cheeseQuarter (Quarter $ S.fromList ["cheese", "pepperoni"]) cheeseQuarter
          , Pizza cheeseQuarter cheeseQuarter cheeseQuarter (Quarter $ S.fromList ["cheese", "pepperoni"])
          ]
  ]
  where
    cheeseQuarter = Quarter $ S.singleton "cheese"

simplePizza :: Pizza
simplePizza = Pizza cheeseQuarter cheeseQuarter cheeseQuarter cheeseQuarter
  where cheeseQuarter = Quarter $ S.singleton "cheese"

-- -- Ensure types make sense
typeTests :: TestTree
typeTests = testGroup "Type tests"
  [ testCase "Pizza equality" $
      Pizza (Quarter $ S.fromList ["cheese", "pepperoni"]) (Quarter $ S.singleton "mushroom") cheeseQuarter cheeseQuarter
        ==
        Pizza (Quarter $ S.fromList ["cheese", "pepperoni"]) (Quarter $ S.singleton "mushroom") cheeseQuarter cheeseQuarter
        @?= True
  , testCase "Pizza inequality" $
      Pizza (Quarter $ S.fromList ["cheese", "pepperoni"]) (Quarter $ S.singleton "mushroom") cheeseQuarter cheeseQuarter
        ==
        Pizza (Quarter $ S.fromList ["cheese", "pepperoni"]) (Quarter $ S.singleton "onion") cheeseQuarter cheeseQuarter
        @?= False
  ]
  where 
    cheeseQuarter = Quarter $ S.singleton "cheese"

-- Unit tests for OptimumSlice functions
unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "scoreTopping favorite" $
      scoreTopping defaultConfig testPref "pepperoni" @?= 10

  , testCase "scoreTopping disliked" $
      scoreTopping defaultConfig testPref "pineapple" @?= (-10)

  , testCase "scoreTopping restricted" $
      scoreTopping defaultConfig testPref "seafood" @?= (-100)

  , testCase "scoreTopping neutral" $
      scoreTopping defaultConfig testPref "onion" @?= 5

  , testCase "scoreQuarter with single topping" $
      scoreQuarter defaultConfig testPref (Quarter $ S.singleton "pepperoni") @?= 10

  , testCase "scoreQuarter with multiple toppings" $
      scoreQuarter defaultConfig testPref (Quarter $ S.fromList ["pepperoni", "mushroom"]) @?= 20

  , testCase "scoreQuarter with mixed ratings" $
      scoreQuarter defaultConfig testPref (Quarter $ S.fromList ["pepperoni", "pineapple"]) @?= 0

  , testCase "scorePizza with good pizza" $
      scorePizza defaultConfig testPref goodPizza @?= 17  -- Multi-quarter scoring: 10 + 5 + 2 + 0

  , testCase "scorePizza with bad pizza" $
      scorePizza defaultConfig testPref badPizza @?= (-175)  -- Multi-quarter scoring: -100 + (-50) + (-25) + 0

  , testCase "Topping domain" $
      toppingDomain [testPref, testPref2] @?= V.fromList ["pepperoni", "mushroom", "onion", "green pepper"]
  
  , testCase "Topping domain removes duplicates" $
      toppingDomain [testPref, testPrefDuplicate] @?= V.fromList ["pepperoni", "mushroom"]
  
  , testCase "optimumSlice basic test" $
      scorePizzaGroup defaultConfig [testPref] (optimumSlice [testPref]) >= scorePizzaGroup defaultConfig [testPref] startPizza @?= True
  ]

startPizza :: Pizza
startPizza = Pizza cheeseQuarter cheeseQuarter cheeseQuarter cheeseQuarter
  where cheeseQuarter = Quarter $ S.singleton "cheese"

-- Test data
testPref :: Preference
testPref = Preference
  { favoriteToppings = V.fromList ["pepperoni", "mushroom"]
  , dislikedToppings = V.fromList ["pineapple", "olive"]
  , restrictedToppings = V.fromList ["seafood"]
  , priority = 1
  }

testPref2 :: Preference
testPref2 = Preference
  { favoriteToppings = V.fromList ["onion", "green pepper"]
  , dislikedToppings = V.fromList ["mushroom"]
  , restrictedToppings = V.fromList []
  , priority = 1
  }

testPrefDuplicate :: Preference
testPrefDuplicate = Preference
  { favoriteToppings = V.fromList ["pepperoni", "mushroom"]
  , dislikedToppings = V.fromList []
  , restrictedToppings = V.fromList []
  , priority = 1
  }

goodPizza :: Pizza
goodPizza = Pizza pepperoniQuarter pepperoniQuarter pepperoniQuarter pepperoniQuarter
  where pepperoniQuarter = Quarter $ S.singleton "pepperoni"

badPizza :: Pizza
badPizza = Pizza seafoodQuarter seafoodQuarter seafoodQuarter seafoodQuarter
  where seafoodQuarter = Quarter $ S.singleton "seafood"
