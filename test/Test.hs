module Main where

import Test.Tasty
import Test.Tasty.HUnit

import OptimumSlice
import Data.Vector qualified as V

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, typeTests, neighborTests]

neighborTests :: TestTree
neighborTests = testGroup "Neighbor tests"
  [ testCase "remove a topping" $
      toppingRemove
        (HalfSection (Section $ V.singleton "pepperoni") (Section $ V.singleton "mushroom"))
        "pepperoni"
      @?= HalfSection (Section $ V.singleton "cheese") (Section $ V.singleton "mushroom")
  , testCase "remove another topping" $
      toppingRemove
        (HalfSection (HalfSection (Section $ V.singleton "mushroom") (Section $ V.singleton "onion")) (Section $ V.singleton "mushroom"))
        "mushroom"
      @?= HalfSection (HalfSection (Section $ V.singleton "cheese") (Section $ V.singleton "onion")) (Section $ V.singleton "cheese")
  , testCase "replace a topping" $
      toppingReplace
        (HalfSection (HalfSection (Section $ V.singleton "mushroom") (Section $ V.singleton "onion")) (Section $ V.singleton "mushroom"))
        "mushroom" "peppers"
      @?= HalfSection (HalfSection (Section $ V.singleton "peppers") (Section $ V.singleton "onion")) (Section $ V.singleton "peppers")
  , testCase "add topping to single section" $
      toppingAddAllSections (Section $ V.singleton "cheese") "pepperoni"
      @?= [Section $ V.fromList ["cheese", "pepperoni"]]
  , testCase "add topping to full section" $
      toppingAddAllSections (Section $ V.fromList ["cheese", "pepperoni", "mushroom"]) "onion"
      @?= []
  , testCase "add topping to half section" $
      toppingAddAllSections 
        (HalfSection (Section $ V.singleton "cheese") (Section $ V.singleton "pepperoni")) 
        "mushroom"
      @?= [ HalfSection (Section $ V.fromList ["cheese", "mushroom"]) (Section $ V.singleton "pepperoni")
          , HalfSection (Section $ V.singleton "cheese") (Section $ V.fromList ["pepperoni", "mushroom"])
          ]
  , testCase "add topping to nested half section" $
      toppingAddAllSections
        (HalfSection (HalfSection (Section $ V.singleton "cheese") (Section $ V.singleton "pepperoni")) (Section $ V.singleton "mushroom"))
        "onion"
      @?= [ HalfSection (HalfSection (Section $ V.fromList ["cheese", "onion"]) (Section $ V.singleton "pepperoni")) (Section $ V.singleton "mushroom")
          , HalfSection (HalfSection (Section $ V.singleton "cheese") (Section $ V.fromList ["pepperoni", "onion"])) (Section $ V.singleton "mushroom")
          , HalfSection (HalfSection (Section $ V.singleton "cheese") (Section $ V.singleton "pepperoni")) (Section $ V.fromList ["mushroom", "onion"])
          ]
  , testCase "generateNeighbors basic" $
      length (generateNeighbors simplePie (V.fromList ["pepperoni", "mushroom"])) @?= 4
  , testCase "generateNeighbors add neighbors" $
      take 4 (generateNeighbors simplePie (V.fromList ["pepperoni", "mushroom"]))
      @?= [ Pie (Section $ V.fromList ["cheese", "pepperoni"]) (Section $ V.singleton "cheese")
          , Pie (Section $ V.singleton "cheese") (Section $ V.fromList ["cheese", "pepperoni"])
          , Pie (Section $ V.fromList ["cheese", "mushroom"]) (Section $ V.singleton "cheese")
          , Pie (Section $ V.singleton "cheese") (Section $ V.fromList ["cheese", "mushroom"])
          ]
  ]

simplePie :: Pie
simplePie = Pie (Section $ V.singleton "cheese") (Section $ V.singleton "cheese")

-- -- Ensure types make sense
typeTests :: TestTree
typeTests = testGroup "Type tests"
  [ testCase "Pie equality" $
      Pie (HalfSection (Section $ V.singleton "pepperoni") (Section $ V.singleton "mushroom")) (Section $ V.singleton "cheese")
        ==
        Pie (HalfSection (Section $ V.singleton "mushroom") (Section $ V.singleton "pepperoni")) (Section $ V.singleton "cheese")
        @?= True
  , testCase "Pie inequality" $
      Pie (HalfSection (Section $ V.singleton "pepperoni") (Section $ V.singleton "mushroom")) (Section $ V.singleton "cheese")
        ==
        Pie (HalfSection (Section $ V.singleton "pepperoni") (Section $ V.singleton "mushroom ")) (Section $ V.singleton "pepperoni")
        @?= False
  ]

-- Unit tests for OptimumSlice functions
unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "scoreTopping favorite" $
      scoreTopping testPref "pepperoni" @?= 10

  , testCase "scoreTopping disliked" $
      scoreTopping testPref "pineapple" @?= (-10)

  , testCase "scoreTopping restricted" $
      scoreTopping testPref "seafood" @?= (-100)

  , testCase "scoreTopping neutral" $
      scoreTopping testPref "onion" @?= 5

  , testCase "scoreHalf with single leaf" $
      scoreHalf testPref (Section $ V.singleton "pepperoni") @?= 10

  , testCase "scoreHalf with multiple leaves" $
      scoreHalf testPref (HalfSection (Section $ V.singleton "pepperoni") (Section $ V.singleton "mushroom")) @?= 20

  , testCase "scoreHalf with mixed ratings" $
      scoreHalf testPref (HalfSection (Section $ V.singleton "pepperoni") (Section $ V.singleton "pineapple")) @?= 0

  , testCase "scorePie with good pie" $
      scorePie testPref goodPie @?= 20

  , testCase "scorePie with bad pie" $
      scorePie testPref badPie @?= (-20)

  , testCase "Topping domain" $
      toppingDomain [testPref, testPref2] @?= V.fromList ["pepperoni", "mushroom", "onion", "green pepper"]
  
  , testCase "Topping domain removes duplicates" $
      toppingDomain [testPref, testPrefDuplicate] @?= V.fromList ["pepperoni", "mushroom"]
  
  , testCase "optimumSlice basic test" $
      scorePieGroup [testPref] (optimumSlice [testPref]) >= scorePieGroup [testPref] startPie @?= True
  ]

startPie :: Pie
startPie = Pie (Section $ V.singleton "cheese") (Section $ V.singleton "cheese")

-- Test data
testPref :: Preference
testPref = Preference
  { favoriteToppings = V.fromList ["pepperoni", "mushroom"]
  , dislikedToppings = V.fromList ["pineapple", "olive"]
  , restrictedToppings = V.fromList ["seafood"]
  }

testPref2 :: Preference
testPref2 = Preference
  { favoriteToppings = V.fromList ["onion", "green pepper"]
  , dislikedToppings = V.fromList ["mushroom"]
  , restrictedToppings = V.fromList []
  }

testPrefDuplicate :: Preference
testPrefDuplicate = Preference
  { favoriteToppings = V.fromList ["pepperoni", "mushroom"]
  , dislikedToppings = V.fromList []
  , restrictedToppings = V.fromList []
  }

goodPie :: Pie
goodPie = Pie
  (HalfSection (Section $ V.singleton "pepperoni") (Section $ V.singleton "mushroom"))
  (Section $ V.singleton "cheese")

badPie :: Pie
badPie = Pie
  (Section $ V.singleton "seafood")
  (HalfSection (Section $ V.singleton "pineapple") (Section $ V.singleton "olive"))
