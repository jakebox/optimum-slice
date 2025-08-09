module Main where

import Test.Tasty
import Test.Tasty.HUnit

import OptimumSlice
import Data.Vector qualified as V

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, typeTests]

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
  ]

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

goodPie :: Pie
goodPie = Pie
  (HalfSection (Section $ V.singleton "pepperoni") (Section $ V.singleton "mushroom"))
  (Section $ V.singleton "cheese")

badPie :: Pie
badPie = Pie
  (Section $ V.singleton "seafood")
  (HalfSection (Section $ V.singleton "pineapple") (Section $ V.singleton "olive"))
