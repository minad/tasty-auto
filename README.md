# tasty-auto: Simple auto discovery for Tasty

[![Hackage](https://img.shields.io/hackage/v/tasty-auto.svg)](https://hackage.haskell.org/package/tasty-auto)
[![Build Status](https://secure.travis-ci.org/minad/tasty-auto.png?branch=master)](http://travis-ci.org/minad/tasty-auto)

This package provides auto discovery for the tasty test framework.

1. Install tasty-auto (using cabal or stack)

2. Create a file test/test.hs

``` haskell
-- test/test.hs
{-# OPTIONS_GHC -F -pgmF tasty-auto #-}
```

3. Put your tests in files with the suffix `*Test.hs` or `*Spec.hs`. Functions
with the following prefixes are automatically discovered:

* `prop_` for QuickCheck properties
* `scprop_` for SmallCheck properties
* `case_` for HUnit test cases (overloaded for `IO ()`, `IO String` and `(String -> IO ()) -> IO`)
* `spec_` for Hspec specifications
* `test_` for Tasty TestTrees (overloaded for `TestTree`, `[TestTree]`, `IO TestTree` and `IO [TestTree]`)

``` haskell
-- test/PropTest.hs
module PropTest where

prop_Addition_is_commutative :: Int -> Int -> Bool
prop_Addition_is_commutative a b = a + b == b + a

-- test/CaseTest.hs
module CaseTest where

import Test.Tasty.HUnit

case_List_comparison_with_different_length :: IO ()
case_List_comparison_with_different_length = [1, 2, 3] `compare` [1,2] @?= GT

-- test/TestSpec.hs
module TestSpec where

import Test.Tasty.Hspec

spec_Prelude :: Spec
spec_Prelude = do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

-- test/TreeTest.hs
{-# LANGUAGE ScopedTypeVariables #-}
module TreeTest where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

test_Addition :: TestTree
test_Addition = testProperty "Addition commutes" $ \(a :: Int) (b :: Int) -> a + b == b + a

test_Multiplication :: [TestTree]
test_Multiplication =
  [ testProperty "Multiplication commutes" $ \(a :: Int) (b :: Int) -> a * b == b * a
  , testProperty "One is identity" $ \(a :: Int) -> a * 1 == a
  ]

test_Generate_Tree :: IO TestTree
test_Generate_Tree = do
  input <- pure "Some input"
  pure $ testCase input $ pure ()

test_Generate_Trees :: IO [TestTree]
test_Generate_Trees = do
  inputs <- pure ["First input", "Second input"]
  pure $ map (\s -> testCase s $ pure ()) inputs
```
