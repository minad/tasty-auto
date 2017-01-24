# tasty-auto: Simple auto discovery for Tasty

[![Hackage](https://img.shields.io/hackage/v/tasty-auto.svg)](https://hackage.haskell.org/package/tasty-auto)
[![Build Status](https://secure.travis-ci.org/minad/tasty-auto.png?branch=master)](http://travis-ci.org/minad/tasty-auto)

This package provides auto discovery for the tasty test framework.

* Install tasty-auto (using cabal or stack)

* Create a file test/test.hs

``` haskell
-- test/test.hs
{-# OPTIONS_GHC -F -pgmF tasty-auto #-}
```

* Put your tests in files with the suffix `*Test.hs` or `*Spec.hs`. Functions
with the following prefixes are automatically discovered:

* `prop_` for QuickCheck properties
* `scprop_` for SmallCheck properties
* `case_` for HUnit test cases (overloaded for `IO ()`, `IO String` and `(String -> IO ()) -> IO`)
* `spec_` for Hspec specifications
* `test_` for Tasty TestTrees (overloaded for `TestTree`, `[TestTree]`, `IO TestTree` and `IO [TestTree]`)

## Examples

``` haskell
-- test/PropTest.hs
module PropTest where

prop_Addition_is_commutative :: Int -> Int -> Bool
prop_Addition_is_commutative a b = a + b == b + a
```

``` haskell
-- test/CaseTest.hs
module CaseTest where

import Test.Tasty.HUnit

case_List_comparison_with_different_length :: IO ()
case_List_comparison_with_different_length = [1, 2, 3] `compare` [1,2] @?= GT
```

``` haskell
-- test/TestSpec.hs
module TestSpec where

import Test.Tasty.Hspec

spec_Prelude :: Spec
spec_Prelude = do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
```

``` haskell
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

### Generated code

The generated code of the preprocessor looks like this:

``` haskell
{-# LINE 1 "test/test.hs" #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where
import Prelude
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.Hspec as HS
import qualified CaseTest
import qualified SCPropTest
import qualified TreeTest
import qualified PropTest
import qualified TestSpec
import qualified SubMod.PropTest
class TestCase a where testCase :: String -> a -> IO T.TestTree
instance TestCase (IO ())                      where testCase n = pure . HU.testCase      n
instance TestCase (IO String)                  where testCase n = pure . HU.testCaseInfo  n
instance TestCase ((String -> IO ()) -> IO ()) where testCase n = pure . HU.testCaseSteps n
class TestGroup a where testGroup :: String -> a -> IO T.TestTree
instance TestGroup T.TestTree        where testGroup _ a = pure a
instance TestGroup [T.TestTree]      where testGroup n a = pure $ T.testGroup n a
instance TestGroup (IO T.TestTree)   where testGroup _ a = a
instance TestGroup (IO [T.TestTree]) where testGroup n a = T.testGroup n <$> a
main :: IO ()
main = do
  t0 <- testCase "List comparison with different length" CaseTest.case_List_comparison_with_different_length
  t1 <- pure $ SC.testProperty "sort reverse" SCPropTest.scprop_sort_reverse
  t2 <- testGroup "Addition" TreeTest.test_Addition
  t3 <- testGroup "Multiplication" TreeTest.test_Multiplication
  t4 <- testGroup "Generate Tree" TreeTest.test_Generate_Tree
  t5 <- testGroup "Generate Trees" TreeTest.test_Generate_Trees
  t6 <- pure $ QC.testProperty "Addition is commutative" PropTest.prop_Addition_is_commutative
  t7 <- HS.testSpec "Prelude" TestSpec.spec_Prelude
  t8 <- pure $ QC.testProperty "Addition is associative" SubMod.PropTest.prop_Addition_is_associative
  T.defaultMain $ T.testGroup "test/test.hs" [t0,t1,t2,t3,t4,t5,t6,t7,t8]
```
