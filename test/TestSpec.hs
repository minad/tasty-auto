module TestSpec where

import Test.Tasty.Hspec

spec_Prelude :: Spec
spec_Prelude = do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
