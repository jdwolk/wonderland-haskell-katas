module Katas.AlphabetCipherSpec (
  spec
) where

import           Katas.AlphabetCipher
import           Test.Hspec

spec :: Spec
spec = do
  describe "does it work" $ do
    it "works!" $ do
      theThing `shouldBe` "hi"
