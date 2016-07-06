module Katas.AlphabetCipher.MatrixSpec (
  spec
) where

import           Katas.AlphabetCipher.Matrix (encode)
import           Test.Hspec

spec :: Spec
spec = do
  describe "encode" $ do
    it "works" $ do
      encode "scones" "meetmebythetree" `shouldBe` "egsgqwtahuiljgs"
