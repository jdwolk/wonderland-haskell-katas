module Katas.AlphabetCipher.SimpleSpec (
  spec
) where

import           Data.Maybe                  (fromJust)
import           Katas.AlphabetCipher.Simple (Headers (..), Table (..),
                                              TableDirection (..), decode,
                                              encode, findIn, makeSquareTable,
                                              makeTable, sliceBy)
import           Test.Hspec

spec :: Spec
spec = do
  describe "makeTable" $ do
    it "has the correct number of rows" $ do
      let rs = [("A", ["a", "b"]),
                ("B", ["b", "c"])
               ]
          table = makeTable ["A", "B"] rs
      (length $ rows table) `shouldBe` length rs

  describe "sliceBy" $ do
    context "RowDir" $ do
      it "returns the value in the given table at the given row" $ do
        let rowA = ["a", "b", "c"]
            rowB = ["b", "c", "d"]
            table = makeTable ["A", "B", "C"] [
                      ("A", rowA),
                      ("B", rowB)
                    ]
        (fromJust $ sliceBy RowDir table "A") `shouldBe` rowA

  describe "findIn" $ do
    it "looks up the given row and column in the table" $ do
      let rowA = ["a", "b", "c"]
          rowB = ["b", "c", "d"]
          table = makeTable ["A", "B", "C"] [
                    ("A", rowA),
                    ("B", rowB)
                  ]
      findIn table "B" "B" `shouldBe` "c"

  describe "encode" $ do
    context "with keyword 'scone'" $ do
      it "encodes correctly" $ do
        encode "scones" "meetmebythetree" `shouldBe` "egsgqwtahuiljgs"

  describe "decode" $ do
    it "inverts an encoded string" $ do
      let cipherText = "meetmebythetree"
          key = "scones"
      decode key (encode key cipherText) `shouldBe` cipherText
