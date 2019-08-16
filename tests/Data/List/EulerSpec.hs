{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Data.List.EulerSpec where

import Data.List.Euler
import Test.Hspec

spec :: Spec
spec = do
  describe "zipWithIndex" $ do
    it "produces an empty list when given an empty list" $ do
      length (zipWithIndex []) `shouldBe` 0

    it "produces a list with indices" $ do
      zipWithIndex [2..10] `shouldBe` [(0,2), (1,3), (2,4), (3,5), (4,6), (5,7), (6,8), (7,9), (8,10)]

  describe "unzipWithIndex" $ do
    it "produces an empty list when given an empty list" $ do
      length (unzipWithIndex []) `shouldBe` 0

    it "removes indices from a list" $ do
      unzipWithIndex [(0,2), (1,3), (2,4), (3,5), (4,6), (5,7), (6,8), (7,9), (8,10)] == [2..10]

  describe "dropNth" $ do
    it "produces an empty list when given an empty list" $ do
      length (dropNth 2 []) `shouldBe` 0

    it "drops the correct elements from a list" $ do
      dropNth 2 [0..10] `shouldBe` [1,3..10]

  describe "replace" $ do
    it "replaces the element at the given index" $ do
      replace 4 100 [0..9] `shouldBe` [0,1,2,3,100,5,6,7,8,9]

    it "does nothing if an index is out of bounds" $ do
      replace 100 100 [0..9] `shouldBe` [0..9]

    it "replaces the first element of a list" $ do
      replace 0 100 [0..9] `shouldBe` [100,1,2,3,4,5,6,7,8,9]

    it "replaces the last element of a list" $ do
      replace 9 100 [0..9] `shouldBe` [0,1,2,3,4,5,6,7,8,100]

    it "does nothing to an empty list" $ do
      replace 0 100 [] `shouldBe` []
      replace 10 100 [] `shouldBe` []

  describe "windows" $ do
    it "produces an empty list when given an empty list" $ do
      length (windows 4 []) `shouldBe` 0

    it "produces successive windows of size 4" $ do
      windows 4 "abcdefgh" `shouldBe` ["abcd", "bcde", "cdef", "defg", "efgh"]

  describe "splitEvery" $ do
    let ls = [0..15]

    it "produces an empty list when given an empty list" $ do
      length (splitEvery 4 []) `shouldBe` 0

    it "produces chunks of a list of size 4" $ do
      splitEvery 4 ls `shouldBe` [[0..3], [4..7], [8..11], [12..15]]
