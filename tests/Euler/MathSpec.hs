{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.MathSpec where

import Euler.Math
import Test.Hspec

spec :: Spec
spec = do
  describe "Euler.Math" $ do
    describe "factorial" $ do
      it "returns the first ten factorials" $ do
        factorial 0 `shouldBe` 1
        factorial 1 `shouldBe` 1
        factorial 2 `shouldBe` 2
        factorial 3 `shouldBe` 6
        factorial 4 `shouldBe` 24
        factorial 5 `shouldBe` 120
        factorial 6 `shouldBe` 720
        factorial 7 `shouldBe` 5040
        factorial 8 `shouldBe` 40320
        factorial 9 `shouldBe` 362880
        factorial 10 `shouldBe` 3628800

    describe "fibonacci" $ do
      it "returns the first 10 fibonacci numbers" $ do
        map fibonacci [1..10] `shouldBe` [1,1,2,3,5,8,13,21,34,55]

    describe "collatz" $ do
      it "returns a correct value for the base case" $ do
        collatzLength 1 `shouldBe` 1

      it "returns correct values" $ do
        collatzLength 13 `shouldBe` 10
        collatzLength 28 `shouldBe` 19
        collatzLength 1022 `shouldBe` 63

    describe "sumDivisors" $ do
      it "returns the sum of the divisors of 72" $ do
        sumDivisors 72 `shouldBe` 123

      it "returns the sum of the divisors of 120" $ do
        sumDivisors 120 `shouldBe` 240

      it "returns the sum of the divisors of 220" $ do
        sumDivisors 220 `shouldBe` 284

      it "returns the sum of the divisors of 284" $ do
        sumDivisors 284 `shouldBe` 220

    describe "truncatables" $ do
      it "returns a list of numbers truncated" $ do
        truncatables 3797 `shouldBe` [3797, 797, 97, 7, 379, 37, 3]