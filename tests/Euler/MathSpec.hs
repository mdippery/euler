{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.MathSpec where

import Data.Ratio ((%))
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

    describe "isPentagonal" $ do
      it "returns true if a number is pentagonal" $ do
        isPentagonal 1 `shouldBe` True
        isPentagonal 5 `shouldBe` True
        isPentagonal 12 `shouldBe` True
        isPentagonal 22 `shouldBe` True
        isPentagonal 35 `shouldBe` True
        isPentagonal 51 `shouldBe` True
        isPentagonal 70 `shouldBe` True
        isPentagonal 92 `shouldBe` True
        isPentagonal 117 `shouldBe` True
        isPentagonal 145 `shouldBe` True
        isPentagonal 48 `shouldBe` False

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

    describe "isPandigital" $ do
      it "returns true if a number is 1-9 pandigital" $ do
        isPandigital 123456789 `shouldBe` True

      it "returns false if a number is not 1-9 pandigital" $ do
        isPandigital 1234456789 `shouldBe` False

    describe "isPandigitalTo" $ do
      it "returns true if a number is 1-5 pandigital" $ do
        isPandigitalTo 5 15234 `shouldBe` True

      it "returns false if a number is not 1-5 pandigital" $ do
        isPandigitalTo 5 12445 `shouldBe` False

    describe "isPandigitalFromTo" $ do
      it "returns true if a number is 0-5 pandigital" $ do
        isPandigitalFromTo 0 5 123045 `shouldBe` True

      it "returns false if a number is not 0-5 pandigital" $ do
        isPandigitalFromTo 0 5 153302 `shouldBe` False

    describe "totient" $ do
      it "returns the count of numbers coprime to a given number" $ do
        totient 1 `shouldBe` 1
        totient 2 `shouldBe` 1
        totient 3 `shouldBe` 2
        totient 4 `shouldBe` 2
        totient 5 `shouldBe` 4
        totient 6 `shouldBe` 2
        totient 7 `shouldBe` 6
        totient 8 `shouldBe` 4
        totient 9 `shouldBe` 6
        totient 10 `shouldBe` 4

    describe "cycleLength" $ do
      it "returns the length of a ratio's recurring cycle" $ do
        cycleLength (1 % 2) `shouldBe` 0
        cycleLength (1 % 3) `shouldBe` 1
        cycleLength (1 % 4) `shouldBe` 0
        cycleLength (1 % 5) `shouldBe` 0
        cycleLength (1 % 6) `shouldBe` 1
        cycleLength (1 % 7) `shouldBe` 6
        cycleLength (1 % 8) `shouldBe` 0
        cycleLength (1 % 9) `shouldBe` 1
        cycleLength (1 % 10) `shouldBe` 0
