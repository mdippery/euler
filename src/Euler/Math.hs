module Euler.Math where

import Euler.Text (digits)

isEven :: Integer -> Bool
isEven n = n `rem` 2 == 0

factorial :: Integer -> Integer
factorial = product . (enumFromTo 1)

divides :: Integer -> Integer -> Bool
divides a b = b `rem` a == 0

fibonacci :: Integer -> Integer
fibonacci n =
  let s   = sqrt 5
      si  = 1 / s
      tp  = 1 + s
      tn  = 1 - s
      tp' = tp / 2
      tn' = tn / 2
   in round $ (si * (tp' ^ n)) - (si * (tn' ^ n))

binomialCoefficient :: Integer -> Integer
binomialCoefficient n = factorial (2 * n) `div` (factorial n * factorial n)

mapDigits :: (Integer -> a) -> Integer -> [a]
mapDigits f = (map f) . digits

sumDigits :: (a -> [Integer]) -> a -> Integer
sumDigits = (.) sum

digitPowers :: Integral a => a -> Integer -> [Integer]
digitPowers exp = mapDigits (^ exp)

sumOfPowers :: Integral a => a -> Integer -> Integer
sumOfPowers exp = sumDigits (digitPowers exp)

digitFactorials :: Integer -> [Integer]
digitFactorials = mapDigits factorial

sumOfFactorials :: Integer -> Integer
sumOfFactorials = sumDigits digitFactorials
