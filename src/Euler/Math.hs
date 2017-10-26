module Euler.Math where

import Euler.Text (digits)

factorial :: Integer -> Integer
factorial = product . (enumFromTo 1)

divides :: Integer -> Integer -> Bool
divides a b = b `rem` a == 0

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
