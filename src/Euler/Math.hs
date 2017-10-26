module Euler.Math where

import Euler.Text (digits)

factorial = product . (enumFromTo 1)

binomialCoefficient n = factorial (2 * n) `div` (factorial n * factorial n)

mapDigits f = (map f) . digits

sumDigits = (.) sum

digitPowers exp = mapDigits (^ exp)

sumOfPowers exp = sumDigits (digitPowers exp)

digitFactorials :: Integer -> [Integer]
digitFactorials = mapDigits factorial

sumOfFactorials = sumDigits digitFactorials
