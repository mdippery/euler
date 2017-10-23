module Euler.Math where

import Euler.Text (digits)

factorial = product . (enumFromTo 1)

binomialCoefficient n = factorial (2 * n) `div` (factorial n * factorial n)

digitPowers exp = (map (^ exp)) . digits

sumOfPowers exp = sum . (digitPowers exp)

digitFactorials :: Integer -> [Integer]
digitFactorials = (map factorial) . digits

sumOfFactorials = sum . digitFactorials
