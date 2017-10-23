module Euler.Math where

import Euler.Text (digits)

factorial n = product [1..n]

binomialCoefficient n = factorial (2 * n) `div` (factorial n * factorial n)

digitPowers exp n = map (^ exp) (digits n)

sumOfPowers exp = (sum . digitPowers exp)
