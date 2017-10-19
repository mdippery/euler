module Euler.Math where

factorial n = product [1..n]

binomialCoefficient n = factorial (2 * n) `div` (factorial n * factorial n)
