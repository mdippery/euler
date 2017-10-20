module Euler where

import Data.List (permutations, sort)
import Euler.Math (binomialCoefficient)

problem15 = binomialCoefficient 20

problem24 = head $ drop 999999 $ sort $ permutations "0123456789"
