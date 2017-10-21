module Euler where

import Data.List (permutations, sort)
import Euler.Math (binomialCoefficient)

problem15 = binomialCoefficient 20

problem24 =
    let ps        = sort . permutations
        millionth = drop 999999
    in  (head . millionth . ps) "0123456789"
