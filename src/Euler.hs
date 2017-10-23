module Euler where

import Data.List (permutations, sort)
import Euler.Math (binomialCoefficient, sumOfPowers)

problem15 = binomialCoefficient 20

problem24 =
    let ps        = sort . permutations
        millionth = drop 999999
    in  (head . millionth . ps) "0123456789"

problem30 =
    let nums      = [2..355000]
        pair x    = (x, sumOfPowers 5 x)
        equalSums = filter (\(x, y) -> x == y)
        extract   = map fst
    in sum $ extract $ equalSums $ map pair nums
