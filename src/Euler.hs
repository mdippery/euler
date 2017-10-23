module Euler where

import Data.List (permutations, sort)
import Euler.Math (binomialCoefficient, sumOfFactorials, sumOfPowers)
import Euler.Util (pairHasEqualElements, pairWithFunc)

problem15 = binomialCoefficient 20

problem24 =
    let ps        = sort . permutations
        millionth = drop 999999
    in  (head . millionth . ps) "0123456789"

problem30 =
    let nums      = [2..355000]
        pair      = pairWithFunc (sumOfPowers 5)
        equalSums = filter pairHasEqualElements
        extract   = map fst
     in sum $ extract $ equalSums $ map pair nums

problem34 =
    let nums            = [3..2540160]
        equalFactorials = filter pairHasEqualElements
        extract         = map fst
     in sum $ extract $ equalFactorials $ map (pairWithFunc sumOfFactorials) nums
