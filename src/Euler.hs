module Euler where

import Data.List (permutations, sort)
import Euler.Math
import Euler.Util

notSolved = 0

problem1 = sum $ filter (\n -> 3 `divides` n || 5 `divides` n) [1..999]

problem2 = sum $ filter isEven $ takeWhile (<= 4000000) (map fibonacci [1..])

problem3 = notSolved

problem4 = notSolved

problem5 = notSolved

problem6 = notSolved

problem7 = notSolved

problem8 = notSolved

problem9 = notSolved

problem10 = notSolved

problem11 = notSolved

problem12 = notSolved

problem13 = notSolved

problem14 = notSolved

problem15 = binomialCoefficient 20

problem16 = notSolved

problem17 = notSolved

problem18 = notSolved

problem19 = notSolved

problem20 = notSolved

problem21 = notSolved

problem22 = notSolved

problem24 =
    let ps        = sort . permutations
        millionth = drop 999999
     in (head . millionth . ps) "0123456789"

problem25 = notSolved

problem29 = notSolved

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

problem35 = notSolved

problem36 = notSolved

problem48 = notSolved

problem53 = notSolved

problem56 = notSolved

problem67 = notSolved
