module Euler where

import Data.List (nub, permutations, sort)
import Euler.Data
import Euler.Math
import Euler.Text
import Euler.Util

notSolved = 0

problem1 = sum $ filter (\n -> 3 `divides` n || 5 `divides` n) [1..999]

problem2 = sum $ filter isEven $ takeWhile (<= 4000000) (map fibonacci [1..])

problem3 = notSolved

problem4 = (head . reverse . sort . filter isPalindrome) [x * y | x <- [100..999], y <- [100..999]]

problem5 = head $ dropWhile (not . divisibleBy [1..20]) [20,40..]

problem6 =
  let ns           = [1..100]
      sumOfSquares = (sum . map (^ 2)) ns
      squareOfSums = sum ns ^ 2
   in squareOfSums - sumOfSquares

problem7 = (head . drop 10000 . filter isPrime) [2..]

problem8 = notSolved

problem9 = notSolved

problem10 = notSolved

problem11 = notSolved

problem12 = notSolved

problem13 = notSolved

problem14 = notSolved

problem15 = binomialCoefficient 20

problem16 = (sum . digits) (2 ^ 1000)

problem17 = notSolved

problem18 = notSolved

problem19 = notSolved

problem20 = (sum . digits . factorial) 100

problem21 = notSolved

problem22 = notSolved

problem24 =
    let ps        = sort . permutations
        millionth = drop 999999
     in (head . millionth . ps) "0123456789"

problem25 = notSolved

problem29 = (length . nub) [a ^ b | a <- [2..100], b <- [2..100]]

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

problem36 =
  let p n = isPalindromeIn 10 n && isPalindromeIn 2 n
      ns  = [1..999999]
   in (sum . filter p) ns

problem48 =
  let p = 10 ^ 10
   in modSum p $ map (\n -> modPower p n n) [1..1000]

problem53 = notSolved

problem56 = notSolved

problem67 = notSolved
