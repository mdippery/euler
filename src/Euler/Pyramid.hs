{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.Pyramid where

import Euler.List
import Euler.Tuple

maxR :: (Num a, Ord a) => [a] -> [a] -> [a]
maxR b t =
  let t' = duplicate t
      b' = fatten b
   in (map maximum . splitEvery 2 . mapT (+) . zip t') b'

reduce :: (Num a, Ord a) => [[a]] -> [a]
reduce ls
  | length ls == 1 = head ls
  | otherwise = reduce (r : ls')
  where
    b = head ls
    t = (head . tail) ls
    r = maxR b t
    ls' = drop 2 ls

maximumPath :: (Num a, Ord a) => [[a]] -> a
maximumPath = head . reduce . reverse
