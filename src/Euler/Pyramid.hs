{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.Pyramid where

import Euler.List
import Euler.Tuple

maxR :: (Num a, Ord a) => [a] -> [a] -> [a]
maxR t b =
  let t' = duplicate t
      b' = fatten b
   in (map maximum . splitEvery 2 . mapT (+) . zip t') b'
