{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.Ring where

sumDiagonals :: Integer -> Integer
sumDiagonals 0 = 1
sumDiagonals n =
  let ur = (2 * n + 1) ^ 2
      ul = ur - (2 * n)
      ll = ul - (2 * n)
      lr = ll - (2 * n)
   in ur + ul + ll + lr + sumDiagonals (n - 1)

numRings :: Integer -> Integer
numRings = flip div 2
