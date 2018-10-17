{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Spiral
  Description : Calculates properties of an Ulam spiral
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Calculates various properties of an <https://en.wikipedia.org/wiki/Ulam_spiral Ulam spiral>.
-}
module Euler.Spiral
  (
    diagonalSize
  , numRings
  , sumDiagonals
  ) where

-- | Calculates the sum of the diagonals of spiral (a series of /n/ rings).
sumDiagonals :: Integer   -- ^ Number of rings in the spiral
             -> Integer   -- ^ Sum of diagonals
sumDiagonals 0 = 1
sumDiagonals n =
  let ur = (2 * n + 1) ^ 2
      ul = ur - (2 * n)
      ll = ul - (2 * n)
      lr = ll - (2 * n)
   in ur + ul + ll + lr + sumDiagonals (n - 1)

-- | Number of rings in a spiral with length and width of size /n/.
numRings :: Integer   -- ^ Size of length and width
         -> Integer   -- ^ Number of spirals
numRings = flip div 2

-- | Number of elements in the diagonal portions of a spiral with /n/ rings.
diagonalSize :: Integer   -- ^ Number of rings in the spiral
             -> Integer   -- ^ Number of elements in the diagonal of the spiral
diagonalSize = (1 +) . (4 *)
