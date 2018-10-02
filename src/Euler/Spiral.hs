{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Spiral
  Description : Calculates the sum of the diagonals of "spirals" of numbers
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Calculates the sum of the diagonals of "spirals" of numbers. These "spirals"
  are defined in <https://projecteuler.net/problem=28 Euler Problem #28>.
-}
module Euler.Spiral
  (
    -- * Data types
    Ring (..)
  , ring

    -- * Basic functions
  , numRings
  , sumDiagonals
  ) where

import Euler.Math (nextPrime)

-- | Describes a single ring in a spiral of integers
data Ring = Ring
  { upperRight :: Integer   -- ^ Value of the upper right corner of the spiral
  , upperLeft  :: Integer   -- ^ Value of the upper left corner of the spiral
  , lowerLeft  :: Integer   -- ^ Value of the lower left corner of the spiral
  , lowerRight :: Integer   -- ^ Value of the lower right corner of the spiral
  } deriving Show

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

-- | Retrieves the /nth/ ring of a spiral.
ring 0 = Ring 1 1 1 1
ring n =
  let delta = 8 * n
      prev = ring (n - 1)
      lr = delta + lowerRight prev
      ll = (delta - 2) + lowerLeft prev
      ul = (delta - 4) + upperLeft prev
      ur = (delta - 6) + upperRight prev
   in Ring ur ul ll lr
