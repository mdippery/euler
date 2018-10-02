{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Spiral
  Description : Calculates the sum of the diagonals of "spirals" of numbers
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Calculates the sum of the diagonals of "spirals" of numbers. These "spirals"
  are defined in <https://projecteuler.net/problem=28 Euler Problem #28> and
  <https://projecteuler.net/problem=58 Euler Problem #58>.
-}
module Euler.Spiral
  (
    -- * Data types
    Ring (..)
  , primesInRing
  , ring

    -- * Basic functions
  , diagonalSize
  , numRings
  , primeRatio
  , primeRatioWithSideLength
  , rings
  , ringSize
  , sumDiagonals
  ) where

import Data.Bool (bool)

import Euler.Math (isPrime)

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
numRings :: Integer   -- ^ Length and width of spiral
         -> Integer   -- ^ Number of rings in the spiral
numRings = flip div 2

-- | List of rings that make up the spiral with /n/ total rings.
rings :: Integer -> [Ring]
rings 0 = [ring 0]
rings n = ring n : rings (n - 1)

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

-- | Number of elements in the /nth/ ring of a spiral of integers.
ringSize :: Integer -> Integer
ringSize = max 1 . (8 *)

-- | Number of elements in the diagonal portions of a spiral with /n/ rings.
diagonalSize :: Integer   -- ^ Number of rings in the spiral
             -> Integer   -- ^ Total size of diagonals in the spiral
diagonalSize = (+) 1 . (*) 4

-- | Number of primes in the ring.
primesInRing :: Num a => Ring -> a
primesInRing (Ring ur ul ll lr) = (sum . map (bool 0 1 . isPrime)) [ur,ul,ll,lr]

-- | Calculates the ratio of prime numbers to all numbers in the diagonals of
-- a spiral with /n/ rings.
primeRatio :: Fractional a
           => Integer   -- ^ Total number of rings comprising the spiral
           -> a         -- ^ Ratio of prime numbers to all numbers in the diagonal of the spiral
primeRatio n = (fromIntegral . sum . map primesInRing . rings) n / (fromIntegral . diagonalSize) n

-- | Calculates the ratio of prime numbers to all numbers in the diagonals of
-- a spiral with sides of length /n/.
primeRatioWithSideLength :: Fractional a
                         => Integer   -- ^ Length of a side of the given spiral
                         -> a         -- ^ Ratio of prime numbers to all numbers in the diagonal of the spiral
primeRatioWithSideLength = primeRatio . numRings
