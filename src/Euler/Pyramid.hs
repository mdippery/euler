{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Pyramid
  Description : Calculates the maximum path through a pyramid of integers
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Finds the maximum path through a pyramid of integers. For more information
  on the structure of the pyramid, see <https://projecteuler.net/problem=18 Euler Problem #18>.
-}
module Euler.Pyramid
  (
    -- * Data types
    Pyramid (..)
  , PyramidRow (..)
  , pyramid

    -- * Basic functions
  , maximumPath
  ) where

import Data.List.Euler (duplicate, fatten, splitEvery)

-- | A single row in a pyramid
newtype PyramidRow a = PyramidRow
  { pyramidBricks :: [a]   -- ^ Value of each cell in the row
  } deriving Show

-- | A pyramid
newtype Pyramid a = Pyramid
  { pyramidRows :: [PyramidRow a]  -- ^ Rows of the pyramid
  } deriving Show

-- | Creates a new pyramid from the given data.
pyramid :: (Num a, Ord a)
        => [[a]]      -- ^ Pyramid data
        -> Pyramid a  -- ^ A new Pyramid
pyramid = Pyramid . map PyramidRow

-- | Calculates the maximum paths between each element in two adjacent rows
-- of the integer pyramid.
maxR :: (Num a, Ord a)
     => PyramidRow a  -- ^ Bottom row
     -> PyramidRow a  -- ^ Top row
     -> PyramidRow a  -- ^ A new row formed with the maximum values of paths between the two rows
maxR (PyramidRow b) (PyramidRow t) =
  let t' = duplicate t
      b' = fatten b
   in (PyramidRow . map maximum . splitEvery 2 . zipWith (+) t') b'

-- | Reduces a pyramid down to one row with one element containing the
-- maximum path through the pyramid.
reduce :: (Num a, Ord a)
       => Pyramid a     -- ^ Pyramid
       -> PyramidRow a  -- ^ One row with one element containing the maximum path
reduce (Pyramid ls)
  | length ls == 1 = head ls
  | otherwise = (reduce . Pyramid) (r : ls')
  where
    b = head ls
    t = (head . tail) ls
    r = maxR b t
    ls' = drop 2 ls

-- | Find the maximum path through a pyramid of integers.
maximumPath :: (Num a, Ord a)
            => Pyramid a  -- ^ Pyramid of integers, as a list of rows in the pyramid
            -> a          -- ^ Value of the maximum path
maximumPath = head . pyramidBricks . reduce . Pyramid . reverse . pyramidRows
