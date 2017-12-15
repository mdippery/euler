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
module Euler.Pyramid where

import Euler.List
import Euler.Tuple

-- | A single row in a pyramid
type PyramidRow = [Integer]

-- | A pyramid
type Pyramid = [PyramidRow]

-- | Calculates the maximum paths between each element in two adjacent rows
-- of the integer pyramid.
maxR :: PyramidRow   -- ^ Bottom row
     -> PyramidRow   -- ^ Top row
     -> PyramidRow   -- ^ A new row formed with the maximum values of paths between the two rows
maxR b t =
  let t' = duplicate t
      b' = fatten b
   in (map maximum . splitEvery 2 . mapT (+) . zip t') b'

-- | Reduces a pyramid down to one row with one element containing the
-- maximum path through the pyramid.
reduce :: Pyramid     -- ^ Pyramid
       -> PyramidRow  -- ^ One row with one element containing the maximum path
reduce ls
  | length ls == 1 = head ls
  | otherwise = reduce (r : ls')
  where
    b = head ls
    t = (head . tail) ls
    r = maxR b t
    ls' = drop 2 ls

-- | Find the maximum path through a pyramid of integers.
maximumPath :: Pyramid  -- ^ Pyramid of integers, as a list of rows in the pyramid
            -> Integer  -- ^ Value of the maximum path
maximumPath = head . reduce . reverse
