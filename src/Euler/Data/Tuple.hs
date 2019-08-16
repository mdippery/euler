{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Data.Tuple
  Description : General functions for working with tuples
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Provides functions for working with tuples.
-}
module Euler.Data.Tuple
  (
    -- * Pairs

    -- ** Operators
    (*%)

    -- ** Equality and ordering
  , maxT
  , minT
  , sortT

    -- ** Transformations
  , flattenT

    -- * Triples

    -- ** Accessing elements
  , fst3

    -- ** Equality and ordering
  , sortT3

    -- ** Currying
  , uncurry3
  ) where

import Control.Monad (liftM2)
import Data.List (sort)

-- | Multiplies two pairs together such that the result is the product of
-- each corresponding element of the pair.
(*%) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(x1, y1) *% (x2, y2) = (x1 * x2, y1 * y2)

-- | Returns the maximum element of a pair.
maxT :: Ord a => (a, a) -> a
maxT = uncurry max

-- | Returns the minimum element of a pair.
minT :: Ord a => (a, a) -> a
minT = uncurry min

-- | Sorts a pair so that the minimum value is first.
sortT :: Ord a => (a, a) -> (a, a)
sortT = liftM2 (,) minT maxT

-- | Flattens a list of pairs into a single list containing all values.
--
-- ==== Examples
--
-- >>> flattenT [(1,2), (3,4)]
-- [1,2,3,4]
flattenT :: [(a, a)] -> [a]
flattenT = foldr (\(a, b) memo -> a : b : memo) []

-- | Returns the first element of a triple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | Sorts the elements of a triple.
sortT3 :: Ord a => (a, a, a) -> (a, a, a)
sortT3 (a, b, c) = let [a', b', c'] = sort [a, b, c] in (a', b', c')

-- | Converts a curried function into a function on triples.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z
