{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Tuple
  Description : General functions for working with tuples
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  General functions for working with tuples.
-}
module Euler.Tuple
  (
    -- * Pairs

    -- ** Equality and ordering
    equalT
  , maxT
  , minT
  , sortT

    -- ** Transformations
  , flattenT
  , mapT

    -- ** Zipping and unzipping
  , unzipT
  , zipT

    -- * Triples

    -- ** Accessing elements
  , fst3

    -- ** Equality and ordering
  , sort3
  ) where

import Control.Monad (ap, liftM2)
import Data.List (sort)

-- | Applies a function of two arguments to a list of 2-tuples, where
-- each element of the tuple is a separate argument to the function,
-- and produces a list of the output values of the function.
mapT :: (a -> b -> c)   -- ^ Mapping function
     -> [(a, b)]        -- ^ List of function arguments
     -> [c]             -- ^ List of outputs of the mapping function
mapT = map . uncurry

-- | Applies a function to each element of a list, returning a list of 2-tuples
-- in the form (x, f x).
zipT :: (a -> b)    -- ^ Mapping function
     -> [a]         -- ^ Original list of values
     -> [(a, b)]    -- ^ List of 2-tuples containing the original value and the output value
zipT = map . ap (,)

-- | Unzips a list of 2-tuples by passing the 2-tuple to the given function,
-- returning a list of the output of that function.
--
-- This is a counterpart to 'zipT'.
unzipT :: ((a, b) -> c)   -- ^ Mapping function
       -> [(a, b)]        -- ^ Original list of tuples
       -> [c]             -- ^ List of results of mapping function applied to each tuple
unzipT = map

-- | Sorts a 2-tuple so that the minimum value is first.
sortT :: Ord a => (a,a) -> (a, a)
sortT = liftM2 (,) minT maxT

-- | Sorts a 3-tuple so that the elements are ordered from least to greatest
sort3 :: Ord a => (a,a,a) -> (a,a,a)
sort3 (a,b,c) =
  let ls = sort [a, b, c]
   in (ls !! 0, ls !! 1, ls !! 2)

-- | Returns the maximum element in a 2-tuple.
maxT :: Ord a => (a, a) -> a
maxT = uncurry max

-- | Returns the minimum element in a 2-tuple.
minT :: Ord a => (a, a) -> a
minT = uncurry min

-- | Flattens a list of 2-tuples into a single list containing all values.
--
-- ==== Examples
--
-- >>> flattenT [(1,2),(3,4)]
-- [1,2,3,4]
flattenT :: [(a, a)] -> [a]
flattenT = foldr (\(a, b) memo -> a : b : memo) []

-- | True if both elements of a 2-tuple are equal.
equalT :: Eq a => (a, a) -> Bool
equalT = uncurry (==)

-- | Returns the first element of a 3-tuple.
fst3 :: (a,a,a) -> a
fst3 (a,_,_) = a
