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

    -- ** Operators
    (*%)

    -- ** Equality and ordering
  , equalT
  , maxT
  , minT
  , sortT

    -- ** Transformations
  , flattenT
  , mapT

    -- * Triples

    -- ** Accessing elements
  , fst3

    -- ** Equality and ordering
  , sort3

    -- ** Currying
  , uncurry3
  ) where

import Control.Monad (ap, liftM2)
import Data.List (sort)

-- | Multiplies two pairs together such that the result is the product of the
-- first two elements of each pair and the product of the second two elements
-- of each pair.
(*%) :: (Num a, Num b) => (a,b) -> (a,b) -> (a,b)
(*%) (n1,d1) (n2,d2) = (n1 * n2, d1 * d2)

-- | Applies a function of two arguments to a list of 2-tuples, where
-- each element of the tuple is a separate argument to the function,
-- and produces a list of the output values of the function.
mapT :: (a -> b -> c)   -- ^ Mapping function
     -> [(a, b)]        -- ^ List of function arguments
     -> [c]             -- ^ List of outputs of the mapping function
mapT = map . uncurry

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
fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

-- | Converts a curried function to a function on triples.
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (x,y,z) = f x y z
