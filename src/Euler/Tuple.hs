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
    equalT
  , flattenT
  , mapT
  , unzipT
  , zipT

    -- * Triples
  , fst3
  ) where

import Control.Monad (ap)

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

-- | Unzips a list of 2-tuples, returning a 2-tuple containing the first set
-- of values in the first element, and the second set of values in the
-- second element.
unzipT :: [(a, b)] -> ([a], [b])
unzipT = unzip

-- | Flattens a list of 2-tuples into a single list containing all values.
flattenT :: [(a, a)] -> [a]
flattenT = foldr (\(a, b) memo -> a : b : memo) []

-- | True if both elements of a 2-tuple are equal.
equalT :: Eq a => (a, a) -> Bool
equalT = uncurry (==)

-- | Returns the first element of a 3-tuple.
fst3 :: (a,a,a) -> a
fst3 (a,_,_) = a
