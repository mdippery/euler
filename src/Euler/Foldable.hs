{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Foldable
  Description : Additional foldable functions
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Provides missing functionality for Haskell 'Foldable' types.
-}
module Euler.Foldable
  (
    -- * Searching
    filterF

    -- * Special folds
  , atLeast
  ) where

import Control.Applicative (pure)
import Data.Foldable (foldMap)
import Data.Monoid (mempty)

-- | Returns a foldable of elements that satisfy the given predicate.
--
-- This is the 'Foldable' version of 'filter'. See the
-- <https://wiki.haskell.org/Foldable_and_Traversable#Some_trickier_functions:_concatMap_and_filter Haskell wiki>
-- for the inspiration for this function.
filterF :: (Applicative f, Foldable f, Monoid (f a))
        => (a -> Bool)    -- ^ Predicate
        -> f a            -- ^ Original foldable
        -> f a            -- ^ New foldable containing only the elements that satisfy the predicate
filterF p = foldMap (\x -> if p x then pure x else mempty)

-- | True if the foldable contains at least /n/ elements that satisify
-- the given predicate.
atLeast :: (Applicative f, Foldable f, Monoid (f a))
        => (a -> Bool)    -- ^ Predicate
        -> Int            -- ^ Minimum number of elements that must satisfy the predicate
        -> f a            -- ^ Foldable
        -> Bool           -- ^ True if at least /n/ elements satisfy the predicate
atLeast p n f = length (filterF p f) >= n
