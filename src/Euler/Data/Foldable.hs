{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Data.Foldable
  Description : Additional foldable functions
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Provides missing functionality for Haskell 'Foldable' types.
-}
module Euler.Data.Foldable
  (
    -- * Searching
    filterF

    -- * Special folds
  , atLeast
  , exactly
  ) where

import Control.Applicative (pure)
import Control.Monad (ap)
import Data.Bool (bool)
import Data.Foldable (foldMap)
import Data.Monoid (mempty)

-- | Returns a foldable of elements that satisfy the given predicate.
--
-- This is the 'Foldable' version of 'filter'. See the
-- <https://wiki.haskell.org/Foldable_and_Traversable#Some_trickier_functions:_concatMap_and_filter Haskell wiki>
-- for the inspiration for this function.
filterF
  :: (Applicative f, Foldable f, Monoid (f a))
  => (a -> Bool)    -- ^ Predicate
  -> f a            -- ^ Original foldable
  -> f a            -- ^ New foldable containing only the elements that satisfy the predicate
filterF = foldMap . ap (bool mempty . pure)

-- | True if the foldable contains at least /n/ elements that satisify
-- the given predicate.
atLeast
  :: (Applicative f, Foldable f, Monoid (f a))
  => (a -> Bool)    -- ^ Predicate
  -> Int            -- ^ Minimum number of elements that must satisfy the predicate
  -> f a            -- ^ Foldable
  -> Bool           -- ^ True if at least /n/ elements satisfy the predicate
atLeast p n f = length (filterF p f) >= n

-- | True if the foldable containts /exactly/ /n/ elements that satisfy the
-- given predicate.
exactly
  :: (Applicative f, Foldable f, Monoid (f a))
  => (a -> Bool)    -- ^ Predicate
  -> Int            -- ^ Exact number of elements that must satisfy the predicate
  -> f a            -- ^ Foldable
  -> Bool           -- ^ True if exactly /n/ elements satisfy the predicate
exactly p n f = length (filterF p f) == n
