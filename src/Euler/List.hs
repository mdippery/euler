{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.List
  Description : Additional list functions
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Provides missing functionality for Haskell lists.
-}
module Euler.List
  (
    -- * Basic properties
    isEmpty

    -- * Basic functions
  , (<:)
  , middle
  , penultimate

    -- * Transformations
  , duplicate
  , fatten
  , replaceAt
  , rotateOnce
  , rotations

    -- * Sublists
  , dropNth
  , splitEvery
  , windows

    -- * Zipping and unzipping
  , zipWithIndex
  , zipWithIndexFrom
  , unzipWithIndex
  ) where

import Data.List (genericLength, genericSplitAt, genericTake)

-- | Appends a value to a list.
(<:) :: [a]   -- ^ List
     -> a     -- ^ Value to append to the list
     -> [a]   -- ^ New list with the given value appended
as <: b = as ++ [b]

-- | True if the list is empty.
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

-- | Returns the middle elements of a list.
--
-- The middle of a list is all the elements except for the first and the last.
--
-- ==== Examples
--
-- >>> middle [1..10]
-- [2,3,4,5,6,7,8,9]
middle :: [a] -> [a]
middle = init . tail

-- | Next-to-last element of a list.
penultimate :: [a] -> a
penultimate = last . init

-- | Duplicates each element of the list.
--
-- ==== Examples
--
-- >>> duplicate [1,2,3,4,5]
-- [1,1,2,2,3,3,4,4,5,5]
duplicate :: [a] -> [a]
duplicate = foldr (\e memo -> e : e : memo) []

-- | Duplicates only the 'middle' of a list.
--
-- ==== Examples
--
-- >>> fatten [1,2,3,4,5]
-- [1,2,2,3,3,4,4,5]
fatten :: [a] -> [a]
fatten ls = head ls : duplicate (middle ls) <: last ls

-- | Combines all elements with their index in the list, starting with 0.
--
-- The resulting list is a list of 2-tuples in the form (index, element).
--
-- ==== Examples
--
-- >>> zipWithIndex [10,20,30]
-- [(0,10), (1,20), (2,30)]
zipWithIndex :: (Enum b, Num b)
             => [a]       -- ^ List
             -> [(b, a)]  -- ^ New list containing a combination of indexes and elements
zipWithIndex = zipWithIndexFrom 0

-- | Combines all elements with their index in the list, starting with the
-- given index.
--
-- The resulting list is a list of 2-tuples in the form (index, element).
--
-- ==== Examples
--
-- >>> zipWithIndexFrom 2 [10,20,30]
-- [(2,10), (3,20), (4,30)]
zipWithIndexFrom :: (Enum b, Num b)
               => b         -- ^ Starting index
               -> [a]       -- ^ List
               -> [(b, a)]  -- ^ List elements combined with their index in the list
zipWithIndexFrom = zip . enumFrom

-- | Removes indexes from a list of elements.
--
-- This is the reverse operation of 'zipWithIndex'. Therefore,
--
-- > (unzipWithIndex . zipWithIndex) ls == ls
-- > (unzipWithIndex . zipWithIndexAt 10) ls == ls
unzipWithIndex :: [(b, a)]  -- ^ List
               -> [a]       -- ^ List with indexes removed
unzipWithIndex = map snd

-- | Removes every /nth/ element from a list.
--
-- ==== Examples
--
-- >>> dropNth 2 [1..10]
-- [1,3,5,7,9]
dropNth :: Integral b
        => b    -- ^ /nth/ element to remove
        -> [a]  -- ^ Original list
        -> [a]  -- ^ New list with every /n/ elements removed
dropNth n =
  let step n (i, x) acc = if i `rem` n == 0 then acc else (i, x):acc
   in unzipWithIndex . foldr (step n) [] . zipWithIndex

-- | Replaces an element at the given index.
--
-- If the given index does not exist in the list, the unmodified list is
-- returned.
--
-- ==== Examples
--
-- >>> replaceAt 3 20 [1..10]
-- [1,2,3,20,5,6,7,8,9,10]
-- >>> replaceAt 100 20 [1..10]
-- [1,2,3,4,5,6,7,8,9,10]
-- >>> replaceAt 0 20 []
-- []
replaceAt :: Integral a
          => a    -- ^ Index to replace
          -> b    -- ^ Replacement element
          -> [b]  -- ^ List
          -> [b]  -- ^ List with the /nth/ element replaced by the given value
replaceAt i e ls =
  case genericSplitAt i ls of
    ([], []) -> []
    (h, []) -> ls
    (h, rest) -> h ++ [e] ++ tail rest

-- | Returns all sublists of the given size.
--
-- ==== Examples
--
-- >>> windows 4 [1..6]
-- [[1,2,3,4], [2,3,4,5], [3,4,5,6]]
windows :: Integral a
        => a      -- ^ Desired size of sublists
        -> [b]    -- ^ List
        -> [[b]]  -- ^ Sublists of the desired size
windows n [] = []
windows n ls
  | genericLength ls >= n = genericTake n ls : windows n (tail ls)
  | otherwise = []

-- | Moves the last element of a list to the front.
--
-- ==== Examples
--
-- >>> rotateOnce [1..5]
-- [5,1,2,3,4]
rotateOnce :: [a] -> [a]
rotateOnce ls = last ls : init ls

-- | All possible rotations of a list.
--
-- ==== Examples
--
-- >>> rotations [1,2,3]
-- [[1,2,3], [2,3,1], [3,1,2]]
rotations :: [a] -> [[a]]
rotations ls = go ls (length ls) []
  where
    go _ 0 acc = acc
    go ls n acc =
      let ls' = rotateOnce ls
       in go ls' (n - 1) (ls' : acc)

-- | Splits a list into /n/-sized chunks.
splitEvery :: Integral a
           => a       -- ^ Desired size of chunks
           -> [b]     -- ^ List
           -> [[b]]   -- ^ List of lists of size /n/
splitEvery _ [] = []
splitEvery n ls =
  let (h,rest) = genericSplitAt n ls
   in h : splitEvery n rest
