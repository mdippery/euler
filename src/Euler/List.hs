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
module Euler.List where

import Euler.Tuple (flattenT)

-- | Appends a value to a list.
(<:) :: [a]   -- ^ List
     -> a     -- ^ Value to append to the list
     -> [a]   -- ^ New list with the given value appended
as <: b = as ++ [b]

-- | 'True' if the list is empty.
isEmpty :: [a]    -- ^ List
        -> Bool   -- ^ 'True' if the list is empty
isEmpty [] = True
isEmpty _  = False

-- | Returns the middle elements of a list.
--
-- The middle of a list is all the elements except for the first and the last.
-- For example:
--
-- > middle [1..10] == [2,3,4,5,6,7,8,9]
middle :: [a]   -- ^ List
       -> [a]   -- ^ List with the first and last elements removed
middle = init . tail

-- | Next-to-last element of a list.
penultimate :: [a]  -- ^ List
            -> a    -- ^ Next-to-last element of the list
penultimate = last . init

-- | Duplicates each element of the list.
--
-- For example,
--
-- > duplicate [1,2,3,4,5] == [1,1,2,2,3,3,4,4,5,5]
duplicate :: [a]  -- ^ List
          -> [a]  -- ^ New list with all elements duplicated
duplicate = foldr (\e memo -> e : e : memo) []

-- | Duplicates only the 'middle' of a list.
--
-- For example,
--
-- > fatten [1,2,3,4,5] == [1,2,2,3,3,4,4,5]
fatten :: [a]   -- ^ List
       -> [a]   -- ^ New list with middle elements duplicated
fatten ls = head ls : duplicate (middle ls) <: last ls

-- | Combines all elements with their index in the list, starting with 0.
--
-- The resulting list is a list of 2-tuples in the form (index, element).
-- For example,
--
-- > zipWithIndex [10,20,30] == [(0,10), (1,20), (2,30)]
zipWithIndex :: (Enum b, Num b)
             => [a]       -- ^ List
             -> [(b, a)]  -- ^ New list containing a combination of indexes and elements
zipWithIndex = zipWithIndexAt 0

-- | Combines all elements with their index in the list, starting with the
-- given index.
--
-- The resulting list is a list of 2-tuples in the form (index, element).
-- For example,
--
-- > zipWithIndex 2 [10,20,30] == [(2,10), (3,20), (4,30)]
zipWithIndexAt :: (Enum b, Num b)
               => b         -- ^ Starting index
               -> [a]       -- ^ List
               -> [(b, a)]  -- ^ List elements combined with their index in the list
zipWithIndexAt = zip . enumFrom

-- | Removes indexes from a list of elements.
--
-- This is the reverse operation of 'zipWithIndex'. Therefore,
--
-- > (unzipWithIndex . zipWithIndex) ls == ls
-- > (unzipWithIndex . zipWithIndexAt 10) ls == ls
unzipWithIndex :: [(b, a)]  -- ^ List
               -> [a]       -- ^ List with indexes removed
unzipWithIndex = map snd

-- | Removes every nth element from a list.
--
-- > dropNth 2 [1..10] == [1,3,5,7,9]
dropNth :: Integral b
        => b    -- ^ nth element to remove
        -> [a]  -- ^ List
        -> [a]  -- ^ List with every n elements removed
dropNth n =
  let step n (i, x) acc = if i `rem` n == 0 then acc else (i, x):acc
   in unzipWithIndex . foldr (step n) [] . zipWithIndex

-- | Replaces an element at the given index.
--
-- If the given index does not exist in the list, the unmodified list is
-- returned. Therefore,
--
-- > replaceAt 3 20 [1..10] == [1,2,3,20,5,6,7,8,9,10]
-- > replaceAt 100 20 [1..10] == [1,2,3,4,5,6,7,8,9,10]
-- > replaceAt 0 20 [] == []
replaceAt :: Int  -- ^ Index to replace
          -> a    -- ^ Replacement element
          -> [a]  -- ^ List
          -> [a]  -- ^ List with the nth element replaced by the given value
replaceAt i e ls =
  case splitAt i ls of
    ([], []) -> []
    (h, []) -> ls
    (h, rest) -> h ++ [e] ++ tail rest

-- | Returns all sublists of the given size.
--
-- For example,
--
-- > windows 4 [1..6] == [[1,2,3,4], [2,3,4,5], [3,4,5,6]]
windows :: Int    -- ^ Desired size of sublists
        -> [a]    -- ^ List
        -> [[a]]  -- ^ Sublists of the desired size
windows n [] = []
windows n ls
  | length ls >= n = take n ls : windows n (tail ls)
  | otherwise      = []

-- | Moves the last element of a list to the front.
--
-- For example,
--
-- > rotateOnce [1..5] == [5,1,2,3,4]
rotateOnce :: [a]   -- ^ List
           -> [a]   -- ^ List with last element moved to the front
rotateOnce ls = last ls : init ls

-- | All possible rotations of a list.
--
-- For example,
--
-- > rotations [1,2,3] == [[1,2,3], [2,3,1], [3,1,2]]
rotations :: [a]    -- ^ List
          -> [[a]]  -- ^ List of all rotations
rotations ls = go ls (length ls) []
  where
    go _ 0 acc = acc
    go ls n acc =
      let ls' = rotateOnce ls
       in go ls' (n - 1) (ls' : acc)

-- | Splits a list into n-sized chunks.
splitEvery :: Int     -- ^ Desired size of chunks
           -> [a]     -- ^ List
           -> [[a]]   -- ^ List of lists of size n
splitEvery _ [] = []
splitEvery n ls =
  let (h,rest) = splitAt n ls
   in h : splitEvery n rest
