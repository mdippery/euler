{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Data.List.Euler
  Description : Additional list functions
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Provides missing functionality for Haskell lists.
-}
module Data.List.Euler
  (
    -- * Basic functions
    (<:)
  , head
  , init
  , middle
  , penultimate
  , tail

    -- * Transformations
  , duplicate
  , fatten
  , replace
  , rotateOnce
  , rotations
  , splitEvery

    -- * Sublists
  , dropNth
  , longestPrefix
  , windows

    -- * Searching
  , findConsecutive

    -- * Sorting
  , descendingSort

    -- * Zipping and unzipping
  , unzipWithIndex
  , zipWithIndex
  , zipWithIndexFrom
  ) where

import Prelude hiding (head, init, tail)
import qualified Prelude as P (head, init, tail)

import Control.Monad (ap, liftM2)
import Data.Bool (bool)
import Data.List (genericLength, genericSplitAt, genericTake, sortOn, unfoldr)
import Data.Ord (Down(..))

-- | Appends a value to a list.
(<:) :: [a] -> a -> [a]
xs <: x = xs ++ [x]

-- | Returns the first element of a list, or 'Nothing' if the list is empty.
--
-- This is a "safe" version of 'P.head'.
head :: [a] -> Maybe a
head = ap (bool Nothing . Just . P.head) (not . null)

-- | Returns the tail of a list, or an empty list if the list is empty.
--
-- This is a "safe" version of 'P.tail'.
tail :: [a] -> [a]
tail = ap (bool [] . P.tail) (not . null)

-- | Returns all elements of a list except the last one, or an empty
-- list if the list is empty.
--
-- This is a "safe" version of P.init
init :: [a] -> [a]
init = ap (bool [] . P.init) (not . null)

-- | Returns the middle elements of a list.
--
-- The middle of a list comprises all the elements of the list minus the first
-- and last elements.
--
-- ==== Examples
--
-- >>> middle [1..10]
-- [2,3,4,5,6,7,8,9]
middle :: [a] -> [a]
middle = init . tail

-- | Next-to-last element of a list, or 'Nothing' if the list is empty.
penultimate :: [a] -> Maybe a
penultimate = ap (bool Nothing . Just . last . init) (not . null)

-- | Duplicates each element of a list.
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
fatten = init . tail . duplicate

-- | Combines all elements with their index in the list, starting with 0.
--
-- The resulting list is a list of 2-tuples in the form (index, element).
--
-- ==== Examples
--
-- >>> zipWithIndex [10,20,30]
-- [(0,10), (1,20), (2,30)]
zipWithIndex :: (Enum b, Num b) => [a] -> [(b, a)]
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
zipWithIndexFrom
  :: (Enum b, Num b)
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
unzipWithIndex :: [(b, a)] -> [a]
unzipWithIndex = map snd

-- | Removes every /nth/ element from the list.
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

-- | Returns the longest prefix common to both of the given lists, which may
-- be an empty list.
--
-- ==== Examples
--
-- >>> longestPrefix "124" "123456789"
-- "12"
longestPrefix
  :: Eq a
  => [a]      -- ^ First list
  -> [a]      -- ^ Second list
  -> [a]      -- ^ Prefix common to both lists
longestPrefix = ((map fst . filter (uncurry (==))) .) . zip

-- | Replaces an element at the given index.
--
-- If the given index does not exist in the list, the unmodified list is
-- returned.
--
-- ==== Examples
--
-- >>> replaceAt 3 20 [1..10]
-- [1,2,3,20,5,6,7,8,9,10]
--
-- >>> replaceAt 100 20 [1..10]
-- [1,2,3,4,5,6,7,8,9,10]
--
-- >>> replaceAt 0 20 []
-- []
replace
  :: Integral a
  => a    -- ^ Index to replace
  -> b    -- ^ Replacement element
  -> [b]  -- ^ List
  -> [b]  -- ^ List with the /nth/ element replaced by the given value
replace i e ls =
  case genericSplitAt i ls of
    ([], [])  -> []
    (_, [])   -> ls
    (h, rest) -> h ++ [e] ++ tail rest

-- | Returns all sublists of the given size.
--
-- ==== Examples
--
-- >>> windows 4 [1..6]
-- [[1,2,3,4], [2,3,4,5], [3,4,5,6]]
windows
  :: Integral a
  => a      -- ^ Desired size of sublists
  -> [b]    -- ^ List
  -> [[b]]  -- ^ Sublists of the desired size
windows n = unfoldr go
  where
    go ls
      | genericLength ls >= n = Just (genericTake n ls, tail ls)
      | otherwise             = Nothing

-- | Moves the last element of the list to the front.
--
-- ==== Examples
--
-- >>> rotateOnce [1..5]
-- [5,1,2,3,4]
rotateOnce :: [a] -> [a]
rotateOnce = ap (bool [] . liftM2 (:) last P.init) (not . null)

-- | All possible rotations of a list.
--
-- ==== Examples
--
-- >>> rotations [1,2,3]
-- [[1,2,3], [3,1,2], [2,3,1]]
rotations :: [a] -> [[a]]
rotations = liftM2 genericTake genericLength (iterate rotateOnce)

-- | Splits a list into /n/-sized chunks.
splitEvery
  :: Integral a
  => a       -- ^ Desired size of chunks
  -> [b]     -- ^ List
  -> [[b]]   -- ^ List of lists of size /n/
splitEvery _ [] = []
splitEvery n ls =
  let (h,rest) = genericSplitAt n ls
   in h : splitEvery n rest

-- | Finds a sublist of consecutive elements of length /n/. Returns the first
-- sublist found, or 'Nothing' if there is not a consecutive subsquence of
-- length /n/.
findConsecutive
  :: Integral a
  => a          -- ^ Length of desired subsequence
  -> [a]        -- ^ List to search
  -> Maybe [a]  -- ^ Subsequence of consecutive elements of length /n/
findConsecutive n ls =
  case dropWhile (not . isConsecutive) (windows n ls) of
    [] -> Nothing
    ws -> Just (P.head ws)
  where
    isConsecutive = ap (==) (genericTake n . enumFrom . P.head)

-- | Sorts a list in descending order.
descendingSort :: Ord a => [a] -> [a]
descendingSort = sortOn Down
