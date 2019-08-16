{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Grid
  Description : Models a grid of numbers
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Provides functions for working with a grid of numbers as described in
  <https://projecteuler.net/problem=11 Euler Problem #11>.
-}
module Euler.Grid
  (
    -- * Data types
    Grid (..)
  , GridDimensions
  , GridDirection (..)
  , GridLine
  , Moveable
  , Skippable
  , moveModifier
  , skipModifier

    -- * Accessors
  , gridHeight
  , gridWidth

    -- * Basic functions
  , gridLines
  , rows

    -- * Utility functions
  , canMove
  , cell
  , columnIndex
  , gridLine
  , rowIndex
  ) where

import Control.Monad (liftM2)
import Data.List (nub, sort)
import Data.Maybe (fromMaybe, isJust)

import Euler.Data.List (splitEvery, zipWithIndex)


-- | A general data type that can indicate movement
class Moveable a where
  -- | Indicates the "cost" of moving in a given direction
  moveModifier :: a -> (Int, Int)

-- | A general data type that indicates how many cells to skip in a grid
class Skippable a where
  -- | Number of cells to skip in a grid
  skipModifier :: a -> Grid -> (Int -> Int)

-- | Dimensions of a grid of numbers.
--
-- Use 'gridHeight' and 'gridWidth' to retrieve the height and width of a grid.
type GridDimensions = (Int, Int)

-- | A single line from a grid of numbers
type GridLine = [Int]

-- | A data structure representing a grid of integers.
--
-- See <https://projecteuler.net/problem=11 Euler Problem #11> for a more
-- concise description of this data structure.
data Grid = Grid
  { gridDimensions :: GridDimensions  -- ^ Dimensions of the grid
  , gridData :: [Int]                 -- ^ Integers that make up the grid
  } deriving Eq

-- | A direction to traverse through the grid
data GridDirection = GCurrent
                   | GUp
                   | GUpRight
                   | GRight
                   | GDownRight
                   | GDown
                   | GDownLeft
                   | GLeft
                   | GUpLeft
  deriving (Enum, Eq, Show)

-- | Converts a grid to a string
instance Show Grid where
  show (Grid _ g) = show g

-- | Defines movement modifiers for grid directions
instance Moveable GridDirection where
  moveModifier GCurrent = (0,0)
  moveModifier GUp = (-1,0)
  moveModifier GUpRight = (-1,1)
  moveModifier GRight = (0,1)
  moveModifier GDownRight = (1,1)
  moveModifier GDown = (1,0)
  moveModifier GDownLeft = (1,-1)
  moveModifier GLeft = (0,-1)
  moveModifier GUpLeft = (-1,-1)

-- | Defines skip modifiers for grid directions
instance Skippable GridDirection where
  skipModifier d (Grid (_,h) _) =
    let (dm,rm) = moveModifier d
        hm = dm * h
     in (+ hm) . (+ rm)

-- | Height of a grid
gridHeight :: Grid -> Int
gridHeight = snd . gridDimensions

-- | Width of a grid
gridWidth :: Grid -> Int
gridWidth = fst . gridDimensions

-- | A list of all the rows contained in a grid
rows :: Grid -> [[Int]]
rows = liftM2 splitEvery gridWidth gridData

-- | Numbered row for the given cell, starting at 0.
rowIndex :: Int   -- ^ Current cell
         -> Grid  -- ^ Grid
         -> Int   -- ^ Numbered row that contains the given cell
rowIndex = (. gridHeight) . div

-- | Numbered column for the given cell, starting at 0.
columnIndex :: Int    -- ^ Current cell
            -> Grid   -- ^ Grid
            -> Int    -- ^ Numbered column that contains the given cell
columnIndex = (. gridWidth) . rem

-- | Indicates if one can move in the given direction from the starting
-- cell.
canMove :: Int            -- ^ Index of the current cell
        -> GridDirection  -- ^ Direction to move
        -> Grid           -- ^ Grid
        -> Bool           -- ^ True if one can move in the given direction from the current cell
canMove current dir g =
  let rs = rows g
      (rm,cm) = moveModifier dir
      r = rowIndex current g
      c = columnIndex current g
      r' = r + rm
      c' = c + cm
   in r' >= 0 && c' >= 0 && r' < length rs && c' < length (rs !! r')

-- | Returns the value for the cell in a given direction from the current cell.
--
-- If moving in the given direction from the current cell is illegal ("off the
-- grid", i.e., that cell doesn't exist), 'Nothing' is returned.
--
-- If
--
-- > canMove n dir g
--
-- returns True, then this function is guaranteed to return a 'Just'.
cell :: Int             -- ^ Current cell
     -> GridDirection   -- ^ Direction to move
     -> Grid            -- ^ Grid
     -> Maybe Int       -- ^ Value of the cell in the given direction from the current cell, if such a cell exists.
cell _ _ (Grid _ []) = Nothing
cell i d g
  | not $ canMove i d g = Nothing
  | otherwise =
    let rs = rows g
        (rm, cm) = moveModifier d
        r = rowIndex i g
        c = columnIndex i g
        r' = r + rm
        c' = c + cm
     in Just $ rs !! r' !! c'

-- | Returns a grid line of the given length, starting at the current cell,
-- pointing in the given direction.
--
-- If no such grid line exists (i.e., part -- or all of it would extend past
-- the bounds of the grid), then 'Nothing' is returned.
gridLine :: Int             -- ^ Desired length of grid line
         -> Int             -- ^ Current cell
         -> GridDirection   -- ^ Direction of grid line
         -> Grid            -- ^ Grid
         -> Maybe GridLine  -- ^ Grid line, if it exists
gridLine size at to g =
  let s = size - 1
      f = skipModifier to g
      cell' = flip (`cell` to) g
   in sequence $ cell at GCurrent g : map cell' (take s [at,f at..])

-- | All grid lines of a given size that exist in the grid.
gridLines :: Int          -- ^ Desired size of grid lines
          -> Grid         -- ^ Grid
          -> [GridLine]   -- ^ A list of all grid lines of the desired size
gridLines size g@(Grid _ cs) =
  let is = (map fst . zipWithIndex) cs
      combos = [(i,d) | i <- is, d <- [GUp .. GUpLeft]]
      f (i,d) = gridLine size i d g
   in (nub . map sort . fromMaybe [] . sequence . filter isJust . map f) combos
