{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.Grid where

import Euler.List (splitEvery, zipWithIndex)

class Moveable a where
  moveModifier :: a -> (Int, Int)

class Skippable a where
  skipModifier :: a -> Grid -> (Int -> Int)

type GridDimensions = (Int, Int)

type GridLine = [Int]

data Grid = Grid GridDimensions [Int]
  deriving Eq

data GridDirection = GCurrent | GRight | GDown | GDiagonal
  deriving (Enum, Eq, Show)

instance Show Grid where
  show (Grid _ g) = show g

instance Moveable GridDirection where
  moveModifier GCurrent = (0,0)
  moveModifier GRight = (0,1)
  moveModifier GDown = (1,0)
  moveModifier GDiagonal = (1,1)

instance Skippable GridDirection where
  skipModifier GCurrent _ = (+ 0)
  skipModifier GRight _ = (+ 1)
  skipModifier GDown (Grid (_,h) _) = (+ h)
  skipModifier GDiagonal (Grid (_,h) _) = (+ h) . (+ 1)

rows :: Grid -> [[Int]]
rows (Grid (w,_) g) = splitEvery w g

rowIndex :: Int -> Grid -> Int
rowIndex i (Grid (_,h) _) = i `div` h

columnIndex :: Int -> Grid -> Int
columnIndex i (Grid (w,_) _) = i `rem` w

canMove :: Int -> GridDirection -> Grid -> Bool
canMove current dir g =
  let rs = rows g
      (rm,cm) = moveModifier dir
      r = rowIndex current g
      c = columnIndex current g
      r' = r + rm
      c' = c + cm
   in r' < length rs && c' < length (rs !! r')

cell :: Int -> GridDirection -> Grid -> Maybe Int
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

gridLine :: Int -> Int -> GridDirection -> Grid -> Maybe GridLine
gridLine size at to g =
  let s = size - 1
      f = skipModifier to g
      cell' = flip (flip cell to) g
   in sequence $ cell at GCurrent g : (map cell' $ take s [at,f at..])

gridLines :: Int -> Grid -> [GridLine]
gridLines size g@(Grid _ cs) =
  let is = (map fst . zipWithIndex) cs
      combos = [(i,d) | i <- is, d <- [GRight,GDiagonal,GDown]]
      f (i,d) = gridLine size i d g
      extract [] = []
      extract (Nothing:rest) = extract rest
      extract (Just x:rest) = x : extract rest
   in extract $ map f combos
