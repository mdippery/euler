module Euler.Grid
  ( GridDimensions
  , GridLine (..)
  , Grid (..)
  , GridDirection (..)
  , cell
  , gridLineProduct
  ) where

type GridDimensions = (Int, Int)

data GridLine = GridLine4 Int Int Int Int deriving (Eq, Show)

data Grid = Grid GridDimensions [Int] deriving Eq

data GridDirection = GRight | GDown | GDiagonal | GCurrent deriving (Eq, Show)

instance Show Grid where
  show (Grid _ g) = show g

cell :: Int -> GridDirection -> Grid -> Maybe Int
cell _ _ (Grid _ []) = Nothing
cell i GCurrent (Grid _ d) = Just $ d !! i
cell i GRight (Grid (w,_) d)
  | hasRight i w = Just $ d !! (i + 1)
  | otherwise    = Nothing
cell i GDown (Grid (_,h) d)
  | hasDown i h (length d) = Just $ d !! (i + h)
  | otherwise              = Nothing
cell i GDiagonal (Grid (w,h) d)
  | noRight i w           = Nothing
  | noDown i h (length d) = Nothing
  | otherwise             = Just $ d !! (i + h + 1)

hasRight :: Int -> Int -> Bool
hasRight i w = (i + 1) `rem` w /= 0

hasDown :: Int -> Int -> Int -> Bool
hasDown i h l = i + h < l

noRight :: Int -> Int -> Bool
noRight i w = not $ hasRight i w

noDown :: Int -> Int -> Int -> Bool
noDown i h l = not $ hasDown i h l

gridLineProduct :: GridLine -> Int
gridLineProduct (GridLine4 a b c d) = a * b * c * d
