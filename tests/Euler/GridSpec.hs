{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.GridSpec where

import Euler.Grid
import Test.Hspec

spec :: Spec
spec = do
  describe "Euler.Grid" $ do
    {-  0  1  2  3
        4  5  6  7
        8  9 10 11
       12 13 14 15
    -}
    let g = Grid (4,4) [0..15]
        e = Grid (0,0) []

    describe "GridDirection" $ do
      it "returns a move modifier" $ do
        moveModifier GCurrent `shouldBe` (0,0)
        moveModifier GRight `shouldBe` (0,1)
        moveModifier GDown `shouldBe` (1,0)
        moveModifier GDownRight `shouldBe` (1,1)

      it "returns a skip modifier" $ do
        skipModifier GCurrent g 5 `shouldBe` 5
        skipModifier GRight g 5 `shouldBe` 6
        skipModifier GDown g 5 `shouldBe` 9
        skipModifier GDownRight g 5 `shouldBe` 10

    describe "gridLine" $ do
      it "returns the gridline to the right from a given point" $ do
        gridLine 3 0 GRight g `shouldBe` Just [0,1,2]
        gridLine 3 1 GRight g `shouldBe` Just [1,2,3]
        gridLine 3 4 GRight g `shouldBe` Just [4,5,6]
        gridLine 3 5 GRight g `shouldBe` Just [5,6,7]
        gridLine 3 8 GRight g `shouldBe` Just [8,9,10]
        gridLine 3 9 GRight g `shouldBe` Just [9,10,11]
        gridLine 3 12 GRight g `shouldBe` Just [12,13,14]
        gridLine 3 13 GRight g `shouldBe` Just [13,14,15]

      it "returns nothing if there are no gridlines to the right" $ do
        gridLine 3 2 GRight g `shouldBe` Nothing
        gridLine 3 3 GRight g `shouldBe` Nothing
        gridLine 3 6 GRight g `shouldBe` Nothing
        gridLine 3 7 GRight g `shouldBe` Nothing
        gridLine 3 10 GRight g `shouldBe` Nothing
        gridLine 3 11 GRight g `shouldBe` Nothing
        gridLine 3 14 GRight g `shouldBe` Nothing
        gridLine 3 15 GRight g `shouldBe` Nothing

      it "returns the gridline going down from a given point" $ do
        gridLine 3 0 GDown g `shouldBe` Just [0,4,8]
        gridLine 3 1 GDown g `shouldBe` Just [1,5,9]
        gridLine 3 2 GDown g `shouldBe` Just [2,6,10]
        gridLine 3 3 GDown g `shouldBe` Just [3,7,11]
        gridLine 3 4 GDown g `shouldBe` Just [4,8,12]
        gridLine 3 5 GDown g `shouldBe` Just [5,9,13]
        gridLine 3 6 GDown g `shouldBe` Just [6,10,14]
        gridLine 3 7 GDown g `shouldBe` Just [7,11,15]

      it "returns nothing if there are no gridlines going down" $ do
        gridLine 3 8 GDown g `shouldBe` Nothing
        gridLine 3 9 GDown g `shouldBe` Nothing
        gridLine 3 10 GDown g `shouldBe` Nothing
        gridLine 3 11 GDown g `shouldBe` Nothing
        gridLine 3 12 GDown g `shouldBe` Nothing
        gridLine 3 13 GDown g `shouldBe` Nothing
        gridLine 3 14 GDown g `shouldBe` Nothing
        gridLine 3 15 GDown g `shouldBe` Nothing

      it "returns the gridline diagonal from a given point" $ do
        gridLine 3 0 GDownRight g `shouldBe` Just [0,5,10]
        gridLine 3 1 GDownRight g `shouldBe` Just [1,6,11]
        gridLine 3 4 GDownRight g `shouldBe` Just [4,9,14]
        gridLine 3 5 GDownRight g `shouldBe` Just [5,10,15]

      it "returns nothing if there are no diagonal gridlines" $ do
        gridLine 3 2 GDownRight g `shouldBe` Nothing
        gridLine 3 3 GDownRight g `shouldBe` Nothing
        gridLine 3 6 GDownRight g `shouldBe` Nothing
        gridLine 3 7 GDownRight g `shouldBe` Nothing
        gridLine 3 8 GDownRight g `shouldBe` Nothing
        gridLine 3 9 GDownRight g `shouldBe` Nothing
        gridLine 3 10 GDownRight g `shouldBe` Nothing
        gridLine 3 11 GDownRight g `shouldBe` Nothing
        gridLine 3 12 GDownRight g `shouldBe` Nothing
        gridLine 3 13 GDownRight g `shouldBe` Nothing
        gridLine 3 14 GDownRight g `shouldBe` Nothing
        gridLine 3 15 GDownRight g `shouldBe` Nothing

    describe "gridLines" $ do
      it "returns all the gridlines of a given size" $ do
        gridLines 3 g `shouldBe` [ [0,1,2], [0,5,10], [0,4,8]
                                 , [1,2,3], [1,6,11], [1,5,9]
                                 , [2,6,10], [2,5,8]
                                 , [3,7,11], [3,6,9]
                                 , [4,5,6], [4,9,14], [4,8,12]
                                 , [5,6,7], [5,10,15], [5,9,13]
                                 , [6,10,14], [6,9,12]
                                 , [7,11,15], [7,10,13]
                                 , [8,9,10], [9,10,11]
                                 , [12,13,14], [13,14,15]
                                 ]

      it "returns no gridlines for an empty grid" $ do
        gridLines 3 e `shouldBe` []

    describe "rows" $ do
      it "returns the rows of the grid" $ do
        rows g `shouldBe` [[0..3], [4..7], [8..11], [12..15]]

      it "returns an empty list of rows if the grid is empty" $ do
        rows e `shouldBe` []

    describe "canMove" $ do
      it "indicates whether the current position exists" $ do
        canMove 0 GCurrent g `shouldBe` True
        canMove 1 GCurrent g `shouldBe` True
        canMove 2 GCurrent g `shouldBe` True
        canMove 3 GCurrent g `shouldBe` True
        canMove 4 GCurrent g `shouldBe` True
        canMove 5 GCurrent g `shouldBe` True
        canMove 6 GCurrent g `shouldBe` True
        canMove 7 GCurrent g `shouldBe` True
        canMove 8 GCurrent g `shouldBe` True
        canMove 9 GCurrent g `shouldBe` True
        canMove 10 GCurrent g `shouldBe` True
        canMove 11 GCurrent g `shouldBe` True
        canMove 12 GCurrent g `shouldBe` True
        canMove 13 GCurrent g `shouldBe` True
        canMove 14 GCurrent g `shouldBe` True
        canMove 15 GCurrent g `shouldBe` True
        canMove 16 GCurrent g `shouldBe` False

      it "indicates whether there are moves to the right" $ do
        canMove 0 GRight g `shouldBe` True
        canMove 1 GRight g `shouldBe` True
        canMove 2 GRight g `shouldBe` True
        canMove 3 GRight g `shouldBe` False
        canMove 4 GRight g `shouldBe` True
        canMove 5 GRight g `shouldBe` True
        canMove 6 GRight g `shouldBe` True
        canMove 7 GRight g `shouldBe` False
        canMove 8 GRight g `shouldBe` True
        canMove 9 GRight g `shouldBe` True
        canMove 10 GRight g `shouldBe` True
        canMove 11 GRight g `shouldBe` False
        canMove 12 GRight g `shouldBe` True
        canMove 13 GRight g `shouldBe` True
        canMove 14 GRight g `shouldBe` True
        canMove 15 GRight g `shouldBe` False

      it "indicates whether there are moves down" $ do
        canMove 0 GDown g `shouldBe` True
        canMove 1 GDown g `shouldBe` True
        canMove 2 GDown g `shouldBe` True
        canMove 3 GDown g `shouldBe` True
        canMove 4 GDown g `shouldBe` True
        canMove 5 GDown g `shouldBe` True
        canMove 6 GDown g `shouldBe` True
        canMove 7 GDown g `shouldBe` True
        canMove 8 GDown g `shouldBe` True
        canMove 9 GDown g `shouldBe` True
        canMove 10 GDown g `shouldBe` True
        canMove 11 GDown g `shouldBe` True
        canMove 12 GDown g `shouldBe` False
        canMove 13 GDown g `shouldBe` False
        canMove 14 GDown g `shouldBe` False
        canMove 15 GDown g `shouldBe` False

      it "indicates whether there are moves diagonally" $ do
        canMove 0 GDownRight g `shouldBe` True
        canMove 1 GDownRight g `shouldBe` True
        canMove 2 GDownRight g `shouldBe` True
        canMove 3 GDownRight g `shouldBe` False
        canMove 4 GDownRight g `shouldBe` True
        canMove 5 GDownRight g `shouldBe` True
        canMove 6 GDownRight g `shouldBe` True
        canMove 7 GDownRight g `shouldBe` False
        canMove 8 GDownRight g `shouldBe` True
        canMove 9 GDownRight g `shouldBe` True
        canMove 10 GDownRight g `shouldBe` True
        canMove 11 GDownRight g `shouldBe` False
        canMove 12 GDownRight g `shouldBe` False
        canMove 13 GDownRight g `shouldBe` False
        canMove 14 GDownRight g `shouldBe` False
        canMove 15 GDownRight g `shouldBe` False

    describe "rowIndex" $ do
      it "returns the row index for a given index" $ do
        rowIndex 0 g `shouldBe` 0
        rowIndex 1 g `shouldBe` 0
        rowIndex 2 g `shouldBe` 0
        rowIndex 3 g `shouldBe` 0
        rowIndex 4 g `shouldBe` 1
        rowIndex 5 g `shouldBe` 1
        rowIndex 6 g `shouldBe` 1
        rowIndex 7 g `shouldBe` 1
        rowIndex 8 g `shouldBe` 2
        rowIndex 9 g `shouldBe` 2
        rowIndex 10 g `shouldBe` 2
        rowIndex 11 g `shouldBe` 2
        rowIndex 12 g `shouldBe` 3
        rowIndex 13 g `shouldBe` 3
        rowIndex 14 g `shouldBe` 3
        rowIndex 15 g `shouldBe` 3

    describe "columnIndex" $ do
      it "returns the column index into a row for a given index" $ do
        columnIndex 0 g `shouldBe` 0
        columnIndex 1 g `shouldBe` 1
        columnIndex 2 g `shouldBe` 2
        columnIndex 3 g `shouldBe` 3
        columnIndex 4 g `shouldBe` 0
        columnIndex 5 g `shouldBe` 1
        columnIndex 6 g `shouldBe` 2
        columnIndex 7 g `shouldBe` 3
        columnIndex 8 g `shouldBe` 0
        columnIndex 9 g `shouldBe` 1
        columnIndex 10 g `shouldBe` 2
        columnIndex 11 g `shouldBe` 3
        columnIndex 12 g `shouldBe` 0
        columnIndex 13 g `shouldBe` 1
        columnIndex 14 g `shouldBe` 2
        columnIndex 15 g `shouldBe` 3

    describe "cell" $ do
      it "returns nothing for the current cell if the grid is empty" $ do
        cell 0 GCurrent e `shouldBe` Nothing

      it "returns the current cell" $ do
        cell 0 GCurrent g `shouldBe` Just 0
        cell 1 GCurrent g `shouldBe` Just 1
        cell 2 GCurrent g `shouldBe` Just 2
        cell 3 GCurrent g `shouldBe` Just 3
        cell 4 GCurrent g `shouldBe` Just 4
        cell 5 GCurrent g `shouldBe` Just 5
        cell 6 GCurrent g `shouldBe` Just 6
        cell 7 GCurrent g `shouldBe` Just 7
        cell 8 GCurrent g `shouldBe` Just 8
        cell 9 GCurrent g `shouldBe` Just 9
        cell 10 GCurrent g `shouldBe` Just 10
        cell 11 GCurrent g `shouldBe` Just 11
        cell 12 GCurrent g `shouldBe` Just 12
        cell 13 GCurrent g `shouldBe` Just 13
        cell 14 GCurrent g `shouldBe` Just 14
        cell 15 GCurrent g `shouldBe` Just 15

      it "returns nothing if the current cell is out of bounds" $ do
        cell 16 GCurrent g `shouldBe` Nothing

      it "returns the cell on the right" $ do
        cell 0 GRight g `shouldBe` Just 1
        cell 1 GRight g `shouldBe` Just 2
        cell 2 GRight g `shouldBe` Just 3
        cell 4 GRight g `shouldBe` Just 5
        cell 5 GRight g `shouldBe` Just 6
        cell 6 GRight g `shouldBe` Just 7
        cell 8 GRight g `shouldBe` Just 9
        cell 9 GRight g `shouldBe` Just 10
        cell 10 GRight g `shouldBe` Just 11
        cell 12 GRight g `shouldBe` Just 13
        cell 13 GRight g `shouldBe` Just 14
        cell 14 GRight g `shouldBe` Just 15

      it "returns nothing if there are no more cells on the right" $ do
        cell 3 GRight g `shouldBe` Nothing
        cell 7 GRight g `shouldBe` Nothing
        cell 11 GRight g `shouldBe` Nothing
        cell 15 GRight g `shouldBe` Nothing

      it "returns the cell below" $ do
        cell 0 GDown g `shouldBe` Just 4
        cell 1 GDown g `shouldBe` Just 5
        cell 2 GDown g `shouldBe` Just 6
        cell 3 GDown g `shouldBe` Just 7
        cell 4 GDown g `shouldBe` Just 8
        cell 5 GDown g `shouldBe` Just 9
        cell 6 GDown g `shouldBe` Just 10
        cell 7 GDown g `shouldBe` Just 11
        cell 8 GDown g `shouldBe` Just 12
        cell 9 GDown g `shouldBe` Just 13
        cell 10 GDown g `shouldBe` Just 14
        cell 11 GDown g `shouldBe` Just 15

      it "returns nothing if there are no more cells below" $ do
        cell 12 GDown g `shouldBe` Nothing
        cell 13 GDown g `shouldBe` Nothing
        cell 14 GDown g `shouldBe` Nothing
        cell 15 GDown g `shouldBe` Nothing

      it "returns the cell diagonally" $ do
        cell 0 GDownRight g `shouldBe` Just 5
        cell 1 GDownRight g `shouldBe` Just 6
        cell 2 GDownRight g `shouldBe` Just 7
        cell 4 GDownRight g `shouldBe` Just 9
        cell 5 GDownRight g `shouldBe` Just 10
        cell 6 GDownRight g `shouldBe` Just 11
        cell 8 GDownRight g `shouldBe` Just 13
        cell 9 GDownRight g `shouldBe` Just 14
        cell 10 GDownRight g `shouldBe` Just 15

      it "returns nothing if there are no more diagonal cells" $ do
        cell 3 GDownRight g `shouldBe` Nothing
        cell 7 GDownRight g `shouldBe` Nothing
        cell 11 GDownRight g `shouldBe` Nothing
        cell 12 GDownRight g `shouldBe` Nothing
        cell 13 GDownRight g `shouldBe` Nothing
        cell 14 GDownRight g `shouldBe` Nothing
        cell 15 GDownRight g `shouldBe` Nothing
