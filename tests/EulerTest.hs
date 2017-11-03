import Euler
import Euler.Grid
import Euler.List
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Verify that problem 1" $ do
    it "is correct" $ do
      problem1 `shouldBe` 233168

  describe "Verify that problem 2" $ do
    it "is correct" $ do
      problem2 `shouldBe` 4613732

  describe "Verify that problem 3" $ do
    it "is correct" $ do
      problem3 `shouldBe` 6857

  describe "Verify that problem 4" $ do
    it "is correct" $ do
      problem4 `shouldBe` 906609

  describe "Verify that problem 5" $ do
    it "is correct" $ do
      problem5 `shouldBe` 232792560

  describe "Verify that problem 6" $ do
    it "is correct" $ do
      problem6 `shouldBe` 25164150

  describe "Verify that problem 7" $ do
    it "is correct" $ do
      problem7 `shouldBe` 104743

  describe "Verify that problem 8" $ do
    it "is correct" $ do
      problem8 `shouldBe` 23514624000

  describe "Verify that problem 9" $ do
    it "is correct" $ do
      problem9 `shouldBe` 31875000

  describe "Verify that problem 10" $ do
    it "is correct" $ do
      problem10 `shouldBe` 142913828922

  describe "Verify that problem 11" $ do
    it "is correct" $ do
      pending
      -- problem11 `shouldBe` 70600674

  describe "Verify that problem 12" $ do
    it "is correct" $ do
      pending
      -- problem12 `shouldBe` 76576500

  describe "Verify that problem 13" $ do
    it "is correct" $ do
      problem13 `shouldBe` "5537376230"

  describe "Verify that problem 14" $ do
    it "is correct" $ do
      pending
      -- problem14 `shouldBe` 837799

  describe "Verify that problem 15" $ do
    it "is correct" $ do
      problem15 `shouldBe` 137846528820

  describe "Verify that problem 16" $ do
    it "is correct" $ do
      problem16 `shouldBe` 1366

  describe "Verify that problem 17" $ do
    it "is correct" $ do
      problem17 `shouldBe` 21124

  describe "Verify that problem 18" $ do
    it "is correct" $ do
      pending
      -- problem18 `shouldBe` 1074

  describe "Verify that problem 19" $ do
    it "is correct" $ do
      pending
      -- problem19 `shouldBe` 171

  describe "Verify that problem 20" $ do
    it "is correct" $ do
      problem20 `shouldBe` 648

  describe "Verify that problem 21" $ do
    it "is correct" $ do
      pending
      -- problem21 `shouldBe` 31626

  describe "Verify that problem 22" $ do
    it "is correct" $ do
      pending
      -- problem22 `shouldBe` 871198282

  describe "Verify that problem 24" $ do
    it "is correct" $ do
      problem24 `shouldBe` "2783915460"

  describe "Verify that problem 25" $ do
    it "is correct" $ do
      pending
      -- problem25 `shouldBe` 4782

  describe "Verify that problem 29" $ do
    it "is correct" $ do
      problem29 `shouldBe` 9183

  describe "Verify that problem 30" $ do
    it "is correct" $ do
      problem30 `shouldBe` 443839

  describe "Verify that problem 34" $ do
    it "is correct" $ do
      problem34 `shouldBe` 40730

  describe "Verify that problem 35" $ do
    it "is correct" $ do
      pending
      -- problem35 `shouldBe` 55

  describe "Verify that problem 36" $ do
    it "is correct" $ do
      problem36 `shouldBe` 872187

  describe "Verify that problem 48" $ do
    it "is correct" $ do
      problem48 `shouldBe` 9110846700

  describe "Verify that problem 53" $ do
    it "is correct" $ do
      pending
      -- problem53 `shouldBe` 4075

  describe "Verify that problem 56" $ do
    it "is correct" $ do
      pending
      -- problem56 `shouldBe` 972

  describe "Verify that problem 67" $ do
    it "is correct" $ do
      pending
      -- problem67 `shouldBe` 7273

  describe "Verify that `zipWithIndex`" $ do
    it "produces an empty list when given an empty list" $ do
      length (zipWithIndex []) `shouldBe` 0

    it "produces a list with indices" $ do
      zipWithIndex [2..10] `shouldBe` [(0,2), (1,3), (2,4), (3,5), (4,6), (5,7), (6,8), (7,9), (8,10)]

  describe "Verify that `unzipWithIndex`" $ do
    it "produces an empty list when given an empty list" $ do
      length (unzipWithIndex []) `shouldBe` 0

    it "removes indices from a list" $ do
      unzipWithIndex [(0,2), (1,3), (2,4), (3,5), (4,6), (5,7), (6,8), (7,9), (8,10)] == [2..10]

  describe "Verify that `dropNth`" $ do
    it "produces an empty list when given an empty list" $ do
      length (dropNth 2 []) `shouldBe` 0

    it "drops the correct elements from a list" $ do
      dropNth 2 [0..10] `shouldBe` [1,3..10]

  describe "Verify that `windows`" $ do
    it "produces an empty list when given an empty list" $ do
      length (windows 4 []) `shouldBe` 0

    it "produces successive windows of size 4" $ do
      windows 4 "abcdefgh" `shouldBe` ["abcd", "bcde", "cdef", "defg", "efgh"]

  describe "Ensure Grid" $ do
    {-  0  1  2 3
        4  5  6 7
        8  9 10 11
       12 13 14 15
    -}
    let g = Grid (4,4) [0..15]
        e = Grid (0,0) []

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
      cell 0 GDiagonal g `shouldBe` Just 5
      cell 1 GDiagonal g `shouldBe` Just 6
      cell 2 GDiagonal g `shouldBe` Just 7
      cell 4 GDiagonal g `shouldBe` Just 9
      cell 5 GDiagonal g `shouldBe` Just 10
      cell 6 GDiagonal g `shouldBe` Just 11
      cell 8 GDiagonal g `shouldBe` Just 13
      cell 9 GDiagonal g `shouldBe` Just 14
      cell 10 GDiagonal g `shouldBe` Just 15

    it "returns nothing if there are no more diagonal cells" $ do
      cell 3 GDiagonal g `shouldBe` Nothing
      cell 7 GDiagonal g `shouldBe` Nothing
      cell 11 GDiagonal g `shouldBe` Nothing
      cell 12 GDiagonal g `shouldBe` Nothing
      cell 13 GDiagonal g `shouldBe` Nothing
      cell 14 GDiagonal g `shouldBe` Nothing
      cell 15 GDiagonal g `shouldBe` Nothing

  describe "Ensure GridLine" $ do
    it "can be muliplied to calculate a product" $ do
      gridLineProduct (GridLine4 2 3 5 10) `shouldBe` 300
