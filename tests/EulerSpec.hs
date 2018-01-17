{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module EulerSpec where

import Euler
import Test.Hspec

spec :: Spec
spec = do
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
      problem11 `shouldBe` 70600674

  describe "Verify that problem 12" $ do
    it "is correct" $ do
      problem12 `shouldBe` 76576500

  describe "Verify that problem 13" $ do
    it "is correct" $ do
      problem13 `shouldBe` "5537376230"

  describe "Verify that problem 14" $ do
    it "is correct" $ do
      problem14 `shouldBe` 837799

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
      problem18 `shouldBe` 1074

  describe "Verify that problem 19" $ do
    it "is correct" $ do
      problem19 `shouldBe` 171

  describe "Verify that problem 20" $ do
    it "is correct" $ do
      problem20 `shouldBe` 648

  describe "Verify that problem 21" $ do
    it "is correct" $ do
      problem21 `shouldBe` 31626

  describe "Verify that problem 22" $ do
    it "is correct" $ do
      score <- problem22
      score `shouldBe` 871198282

  describe "Verify that problem 23" $ do
    it "is correct" $ do
      problem23 `shouldBe` 4179871

  describe "Verify that problem 24" $ do
    it "is correct" $ do
      problem24 `shouldBe` "2783915460"

  describe "Verify that problem 25" $ do
    it "is correct" $ do
      problem25 `shouldBe` 4782

  describe "Verify that problem 26" $ do
    it "is correct" $ do
      problem26 `shouldBe` 983

  describe "Verify that problem 28" $ do
    it "is correct" $ do
      problem28 `shouldBe` 669171001

  describe "Verify that problem 29" $ do
    it "is correct" $ do
      problem29 `shouldBe` 9183

  describe "Verify that problem 30" $ do
    it "is correct" $ do
      problem30 `shouldBe` 443839

  describe "Verify that problem 31" $ do
    it "is correct" $ do
      problem31 `shouldBe` 73682

  describe "Verify that problem 34" $ do
    it "is correct" $ do
      problem34 `shouldBe` 40730

  describe "Verify that problem 35" $ do
    it "is correct" $ do
      problem35 `shouldBe` 55

  describe "Verify that problem 36" $ do
    it "is correct" $ do
      problem36 `shouldBe` 872187

  describe "Verify that problem 37" $ do
    it "is correct" $ do
      problem37 `shouldBe` 748317

  describe "Verify that problem 39" $ do
    it "is correct" $ do
      problem39 `shouldBe` 840

  describe "Verify that problem 40" $ do
    it "is correct" $ do
      problem40 `shouldBe` 210

  describe "Verify that problem 42" $ do
    it "is correct" $ do
      count <- problem42
      count `shouldBe` 162

  describe "Verify that problem 44" $ do
    it "is correct" $ do
      problem44 `shouldBe` 5482660

  describe "Verify that problem 45" $ do
    it "is correct" $ do
      problem45 `shouldBe` 1533776805

  describe "Verify that problem 46" $ do
    it "is correct" $ do
      problem46 `shouldBe` 5777

  describe "Verify that problem 48" $ do
    it "is correct" $ do
      problem48 `shouldBe` 9110846700

  describe "Verify that problem 52" $ do
    it "is correct" $ do
      problem52 `shouldBe` 142857

  describe "Verify that problem 53" $ do
    it "is correct" $ do
      problem53 `shouldBe` 4075

  describe "Verify that problem 54" $ do
    it "is correct" $ do
      count <- problem54
      count `shouldBe` 376

  describe "Verify that problem 55" $ do
    it "is correct" $ do
      problem55 `shouldBe` 249

  describe "Verify that problem 56" $ do
    it "is correct" $ do
      problem56 `shouldBe` 972

  describe "Verify that problem 67" $ do
    it "is correct" $ do
      problem67 `shouldBe` 7273

  describe "Verify that problem 69" $ do
    it "is correct" $ do
      problem69 `shouldBe` 510510

  describe "Verify that problem 71" $ do
    it "is correct" $ do
      problem71 `shouldBe` 428570

  describe "Verify that problem 74" $ do
    it "is correct" $ do
      problem74 `shouldBe` 402

  describe "Verify that problem 100" $ do
    it "is correct" $ do
      problem100 `shouldBe` 756872327473
