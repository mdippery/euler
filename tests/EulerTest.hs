import Euler
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Verify that problem 15 is correct" $ do
    it "is correct" $ do
      problem15 `shouldBe` 137846528820

  describe "Verify that problem 24 is correct" $ do
    it "is correct" $ do
      problem24 `shouldBe` "2783915460"

  describe "Verify that problem 30 is correct" $ do
    it "is correct" $ do
      problem30 `shouldBe` 443839

  describe "Verify that problem 34 is correct" $ do
    it "is correct" $ do
      problem34 `shouldBe` 40730
