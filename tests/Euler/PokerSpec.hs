{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.PokerSpec where

import Euler.Poker
import Test.Hspec

spec :: Spec
spec = do
  describe "isHighCard" $ do
    it "returns true if the hand is a high card hand" $ do
      let h = PlayerHand PlayerOne (Card Two Clubs) (Card Three Diamonds) (Card Four Spades) (Card Five Hearts) (Card Seven Diamonds)
      isHighCard h `shouldBe` True

    it "returns false if the hand is a straight" $ do
      let h = PlayerHand PlayerOne (Card Two Clubs) (Card Three Diamonds) (Card Four Spades) (Card Five Hearts) (Card Six Diamonds)
      isHighCard h `shouldBe` False

    it "returns false if the hand is not a high card hand" $ do
      let h = PlayerHand PlayerOne (Card Two Clubs) (Card Two Diamonds) (Card Four Spades) (Card Five Hearts) (Card Six Diamonds)
      isHighCard h `shouldBe` False

  describe "isOnePair" $ do
    it "returns true if the hand is one pair" $ do
      let h = PlayerHand PlayerOne (Card Two Clubs) (Card Two Diamonds) (Card Four Spades) (Card Five Hearts) (Card Six Diamonds)
      isOnePair h `shouldBe` True

    it "returns false if the hand is three of a kind" $ do
      let h = PlayerHand PlayerOne (Card Two Clubs) (Card Two Diamonds) (Card Two Spades) (Card Four Hearts) (Card Six Diamonds)
      isOnePair h `shouldBe` False

    it "returns false if the hand is not one pair" $ do
      let h = PlayerHand PlayerOne (Card Ten Diamonds) (Card Jack Diamonds) (Card Queen Diamonds) (Card King Diamonds) (Card Ace Diamonds)
      isOnePair h `shouldBe` False

  describe "isTwoPair" $ do
    it "returns true if the hand is two pairs" $ do
      let h = PlayerHand PlayerOne (Card Two Clubs) (Card Two Diamonds) (Card Four Spades) (Card Four Hearts) (Card Six Diamonds)
      isTwoPair h `shouldBe` True

    it "returns false if the hand is not two pairs" $ do
      let h = PlayerHand PlayerOne (Card Ten Diamonds) (Card Jack Diamonds) (Card Queen Diamonds) (Card King Diamonds) (Card Ace Diamonds)
      isTwoPair h `shouldBe` False

  describe "isThreeKind" $ do
    it "returns true if the hand is three of a kind" $ do
      let h = PlayerHand PlayerOne (Card Two Clubs) (Card Two Diamonds) (Card Two Spades) (Card Four Hearts) (Card Six Diamonds)
      isThreeKind h `shouldBe` True

    it "returns false if the hand is a full house" $ do
      let h = PlayerHand PlayerOne (Card Two Spades) (Card Two Diamonds) (Card Two Clubs) (Card King Spades) (Card King Diamonds)
      isThreeKind h `shouldBe` False

    it "returns false if the hand is not three of a kind" $ do
      let h = PlayerHand PlayerOne (Card Ten Diamonds) (Card Jack Diamonds) (Card Queen Diamonds) (Card King Diamonds) (Card Ace Diamonds)
      isThreeKind h `shouldBe` False

  describe "isStraight" $ do
    it "returns true if the hand is a straight" $ do
      let h = PlayerHand PlayerOne (Card Nine Spades) (Card Ten Diamonds) (Card Jack Hearts) (Card Queen Diamonds) (Card King Spades)
      isStraight h `shouldBe` True

    it "returns false if the hand is not a straight" $ do
      let h = PlayerHand PlayerOne (Card Eight Spades) (Card Ten Diamonds) (Card Jack Hearts) (Card Queen Diamonds) (Card King Spades)
      isStraight h `shouldBe` False

  describe "isFlush" $ do
    it "returns true if the hand is a flush" $ do
      let h = PlayerHand PlayerOne (Card Two Spades) (Card Two Spades) (Card Two Spades) (Card Two Spades) (Card Two Spades)
      isFlush h `shouldBe` True

    it "returns false if the hand is not a flush" $ do
      let h = PlayerHand PlayerOne (Card Two Spades) (Card Two Diamonds) (Card Two Hearts) (Card Two Clubs) (Card Two Spades)
      isFlush h `shouldBe` False

  describe "isFullHouse" $ do
    it "returns true if the hand is a full house" $ do
      let h = PlayerHand PlayerOne (Card Two Spades) (Card Two Diamonds) (Card Two Clubs) (Card King Spades) (Card King Diamonds)
      isFullHouse h `shouldBe` True

    it "returns false if hand is three of a kind" $ do
      let h = PlayerHand PlayerOne (Card Two Clubs) (Card Two Diamonds) (Card Two Spades) (Card Four Hearts) (Card Six Diamonds)
      isFullHouse h `shouldBe` False

    it "returns false if the hand is not a full house" $ do
      let h = PlayerHand PlayerOne (Card Two Spades) (Card Two Diamonds) (Card Two Clubs) (Card King Spades) (Card Queen Diamonds)
      isFullHouse h `shouldBe` False

  describe "isFourKind" $ do
    it "returns true if the hand is four of a kind" $ do
      let h = PlayerHand PlayerOne (Card Two Clubs) (Card Two Diamonds) (Card Two Spades) (Card Four Hearts) (Card Two Diamonds)
      isFourKind h `shouldBe` True

    it "returns false if the hand is not four of a kind" $ do
      let h = PlayerHand PlayerOne (Card Ten Diamonds) (Card Ten Clubs) (Card Ten Spades) (Card King Diamonds) (Card Ace Diamonds)
      isFourKind h `shouldBe` False

  describe "isStraightFlush" $ do
    it "returns true if the hand is a straight flush" $ do
      let h = PlayerHand PlayerOne (Card Two Spades) (Card Three Spades) (Card Four Spades) (Card Five Spades) (Card Six Spades)
      isStraightFlush h `shouldBe` True

    it "returns false if the hand is not a straight flush" $ do
      let h = PlayerHand PlayerOne (Card Two Spades) (Card Three Spades) (Card Four Spades) (Card Five Spades) (Card Seven Spades)
      isStraightFlush h `shouldBe` False

    it "returns false if the hand is a royal flush" $ do
      let h = PlayerHand PlayerOne (Card Ten Diamonds) (Card Jack Diamonds) (Card Queen Diamonds) (Card King Diamonds) (Card Ace Diamonds)
      isStraightFlush h `shouldBe` False

  describe "isRoyalFlush" $ do
    it "returns false if the hand is a royal flush" $ do
      let h = PlayerHand PlayerOne (Card Two Spades) (Card Three Spades) (Card Four Spades) (Card Five Spades) (Card Six Spades)
      isRoyalFlush h `shouldBe` False

    it "returns true if the hand is a royal flush" $ do
      let h = PlayerHand PlayerOne (Card Ten Diamonds) (Card Jack Diamonds) (Card Queen Diamonds) (Card King Diamonds) (Card Ace Diamonds)
      isRoyalFlush h `shouldBe` True

  describe "playerHandType" $ do
    it "returns the correct hand type" $ do
      let highCard = PlayerHand PlayerOne (Card Two Clubs) (Card Three Diamonds) (Card Four Spades) (Card Five Hearts) (Card Seven Diamonds)
          onePair = PlayerHand PlayerOne (Card Two Clubs) (Card Two Diamonds) (Card Four Spades) (Card Five Hearts) (Card Six Diamonds)
          twoPairs = PlayerHand PlayerOne (Card Two Clubs) (Card Two Diamonds) (Card Four Spades) (Card Four Hearts) (Card Six Diamonds)
          threeKind = PlayerHand PlayerOne (Card Two Clubs) (Card Two Diamonds) (Card Two Spades) (Card Four Hearts) (Card Six Diamonds)
          straight = PlayerHand PlayerOne (Card Nine Spades) (Card Ten Diamonds) (Card Jack Hearts) (Card Queen Diamonds) (Card King Spades)
          flush = PlayerHand PlayerOne (Card Two Spades) (Card Two Spades) (Card Two Spades) (Card Two Spades) (Card Two Spades)
          fullHouse = PlayerHand PlayerOne (Card Two Spades) (Card Two Diamonds) (Card Two Clubs) (Card King Spades) (Card King Diamonds)
          straightFlush = PlayerHand PlayerOne (Card Two Spades) (Card Three Spades) (Card Four Spades) (Card Five Spades) (Card Six Spades)
          royalFlush = PlayerHand PlayerOne (Card Ten Diamonds) (Card Jack Diamonds) (Card Queen Diamonds) (Card King Diamonds) (Card Ace Diamonds)
      playerHandType highCard `shouldBe` HighCard
      playerHandType onePair `shouldBe` OnePair
      playerHandType twoPairs `shouldBe` TwoPairs
      playerHandType threeKind `shouldBe` ThreeOfAKind
      playerHandType straight `shouldBe` Straight
      playerHandType flush `shouldBe` Flush
      playerHandType fullHouse `shouldBe` FullHouse
      playerHandType straightFlush `shouldBe` StraightFlush
      playerHandType royalFlush `shouldBe` RoyalFlush

  describe "parseCard" $ do
    it "returns a card" $ do
      parseCard "5H" `shouldBe` Card Five Hearts
      parseCard "QC" `shouldBe` Card Queen Clubs
      parseCard "3S" `shouldBe` Card Three Spades
      parseCard "JD" `shouldBe` Card Jack Diamonds

  describe "parseHand" $ do
    it "returns a player's hand" $ do
      let s = "8C TS KC 9H 4S"
          expected = PlayerHand PlayerOne (Card Eight Clubs) (Card Ten Spades) (Card King Clubs) (Card Nine Hearts) (Card Four Spades)
      parseHand PlayerOne (words s) `shouldBe` expected

  describe "parseGame" $ do
    it "returns both player's hands" $ do
      let s = "8C TS KC 9H 4S 7D 2S 5D 3S AC"
          p1 = PlayerHand PlayerOne (Card Eight Clubs) (Card Ten Spades) (Card King Clubs) (Card Nine Hearts) (Card Four Spades)
          p2 = PlayerHand PlayerTwo (Card Seven Diamonds) (Card Two Spades) (Card Five Diamonds) (Card Three Spades) (Card Ace Clubs)
      parseGame s `shouldBe` (p1, p2)

  describe "orderedCards" $ do
    it "returns an ordered list of cards" $ do
      let h = PlayerHand PlayerOne (Card Eight Clubs) (Card Two Spades) (Card Eight Spades) (Card Two Diamonds) (Card Eight Hearts)
      (cardValue . head . orderedCards) h `shouldBe` Eight
      (cardValue . head . tail . orderedCards) h `shouldBe` Two

  describe "winner" $ do
    it "returns player two as the winner with a pair of eights" $ do
      let (ph1, ph2) = parseGame "5H 5C 6S 7S KD 2C 3S 8S 8D TD"
      winner ph1 ph2 `shouldBe` PlayerTwo

    it "returns player one as the winner with a high ace" $ do
      let (ph1, ph2) = parseGame "5D 8C 9S JS AC 2C 5C 7D 8S QH"
      winner ph1 ph2 `shouldBe` PlayerOne

    it "returns player two as the winner with a flush" $ do
      let (ph1, ph2) = parseGame "2D 9C AS AH AC 3D 6D 7D TD QD"
      winner ph1 ph2 `shouldBe` PlayerTwo

    it "returns player one as the winner with a pair of queens and high nine" $ do
      let (ph1, ph2) = parseGame "4D 6S 9H QH QC 3D 6D 7H QD QS"
      winner ph1 ph2 `shouldBe` PlayerOne

    it "returns player one as the winner with a full house of fours" $ do
      let (ph1, ph2) = parseGame "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D"
      winner ph1 ph2 `shouldBe` PlayerOne
