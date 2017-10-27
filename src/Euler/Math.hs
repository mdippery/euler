module Euler.Math where

import qualified Data.Digits as D
import           Euler.Data (digits)

isEven :: Integer -> Bool
isEven n = n `rem` 2 == 0

isOdd :: Integer -> Bool
isOdd = not . isEven

isPalindrome :: Integer -> Bool
isPalindrome = isPalindromeIn 10

isPalindromeIn :: Integer -> Integer -> Bool
isPalindromeIn base n = D.digits base n == reverse (D.digits base n)

factorial :: Integer -> Integer
factorial = product . (enumFromTo 1)

divides :: Integer -> Integer -> Bool
divides a b = b `rem` a == 0

divisibleBy :: [Integer] -> Integer -> Bool
divisibleBy ns n = all (flip divides n) ns

fibonacci :: Integer -> Integer
fibonacci n =
  let s   = sqrt 5
      si  = 1 / s
      tp  = 1 + s
      tn  = 1 - s
      tp' = tp / 2
      tn' = tn / 2
   in round $ (si * (tp' ^ n)) - (si * (tn' ^ n))

binomialCoefficient :: Integer -> Integer
binomialCoefficient n = factorial (2 * n) `div` (factorial n * factorial n)

numDigits :: Integer -> Int
numDigits = length . digits

modMult :: Integer -> Integer -> Integer -> Integer
modMult m a b = (a * b) `mod` m

modProduct :: Integer -> [Integer] -> Integer
modProduct m ns = foldl (modMult m) 1 (map (flip mod m) ns)

modPower :: Integer -> Integer -> Integer -> Integer
modPower m a b = modProduct m $ take (fromIntegral b) $ repeat a

modAdd :: Integer -> Integer -> Integer -> Integer
modAdd m a b = (a + b) `mod` m

modSum :: Integer -> [Integer] -> Integer
modSum m ns = foldl (modAdd m) 0 (map (flip mod m) ns)

choose :: Integer -> Integer -> Integer
choose n r = factorial n `div` (factorial r * factorial (n - r))

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n
  | isEven n = n : collatz (n `div` 2)
  | isOdd n  = n : collatz (3 * n + 1)

mapDigits :: (Integer -> a) -> Integer -> [a]
mapDigits f = (map f) . digits

sumDigits :: (a -> [Integer]) -> a -> Integer
sumDigits = (.) sum

digitPowers :: Integral a => a -> Integer -> [Integer]
digitPowers exp = mapDigits (^ exp)

sumOfPowers :: Integral a => a -> Integer -> Integer
sumOfPowers exp = sumDigits (digitPowers exp)

digitFactorials :: Integer -> [Integer]
digitFactorials = mapDigits factorial

sumOfFactorials :: Integer -> Integer
sumOfFactorials = sumDigits digitFactorials
