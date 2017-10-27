module Euler.Math where

import qualified Data.Digits as D
import           Euler.Data (digits)

isEven :: Integral a => a -> Bool
isEven n = n `rem` 2 == 0

isOdd :: Integral a => a -> Bool
isOdd = not . isEven

isPalindrome :: Integral a => a -> Bool
isPalindrome = isPalindromeIn 10

isPalindromeIn :: Integral a => a -> a -> Bool
isPalindromeIn base n = D.digits base n == reverse (D.digits base n)

factorial :: (Enum a, Num a) => a -> a
factorial = product . (enumFromTo 1)

divides :: Integral a => a -> a -> Bool
divides a b = b `rem` a == 0

divisibleBy :: Integral a => [a] -> a -> Bool
divisibleBy ns n = all (flip divides n) ns

fibonacci :: Integral a => a -> a
fibonacci n =
  let s   = sqrt 5
      si  = 1 / s
      tp  = 1 + s
      tn  = 1 - s
      tp' = tp / 2
      tn' = tn / 2
   in round $ (si * (tp' ^ n)) - (si * (tn' ^ n))

binomialCoefficient :: Integral a => a -> a
binomialCoefficient n = factorial (2 * n) `div` (factorial n * factorial n)

numDigits :: Integral a => a -> Int
numDigits = length . digits

modMult :: Integral a => a -> a -> a -> a
modMult m a b = (a * b) `mod` m

modProduct :: Integral a => a -> [a] -> a
modProduct m ns = foldl (modMult m) 1 (map (flip mod m) ns)

modPower :: Integral a => a -> a -> a -> a
modPower m a b = modProduct m $ take (fromIntegral b) $ repeat a

modAdd :: Integral a => a -> a -> a -> a
modAdd m a b = (a + b) `mod` m

modSum :: Integral a => a -> [a] -> a
modSum m ns = foldl (modAdd m) 0 (map (flip mod m) ns)

choose :: Integral a => a -> a -> a
choose n r = factorial n `div` (factorial r * factorial (n - r))

collatz :: Integral a => a -> [a]
collatz 1 = [1]
collatz n
  | isEven n = n : collatz (n `div` 2)
  | isOdd n  = n : collatz (3 * n + 1)

mapDigits :: Integral b => (b -> a) -> b -> [a]
mapDigits f = (map f) . digits

sumDigits :: Integral b => (a -> [b]) -> a -> b
sumDigits = (.) sum

digitPowers :: Integral a => a -> a -> [a]
digitPowers exp = mapDigits (^ exp)

sumOfPowers :: Integral a => a -> a -> a
sumOfPowers exp = sumDigits (digitPowers exp)

digitFactorials :: Integral a => a -> [a]
digitFactorials = mapDigits factorial

sumOfFactorials :: Integral a => a -> a
sumOfFactorials = sumDigits digitFactorials
