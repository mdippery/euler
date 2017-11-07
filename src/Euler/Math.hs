{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.Math where

import qualified Data.Digits as D
import           Data.List.Ordered (minus, unionAll)
import           Euler.Data (digits)
import           Euler.List ((<:), isEmpty)

sqrtI :: Integral a => a -> a
sqrtI = round . sqrt . fromIntegral

isEven :: Integral a => a -> Bool
isEven = divides 2

isOdd :: Integral a => a -> Bool
isOdd = not . isEven

isPrime :: (Integral a) => a -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = isEmpty $ filter (flip divides n) [2..sqrtI n]

triangleNumber :: Integral a => a -> a
triangleNumber n = n * (n + 1) `div` 2

triangleNumbers :: Integral a => [a]
triangleNumbers = map triangleNumber [1..]

primes :: [Integer]
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

primesBelow :: Integer -> [Integer]
primesBelow n = primesTo (n - 1)

primesTo :: Integer -> [Integer]
primesTo n = takeWhile (<= n) primes

isPalindrome :: Integral a => a -> Bool
isPalindrome = isPalindromeIn 10

isPalindromeIn :: Integral a => a -> a -> Bool
isPalindromeIn base n = D.digits base n == reverse (D.digits base n)

factorial :: (Enum a, Num a) => a -> a
factorial = product . enumFromTo 1

divides :: Integral a => a -> a -> Bool
divides a b = b `rem` a == 0

divisors :: Integral a => a -> [a]
divisors n = 1 : filter (flip divides n) [2..n `div` 2] <: n

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
collatz n = n : (collatz . nextN) n
  where
    nextN n
      | isEven n = n `div` 2
      | isOdd n = 3 * n + 1
