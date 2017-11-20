{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.Math where

import           Data.Array ((!), Array, bounds, inRange, listArray)
import qualified Data.Digits as D
import           Data.List (group, intersect, nub, sort, unfoldr)
import           Data.List.Ordered (minus, unionAll)
import           Data.Maybe (listToMaybe)
import           Data.Ratio ((%), Ratio, denominator, numerator)
import           Euler.Data (digits, unDigits)
import           Euler.List ((<:), isEmpty)

sqrtI :: Integral a => a -> a
sqrtI = round . sqrt . fromIntegral

isEven :: Integral a => a -> Bool
isEven = divides 2

isOdd :: Integral a => a -> Bool
isOdd = not . isEven

isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = isEmpty $ filter (flip divides n) [2..sqrtI n]

isPandigital :: Integral a => a -> Bool
isPandigital = isPandigitalTo 9

isPandigitalTo :: Integral a => a -> a -> Bool
isPandigitalTo = isPandigitalFromTo 1

isPandigitalFromTo :: Integral a => a -> a -> a -> Bool
isPandigitalFromTo s e = (== [s..e]) . sort . digits

triangleNumber :: Integral a => a -> a
triangleNumber n = n * (n + 1) `div` 2

triangleNumbers :: Integral a => [a]
triangleNumbers = map triangleNumber [1..]

primes :: [Integer]
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

primesBelow :: Integer -> [Integer]
primesBelow = primesTo . (flip (-) 1)

primesTo :: Integer -> [Integer]
primesTo n = takeWhile (<= n) primes

isPalindrome :: Integral a => a -> Bool
isPalindrome = isPalindromeIn 10

isPalindromeIn :: Integral a => a -> a -> Bool
isPalindromeIn base n = D.digits base n == reverse (D.digits base n)

factorial :: (Enum a, Num a) => a -> a
factorial = product . enumFromTo 1

divides :: Integral a => a -> a -> Bool
divides = ((.) (== 0)) . flip rem

divisibleBy :: Integral a => [a] -> a -> Bool
divisibleBy = (flip (.) (flip divides)) . flip all

factorization :: Integer -> [Integer]
factorization = unfoldr f
  where
    f n = listToMaybe [(x, n `div` x) | x <- [2..n], x `divides` n]

primeFactors :: Integer -> [Integer]
primeFactors = nub . factorization

numDivisors :: Integer -> Int
numDivisors = product . map ((+ 1) . length) . group . factorization

sumDivisors :: Integer -> Integer
sumDivisors n = go n - n
  where
    pow' ls = (head ls, length ls)
    sum' (n,p) = (sum . map (\x -> n ^ x)) [0..p]
    go = product . map (sum' . pow') . group . factorization

isAmicable :: Integer -> Integer -> Bool
isAmicable a b = a < b && d a == b && d b == a
  where
    d n = memoSumDivisors ! n

fibonacci :: Integral a => Int -> a
fibonacci = (!!) fibonaccis

fibonaccis :: Integral a => [a]
fibonaccis = 0 : 1 : zipWith (+) fibonaccis (tail fibonaccis)

truncatables :: Integral a => a -> [a]
truncatables n = n : go' tail n ++ go' init n
  where
    go' f n = go f ((f . digits) n)
    go _ [] = []
    go f ns = unDigits ns : go f (f ns)

isTruncatablePrime :: Integer -> Bool
isTruncatablePrime = all isPrime . truncatables

binomialCoefficient :: Integral a => a -> a
binomialCoefficient n = factorial (2 * n) `div` (factorial n * factorial n)

modMult :: Integral a => a -> a -> a -> a
modMult m a b = (a * b) `mod` m

modProduct :: Integral a => a -> [a] -> a
modProduct m = foldl (modMult m) 1 . map (flip mod m)

modPower :: Integral a => a -> a -> Int -> a
modPower m a b = (modProduct m . take b . repeat) a

modAdd :: Integral a => a -> a -> a -> a
modAdd m a b = (a + b) `mod` m

modSum :: Integral a => a -> [a] -> a
modSum m = foldl (modAdd m) 0 . map (flip mod m)

choose :: Integral a => a -> a -> a
choose n r = factorial n `div` (factorial r * factorial (n - r))

isCoprime :: Integer -> Integer -> Bool
isCoprime a b = case factorization a `intersect` factorization b of
                  [] -> True
                  _  -> False

totient :: Integer -> Integer
totient 1 = 1
totient n =
  let ratio = foldl (\memo x -> memo * (1 - (1 % x))) (n % 1) $ primeFactors n
   in numerator ratio `div` denominator ratio

collatzLength :: Integer -> Integer
collatzLength 1 = 1
collatzLength n
  | inRange (bounds memoCollatzLengths) n' = 1 + memoCollatzLengths ! n'
  | otherwise = 1 + collatzLength n'
  where
    n' = case n of
           1 -> 1
           n | isEven n -> n `div` 2
             | isOdd n -> 3 * n + 1

closestRatio' :: (Integral a) => Ratio a -> a -> Ratio a -> Ratio a
closestRatio' target 2 memo = memo
closestRatio' target q memo =
  let a = numerator target
      b = denominator target
      r = numerator memo
      s = denominator memo
      p = (a * q - 1) `div` b
      n = q - 1
   in if p * s > r * q
         then closestRatio' target n (p % q)
         else closestRatio' target n (r % s)

closestRatio :: Integral a => Ratio a -> Ratio a
closestRatio target = closestRatio' target 1000000 (0 % 1)


--  Stored values for memoization
-------------------------------------------------------------------------------

memoCollatzLengths :: Array Integer Integer
memoCollatzLengths = listArray (1, 1000000) $ map collatzLength [1..1000000]

memoSumDivisors :: Array Integer Integer
memoSumDivisors = listArray (1, 10000) $ map sumDivisors [1..10000]
