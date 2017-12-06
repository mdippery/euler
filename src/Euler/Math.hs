{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

module Euler.Math where

import           Control.Monad (ap, liftM2)
import           Data.Array ((!), Array, bounds, inRange, listArray)
import           Data.List (group, intersect, nub, sort, unfoldr)
import           Data.Maybe (listToMaybe)
import           Data.Ratio ((%), Ratio, denominator, numerator)

import qualified Data.Digits as D
import           Data.List.Ordered (minus, unionAll)

import           Euler.Data (digits, unDigits)
import           Euler.List ((<:), isEmpty)


sqrtI :: Integral a => a -> a
sqrtI = round . sqrt . fromIntegral

cubeRoot :: Integer -> Double
cubeRoot = (** (1 / 3)) . fromIntegral

isCube :: Integer -> Bool
isCube n = round (cubeRoot n) ^ 3 == n

isEven :: Integral a => a -> Bool
isEven = divides 2

isOdd :: Integral a => a -> Bool
isOdd = not . isEven

numLength :: (Integral a, Num b) => a -> b
numLength n = numLength' n
  where
    numLength' n
      | n < 10 = 1
      | otherwise = 1 + numLength' (n `div` 10)

isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = isEmpty . ap (filter . flip divides) (enumFromTo 2 . sqrtI) $ n

isPandigital :: Integral a => a -> Bool
isPandigital = isPandigitalTo 9

isPandigitalTo :: Integral a => a -> a -> Bool
isPandigitalTo = isPandigitalFromTo 1

isPandigitalFromTo :: Integral a => a -> a -> a -> Bool
isPandigitalFromTo s e = (== [s..e]) . sort . digits

floatIsInteger :: RealFrac a => a -> Bool
floatIsInteger n = n == fromIntegral (floor n)

triangleNumbers :: Integral a => [a]
triangleNumbers = map tn [1..]
  where
    tn n = n * (n + 1) `div` 2

isTriangle :: Integer -> Bool
isTriangle n =
  let sqn = sqrt $ 8 * (fromInteger n) + 1
      p = (sqn - 1) / 2
   in floatIsInteger p

pentagonalNumbers :: [Integer]
pentagonalNumbers = map pn [1..]
  where
    pn n = n * (3 * n - 1) `div` 2

pentagonalNumber :: Int -> Integer
pentagonalNumber = ((0 : pentagonalNumbers) !!)

isPentagonal :: Integer -> Bool
isPentagonal n =
  let sqn = sqrt $ 24 * (fromInteger n) + 1
      p = (sqn + 1) / 6
   in floatIsInteger p

hexagonalNumbers :: [Integer]
hexagonalNumbers = map hn [1..]
  where
    hn n = n * (2 * n - 1)

isHexagonal :: Integer -> Bool
isHexagonal n =
  let sqn = sqrt $ 8 * (fromInteger n) + 1
      p = (sqn + 1) / 4
   in floatIsInteger p

primes :: [Integer]
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

primesBelow :: Integer -> [Integer]
primesBelow = primesTo . (flip (-) 1)

primesTo :: Integer -> [Integer]
primesTo = flip takeWhile primes . flip (<=)

isPalindrome :: Integral a => a -> Bool
isPalindrome = isPalindromeIn 10

isPalindromeIn :: Integral a => a -> a -> Bool
isPalindromeIn base n = D.digits base n == reverse (D.digits base n)

factorial :: (Enum a, Num a) => a -> a
factorial = product . enumFromTo 1

divides :: Integral a => a -> a -> Bool
divides = ((== 0) .) . flip rem

divisibleBy :: Integral a => [a] -> a -> Bool
divisibleBy = (. (flip divides)) . flip all

divisibleByAny :: Integral a => [a] -> a -> Bool
divisibleByAny = (. (flip divides)) . flip any

factorization :: Integer -> [Integer]
factorization = unfoldr f
  where
    f n = listToMaybe [(x, n `div` x) | x <- [2..n], x `divides` n]

primeFactors :: Integer -> [Integer]
primeFactors = nub . factorization

numDivisors :: Integer -> Int
numDivisors = product . map ((+ 1) . length) . group . factorization

sumDivisors :: Integer -> Integer
sumDivisors = (-) =<< go
  where
    pow' = liftM2 (,) head length
    sum' = uncurry ((. enumFromTo 0) . (sum .) . map . (^))
    go = product . map (sum' . pow') . group . factorization

isAmicable :: Integer -> Integer -> Bool
isAmicable a b = a < b && d a == b && d b == a
  where
    d = (memoSumDivisors !)

isAbundant :: Integer -> Bool
isAbundant = (>) =<< (memoSumDivisors !)

fibonacci :: Integral a => Int -> a
fibonacci = (fibonaccis !!)

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
modProduct m = foldr (modMult m) 1 . map (`mod` m)

modPower :: Integral a => a -> a -> Int -> a
modPower m a b = (modProduct m . take b . repeat) a

modAdd :: Integral a => a -> a -> a -> a
modAdd m a b = (a + b) `mod` m

modSum :: Integral a => a -> [a] -> a
modSum m = foldr (modAdd m) 0 . map (`mod` m)

choose :: Integral a => a -> a -> a
choose n r = factorial n `div` (factorial r * factorial (n - r))

isCoprime :: Integer -> Integer -> Bool
isCoprime a b = (isEmpty . uncurry intersect) (factorization a, factorization b)

totient :: Integer -> Integer
totient n =
  let ratio = foldr (\x memo -> memo * (1 - (1 % x))) (n % 1) $ primeFactors n
   in (liftM2 div numerator denominator) ratio

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

closestRatio :: Integral a => Ratio a -> Ratio a
closestRatio target = foldr closest (0 % 1) [1000000,999999..2]
  where
    closest q memo =
      let a = numerator target
          b = denominator target
          r = numerator memo
          s = denominator memo
          p = (a * q - 1) `div` b
          n = q - 1
       in if p * s > r * q
             then p % q
             else r % s

rightTriangles :: Integral a => a -> [(a,a,a)]
rightTriangles p = map toT $ nub $ map sort $ filter sqfits $ filter pfits [[a, b a, c a] | a <- [1..u]]
  where
    u = p `div` 3
    b a = (p ^ 2 - 2 * p * a) `div` (2 * p - 2 * a)
    c a = p - b a - a
    pfits = (== p) . sum
    sqfits ns = (ns !! 2) ^ 2 == (ns !! 0) ^ 2 + (ns !! 1) ^ 2
    toT ns = (ns !! 0, ns !! 1, ns !! 2)

isLychrel :: Integral a => a -> Bool
isLychrel n = isLychrel' n 0
  where
    isLychrel' _ 50 = True
    isLychrel' n i =
      let n' = (unDigits . reverse . digits) n
          n'' = n + n'
       in if isPalindrome n'' then False else isLychrel' n'' (i + 1)


--  Stored values for memoization
-------------------------------------------------------------------------------

memoCollatzLengths :: Array Integer Integer
memoCollatzLengths = listArray (1, 1000000) $ map collatzLength [1..1000000]

memoSumDivisors :: Array Integer Integer
memoSumDivisors = listArray (1, 30000) $ map sumDivisors [1..30000]
