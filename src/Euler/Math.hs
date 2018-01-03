{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Math
  Description : Solutions for math equations
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Helps solve various math-heavy problems.
-}
module Euler.Math
  (
    -- * Basic properties
    isAbundant
  , isAbundantSum
  , isAmicable
  , isComposite
  , isCoprime
  , isCube
  , isEven
  , isHexagonal
  , isInteger
  , isLychrel
  , isOdd
  , isOtherGoldbach
  , isPalindrome
  , isPalindromeIn
  , isPandigital
  , isPandigitalFromTo
  , isPandigitalTo
  , isPentagonal
  , isPowerOf
  , isPrime
  , isSquare
  , isTriangle
  , isTruncatablePrime

    -- * Operations and calculations
  , binomialCoefficient
  , closestRatio
  , collatzLength
  , choose
  , cycleLength
  , divides
  , divisibleBy
  , divisibleByAny
  , factorial
  , factorization
  , maximumPandigital
  , modAdd
  , modMult
  , modPower
  , modProduct
  , modSum
  , multiplicands
  , numDivisors
  , numLength
  , rightTriangles
  , sumDivisors
  , totient
  , truncateN

    -- * Roots and powers
  , cubeRoot
  , nthRoot
  , sqrtI

    -- * Groups, classes, and sequences
  , abundantNumbers
  , composites
  , compositesTo
  , fibonacci
  , fibonaccis
  , hexagonalNumbers
  , numbersOfLength
  , pentagonalNumber
  , pentagonalNumbers
  , primes
  , primesBelow
  , primesTo
  , triangleNumbers
  ) where

import           Control.Monad (ap, liftM2)
import           Data.Array ((!), Array, bounds, inRange, listArray)
import           Data.List (elemIndex, group, intersect, nub, sort, unfoldr)
import           Data.Maybe (fromJust, listToMaybe)
import           Data.Ratio ((%), Ratio, denominator, numerator)

import qualified Data.Digits as D
import           Data.List.Ordered (minus, unionAll)

import           Euler.Data (digits, unDigits)
import           Euler.List ((<:), isEmpty)
import           Euler.Tuple (sortT, zipT)


-- | True if a number is even.
isEven :: Integral a => a -> Bool
isEven = divides 2

-- | True if a number is odd.
isOdd :: Integral a => a -> Bool
isOdd = not . isEven

-- | Calculates the square root of a number, returning it as an integer.
--
-- If the number is not the square of another number, the result will be
-- rounded to the nearest integer, as per the 'round' function.
sqrtI :: Integral a => a -> a
sqrtI = round . sqrt . fromIntegral

-- | Calculates the cube root of a number.
cubeRoot :: Integer -> Double
cubeRoot = nthRoot 3

-- | True if a number is the square of some number.
isSquare :: Integer -> Bool
isSquare = isPowerOf 2

-- | True if a number is the cube of some number.
isCube :: Integer -> Bool
isCube = isPowerOf 3

-- | Calculates the /nth/ root of a number.
nthRoot :: Integer  -- ^ /nth/ root to calculate
        -> Integer  -- ^ Number
        -> Double   -- ^ /nth/ root of the number
nthRoot r = (** (1 / (fromIntegral r))) . fromIntegral

-- | True if a number is the /nth/ power of another number.
isPowerOf :: Integer  -- ^ /nth/ power
          -> Integer  -- ^ Number
          -> Bool     -- ^ True if the number is the /nth/ power of some number
isPowerOf p n = round (nthRoot p n) ^ p == n

-- | Number of digits in a given number in base 10.
numLength :: (Integral a)
          => a    -- ^ Number
          -> Int  -- ^ Number of digits in the number
numLength = length . digits

-- | All numbers with the given number of digits in base 10.
numbersOfLength :: (Num b, Enum b, Integral a)
                => a    -- ^ Number of digits
                -> [b]  -- ^ All integers with the given number of digits in base 10
numbersOfLength n = enumFromTo (10 ^ (n - 1)) (10 ^ n - 1)

-- | Returns the length of the recurring cycle of a fraction.
--
-- If the fraction does not have a recurring cycle, 0 is returned.
--
-- ==== Examples
--
-- > cycleLength (1 % 2) == 0
-- > cycleLength (1 % 7) == 6
cycleLength :: Integral a => Ratio a -> Int
cycleLength x = go [] (numerator x) (denominator x)
  where
    go rs n d
      | r == 0 = 0
      | r `elem` rs = 1 + (fromJust $ elemIndex r rs)
      | otherwise = go (r : rs) (r * 10) d
      where
        r = n `rem` d

-- | True if the number is prime.
isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = isEmpty . ap (filter . flip divides) (enumFromTo 2 . sqrtI) $ n

-- | True if the number is a composite number.
isComposite :: Integral a => a -> Bool
isComposite = not . isPrime

-- | Infinite list of all prime numbers.
primes :: [Integer]
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

-- | All prime numbers less than the given number.
primesBelow :: Integer    -- ^ Upper bound, exclusive
            -> [Integer]  -- ^ All prime numbers less than the upper bound
primesBelow = primesTo . (flip (-) 1)

-- | All prime numbers less than or equal to the given number.
primesTo :: Integer     -- ^ Upper bound, inclusive
         -> [Integer]   -- ^ All prime numbers less than or equal to the upper bound
primesTo = flip takeWhile primes . flip (<=)

-- | Infinite list of all composite numbers.
composites :: [Integer]
composites = (filter isComposite . enumFrom) 4

-- | All composite numbers less than or equal to the given number.
compositesTo :: Integral a
             => a     -- ^ Upper bound, inclusive
             -> [a]   -- ^ All composite numbers less than or equal to the upper bound
compositesTo = filter isComposite . enumFromTo 4

-- | True if /n/ is an odd composite number that can be expressed as the
-- sum of a prime and twice a square.
--
-- The results of this function are further elaborated upon in
-- <https://projecteuler.net/problem=46 Euler Problem #46>.
--
-- ==== Examples
--
-- > isOtherGoldbach 9 == True
-- > isOtherGoldbach 23 == False
-- > (head . filter (not . isOtherGoldbach)) composites == 5777
isOtherGoldbach :: Integer -> Bool
isOtherGoldbach n = isEven n || isPrime n || (not . isEmpty) (go n)
  where
    go n = [x | x <- primesTo n, isSquare ((n - x) `div` 2)]

-- | True if the number contains all the digits from 1 to 9, in some order.
--
-- This is equivalent to @isPandigital 9@.
isPandigital :: Integral a => a -> Bool
isPandigital = isPandigitalTo 9

-- | True if the number contains all the digits from 1 to /n/, in some order.
--
-- This is equivalent to @isPandigitalFromTo 1@.
isPandigitalTo :: Integral a
               => a     -- ^ Upper limit of range
               -> a     -- ^ Number
               -> Bool  -- True if the number contains all the digits from 1 to /n/, in some order
isPandigitalTo = isPandigitalFromTo 1

-- | True if the number contains all the digits from /m/ to /n/, in some order.
isPandigitalFromTo :: Integral a
                   => a     -- ^ Lower bound of digit range
                   -> a     -- ^ Upper bound of digit range
                   -> a     -- ^ Number
                   -> Bool  -- ^ True if the number contains all the digits from /m/ to /n/, in some order.
isPandigitalFromTo s e = (== [s..e]) . sort . digits

-- | Calculates the largest /n/-digit pandigital that can be formed by the
-- given number.
--
-- A number is said to be /1 to n pandigital/ if it makes use of all digits
-- from 1 to /n/ exactly once. For example, 1234 is a 4-digit pandigital. 134
-- is not pandigital at all.
--
-- Returns @Nothing@ if the given number is not a pandigital; otherwise,
-- returns @Just n@ where /n/ is the maximum /n/-digit pandigital formed
-- by the number.
maximumPandigital :: Integral a => a -> Maybe Int
maximumPandigital n =
  case elemIndex True (map (flip isPandigitalTo n) [9,8..1]) of
    Nothing -> Nothing
    Just i -> Just (9 - i)

-- | True if the decimal number is a whole number.
--
-- ==== Examples
--
-- > isInteger 10.0 == True
-- > isInteger 10.1 == False
isInteger :: RealFrac a => a -> Bool
isInteger = ap (==) (fromIntegral . floor)

-- | Infinite list of all <https://en.wikipedia.org/wiki/Triangular_number triangle numbers>.
triangleNumbers :: Integral a => [a]
triangleNumbers = map tn [1..]
  where
    tn n = n * (n + 1) `div` 2

-- | True if the given number is a <https://en.wikipedia.org/wiki/Triangular_number triangle number>.
isTriangle :: Integer -> Bool
isTriangle n =
  let sqn = sqrt $ 8 * (fromInteger n) + 1
      p = (sqn - 1) / 2
   in isInteger p

-- | Infinite list of all <https://en.wikipedia.org/wiki/Pentagonal_number pentagonal numbers>.
pentagonalNumbers :: [Integer]
pentagonalNumbers = map pn [1..]
  where
    pn n = n * (3 * n - 1) `div` 2

-- | /nth/ <https://en.wikipedia.org/wiki/Pentagonal_number pentagonal number>.
pentagonalNumber :: Int -> Integer
pentagonalNumber = ((0 : pentagonalNumbers) !!)

-- | True if a number is a <https://en.wikipedia.org/wiki/Pentagonal_number pentagonal number>.
isPentagonal :: Integer -> Bool
isPentagonal n =
  let sqn = sqrt $ 24 * (fromInteger n) + 1
      p = (sqn + 1) / 6
   in isInteger p

-- | Infinite list of all <https://en.wikipedia.org/wiki/Hexagonal_number hexagonal numbers>.
hexagonalNumbers :: [Integer]
hexagonalNumbers = map hn [1..]
  where
    hn n = n * (2 * n - 1)

-- | True if the given number is a <https://en.wikipedia.org/wiki/Hexagonal_number hexagonal number>.
isHexagonal :: Integer -> Bool
isHexagonal n =
  let sqn = sqrt $ 8 * (fromInteger n) + 1
      p = (sqn + 1) / 4
   in isInteger p

-- | True if the number is <https://en.wikipedia.org/wiki/Palindromic_number palindromic>
-- in base 10.
isPalindrome :: Integral a => a -> Bool
isPalindrome = isPalindromeIn 10

-- | True if a number is palindromic in the given base.
isPalindromeIn :: Integral a
               => a     -- ^ Base
               -> a     -- ^ Number
               -> Bool  -- ^ True if the number is palindromic in the given base
isPalindromeIn base n = D.digits base n == reverse (D.digits base n)

-- | Factorial of /n/.
factorial :: (Enum a, Num a) => a -> a
factorial = product . enumFromTo 1

-- | True if /a/ divides /b/, that is, b divided by a yields no remainder.
--
-- This is best used as an infix operator.
--
-- ==== Examples
--
-- > 10 `divides` 100 == True
-- > 9 `divides` 100 == False
divides :: Integral a
        => a      -- ^ /a/
        -> a      -- ^ /b/
        -> Bool   -- ^ True if /a/ divides /b/
divides = ((== 0) .) . flip rem

-- | True if a number is divisible by /all/ the numbers in a list.
--
-- ==== Examples
--
-- > divisibleBy [3,8] 24 == True
divisibleBy :: Integral a
            => [a]    -- ^ List of numbers that the number in question /must/ be divisible by
            -> a      -- ^ Number
            -> Bool   -- ^ True if a number is divisible by the entire list of numbers
divisibleBy = (. (flip divides)) . flip all

-- | True if a number is divisible by /any/ of the numbers in a list.
--
-- ==== Examples
--
-- > divisibleByAny [7,8] 24 == True
-- > divisibleByAny [9,16] 24 == False
divisibleByAny :: Integral a
               => [a]   -- ^ List of possible divisors
               -> a     -- ^ Number
               -> Bool  -- ^ True if a number is divisible by any number in the list
divisibleByAny = (. (flip divides)) . flip any

-- | List of all factors of the given number
factorization :: Integer -> [Integer]
factorization = unfoldr f
  where
    f n = listToMaybe [(x, n `div` x) | x <- [2..n], x `divides` n]

-- | Calculates pairs of numbers that can be multiplied together to produce
-- the given number.
multiplicands :: Integral a => a -> [(a, a)]
multiplicands n = nub $ map sortT $ zipT (div n) $ filter (flip divides n) $ enumFromTo 1 n

-- | List of all the prime factors of a given number
primeFactors :: Integer -> [Integer]
primeFactors = nub . factorization

-- | Number of divisors of a given number.
numDivisors :: Integer -> Int
numDivisors = product . map ((+ 1) . length) . group . factorization

-- | Sum of the divisors of a given number
sumDivisors :: Integer -> Integer
sumDivisors = (-) =<< go
  where
    pow' = liftM2 (,) head length
    sum' = uncurry ((. enumFromTo 0) . (sum .) . map . (^))
    go = product . map (sum' . pow') . group . factorization

-- | True if a pair of numbers are <https://en.wikipedia.org/wiki/Amicable_numbers amicable>.
--
-- Two numbers are amicable if the sum of the divisors of one number is equal
-- to the other number and vice-versa.
isAmicable :: Integer -> Integer -> Bool
isAmicable a b = a < b && d a == b && d b == a
  where
    d = (memoSumDivisors !)

-- | True if the sum of the divisors of a number is greater than the number itself.
isAbundant :: Integer -> Bool
isAbundant = (>) =<< (memoSumDivisors !)

-- | List of all abundant numbers
abundantNumbers :: [Integer]
abundantNumbers = filter (memoAbundantNumbers !) [1..28123]

-- | True if the number is the sum of two abundant numbers
isAbundantSum :: Integer -> Bool
isAbundantSum n = any (\x -> isAbundant (n - x)) $ takeWhile (< n) abundantNumbers

-- | The /nth/ Fibonacci number
fibonacci :: Integral a => Int -> a
fibonacci = (fibonaccis !!)

-- | Infinite list of Fibonacci numbers
fibonaccis :: Integral a => [a]
fibonaccis = 0 : 1 : zipWith (+) fibonaccis (tail fibonaccis)

-- | List of all the truncated numbers associated with the given number.
--
-- ==== Examples
--
-- > truncateN 3797 == [3797, 797, 97, 7, 379, 37, 3]
--
-- That is, the returned list consists of the original number, plus all
-- the numbers formed by successively removing digits from both ends.
truncateN :: Integral a => a -> [a]
truncateN n = n : go' tail n ++ go' init n
  where
    go' f n = go f ((f . digits) n)
    go _ [] = []
    go f ns = unDigits ns : go f (f ns)

-- | True if a number, along with all its associated truncated numbers, are prime.
isTruncatablePrime :: Integer -> Bool
isTruncatablePrime = all isPrime . truncateN

-- | <https://en.wikipedia.org/wiki/Binomial_coefficient Binomial coefficient> of /n/.
binomialCoefficient :: Integral a => a -> a
binomialCoefficient n = factorial (2 * n) `div` (factorial n * factorial n)

-- | Multiplies two numbers, modulo some other number.
modMult :: Integral a
        => a  -- ^ Modulus
        -> a  -- ^ First operand
        -> a  -- ^ Second operand
        -> a  -- ^ /(a * b) % m/
modMult m a b = (a * b) `mod` m

-- | A 'product' function that uses modular multiplication.
modProduct :: Integral a => a -> [a] -> a
modProduct m = foldr (modMult m) 1 . map (`mod` m)

-- | Raises a number to a given power, modulo another number.
modPower :: Integral a
         => a     -- ^ Modulus
         -> a     -- ^ Base number
         -> Int   -- ^ Power
         -> a     -- ^ /bth/ power of /a/, modulo /m/
modPower m a b = (modProduct m . take b . repeat) a

-- | Adds two numbers, modulo some other number
modAdd :: Integral a
       => a   -- ^ Modulus
       -> a   -- ^ First operand
       -> a   -- ^ Second operand
       -> a   -- ^ /(a + b) % m/
modAdd m a b = (a + b) `mod` m

-- | A 'sum' function that uses modular addition.
modSum :: Integral a => a -> [a] -> a
modSum m = foldr (modAdd m) 0 . map (`mod` m)

-- | <https://en.wikipedia.org/wiki/Combination Combination> function.
--
-- This is best used as an infix operator.
--
-- ==== Examples
--
-- > 10 `choose` 5 == 252
choose :: Integral a
       => a   -- ^ /n/
       -> a   -- ^ /k/
       -> a   -- ^ Number of /k/ distinct elements in a set of size /n/
choose n r = factorial n `div` (factorial r * factorial (n - r))

-- | True if two integers are coprime or /relatively prime/ to each other.
--
-- Two integers are <https://en.wikipedia.org/wiki/Coprime_integers coprime>
-- if their greatest common divisor is 1.
isCoprime :: Integer -> Integer -> Bool
isCoprime a b = (isEmpty . uncurry intersect) (factorization a, factorization b)

-- | <https://en.wikipedia.org/wiki/Euler's_totient_function Euler's totient function>.
--
-- The totient function counts the number of positive integers up to /n/ that
-- are relatively prime to /n/.
totient :: Integer  -- ^ /n/
        -> Integer  -- ^ Number of integers from 1 to /n/ that are relatively prime to /n/
totient n =
  let ratio = foldr (\x memo -> memo * (1 - (1 % x))) (n % 1) $ primeFactors n
   in (liftM2 div numerator denominator) ratio

-- | Counts the number of steps to get from an arbitrary integer to 1 by
-- the process described in the <https://en.wikipedia.org/wiki/Collatz_conjecture Collatz conjecture>.
collatzLength :: Integer  -- ^ Starting number
              -> Integer  -- ^ Number of steps to go from the starting number to 1
collatzLength 1 = 1
collatzLength n
  | inRange (bounds memoCollatzLengths) n' = 1 + memoCollatzLengths ! n'
  | otherwise = 1 + collatzLength n'
  where
    n' = case n of
           1 -> 1
           n | isEven n -> n `div` 2
             | isOdd n -> 3 * n + 1

-- | Finds the ratio closest to and less than the given ratio.
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

-- | Generates triplets for a right triangle of the given perimeter.
rightTriangles :: Integral a
               => a           -- ^ Perimeter
               -> [(a,a,a)]   -- ^ Solutions for right triangles of the given perimeter
rightTriangles p = map toT $ nub $ map sort $ filter sqfits $ filter pfits [[a, b a, c a] | a <- [1..u]]
  where
    u = p `div` 3
    b a = (p ^ 2 - 2 * p * a) `div` (2 * p - 2 * a)
    c a = p - b a - a
    pfits = (== p) . sum
    sqfits ns = (ns !! 2) ^ 2 == (ns !! 0) ^ 2 + (ns !! 1) ^ 2
    toT ns = (ns !! 0, ns !! 1, ns !! 2)

-- | True if a number is a /Lychrel number/, i.e., if you reverse its digits
-- and add them together, and keep applying that operation, you will never
-- reach a palindromic number.
--
-- Lychrel numbers are theoretical. 'isLychrel' assumes that a number will
-- either produce a palindrome or prove to be Lychrel in 50 iterations or
-- less, as described in <https://projecteuler.net/problem=55 Euler Problem #55>.
isLychrel :: Integral a => a -> Bool
isLychrel = isLychrel' 0
  where
    isLychrel' 50 _ = True
    isLychrel' i n =
      let n' = (unDigits . reverse . digits) n
          n'' = n + n'
       in if isPalindrome n'' then False else isLychrel' (i + 1) n''


--  Stored values for memoization
-------------------------------------------------------------------------------

memoCollatzLengths :: Array Integer Integer
memoCollatzLengths = listArray (1, 1000000) $ map collatzLength [1..1000000]

memoSumDivisors :: Array Integer Integer
memoSumDivisors = listArray (1, 30000) $ map sumDivisors [1..30000]

memoAbundantNumbers :: Array Integer Bool
memoAbundantNumbers = listArray (1, 28123) $ map isAbundant [1..28123]
