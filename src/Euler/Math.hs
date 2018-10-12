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
  , isInteger
  , isLychrel
  , isOdd
  , isOtherGoldbach
  , isPalindrome
  , isPalindromeIn
  , isPandigital
  , isPandigitalFromTo
  , isPandigitalTo
  , isPandigitalN
  , isPowerOf
  , isPrime
  , isProductPandigital
  , isSquare
  , isTruncatablePrime

    -- * Operations and calculations
  , between
  , bigOmega
  , binomialCoefficient
  , closestRatio
  , collatzLength
  , choose
  , cycleLength
  , digitFactorial
  , digitsIn
  , divides
  , divisibleBy
  , extract1
  , extract3
  , factorial
  , factorialChain
  , factorization
  , littleOmega
  , maximumPandigital
  , modAdd
  , modMult
  , modPower
  , modProduct
  , modSum
  , multiplicands
  , numDivisors
  , primeFactors
  , reverseNum
  , reverseNumIn
  , sumDigitFactorial
  , sumDivisors
  , totient
  , totientRatio
  , truncateN

    -- ** Quadratic equations
  , consecutiveQuadraticPrimes
  , isPrimeQuadratic
  , quadratic

    -- * Roots and powers
  , cubeRoot
  , nthRoot
  , sqrtI

    -- * Groups, classes, and sequences
  , abundantNumbers
  , fibonacci
  , fibonaccis
  , numbersOfLength
  , palindromes

    -- ** Primes and composites
  , composites
  , compositesTo
  , nextPrime
  , primes
  , primesBelow
  , primesTo

    -- ** Figurate numbers

    -- *** Identifying
  , isTriangle
  , isPentagonal
  , isHexagonal
  , isHeptagonal
  , isOctagonal

    -- *** Retrieving
  , pentagonalNumber

    -- *** Listing
  , squareNumbers
  , triangleNumbers
  , heptagonalNumbers
  , hexagonalNumbers
  , pentagonalNumbers
  , octagonalNumbers
  ) where

import Control.Monad (ap, liftM2)
import Data.Array ((!), Array, bounds, inRange, listArray)
import Data.Foldable (null)
import Data.List (elemIndex, foldl', genericIndex, genericLength, genericTake, group, intersect, isPrefixOf, nub, permutations, sort, unfoldr)
import Data.Maybe (fromJust, listToMaybe)
import Data.Ratio ((%), Ratio, denominator, numerator)

import Data.List.Ordered (minus, unionAll)

import Euler.Data (allDigits, digits, unDigits)
import Euler.List ((<:), longestPrefix)
import Euler.Text (toInt)
import Euler.Tuple (sortT)


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

-- | All numbers with the given number of digits in base 10.
numbersOfLength :: (Integral a, Num b, Enum b)
                => a    -- ^ Number of digits
                -> [b]  -- ^ All integers with the given number of digits in base 10
numbersOfLength n = enumFromTo (10 ^ (n - 1)) (10 ^ n - 1)

-- | List of all 0-9 palindromic numbers.
palindromes :: [Int]
palindromes = filter (>= 1000000000) $ map toInt $ permutations "0123456789"

-- | Returns the length of the recurring cycle of a fraction.
--
-- If the fraction does not have a recurring cycle, 0 is returned.
--
-- ==== Examples
--
-- >>> cycleLength (1 % 2)
-- 0
-- >>> cycleLength (1 % 7)
-- 6
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
isPrime n
  | n < 2  = False
  | n == 2 = True
  | n > 2  = null $ filter (flip divides n) [2..sqrtI n]

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

-- | First prime number greater than /n/.
nextPrime :: Integer -> Integer
nextPrime n = (head . dropWhile (<= n)) primes

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
-- >>> isOtherGoldbach 9
-- True
-- >>> isOtherGoldbach 23
-- False
-- >>> (head . filter (not . isOtherGoldbach)) composites
-- 5777
isOtherGoldbach :: Integer -> Bool
isOtherGoldbach n = isEven n || isPrime n || (not . null) (go n)
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
isPandigitalFromTo s e = (== [s..e]) . allDigits

-- | True if the number is some n-digit pandigital number.
isPandigitalN :: Integral a => a -> Bool
isPandigitalN = flip isPrefixOf [1..9] . allDigits

-- | True if /m/, /n/, and the product of /m/ and /n/ are pandigital when
-- all three numbers are concatenated together.
isProductPandigital :: Integral a => a -> a -> Bool
isProductPandigital m n =
  let p    = m * n
      m'   = digits m
      n'   = digits n
      p'   = digits p
      p''  = m' ++ n' ++ p'
      p''' = sort p''
   in p''' == [1..9]

-- | Calculates the largest /n/-digit pandigital that can be formed by the
-- given number. A number is said to be /1 to n pandigital/ if it makes use of
-- all digits from 1 to /n/ exactly once.
--
-- Returns @Nothing@ if the given number is not a pandigital; otherwise,
-- returns @Just n@ where /n/ is the maximum /n/-digit pandigital formed
-- by the number.
--
-- ==== Examples
--
-- >>> maximumPandigital 1234
-- Just 4
-- >>> maximumPandigital 134
-- Nothing
maximumPandigital :: Integral a => a -> Maybe Int
maximumPandigital n =
  let prefix = longestPrefix (allDigits n) [1..9]
   in case length prefix of
        0 -> Nothing
        x -> Just x

-- | True if the decimal number is a whole number.
--
-- ==== Examples
--
-- >>> isInteger 10.0
-- True
-- >>> isInteger 10.1
-- False
isInteger :: RealFrac a => a -> Bool
isInteger = ap (==) (fromIntegral . floor)

-- | Infinite list of square numbers.
squareNumbers :: [Integer]
squareNumbers = map (^ 2) [1..]

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
pentagonalNumber :: Integral a => a -> Integer
pentagonalNumber = genericIndex (0 : pentagonalNumbers)

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

-- | Infinite list of <https://en.wikipedia.org/wiki/Heptagonal_number heptagonal numbers>.
heptagonalNumbers :: [Integer]
heptagonalNumbers = map hn [1..]
  where
    hn n = ((5 * n ^ 2) - (3 * n)) `div` 2

-- | True if the given number is a <https://en.wikipedia.org/wiki/Heptagonal_number heptagonal number>.
isHeptagonal :: Integer -> Bool
isHeptagonal n =
  let sqn = sqrt $ 40 * (fromInteger n) + 9
      p = (sqn + 3) / 10
   in isInteger p

-- | Infinite list of <https://en.wikipedia.org/wiki/Octagonal_number octagonal numbers>.
octagonalNumbers :: [Integer]
octagonalNumbers = map hn [1..]
  where
    hn n = 3 * n ^ 2 - 2 * n

-- | True if the given number is an <https://en.wikipedia.org/wiki/Octagonal_number octagonal number>.
isOctagonal :: Integer -> Bool
isOctagonal n =
  let sqn = sqrt $ 3 * (fromInteger n) + 1
      p = (sqn + 1) / 3
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
isPalindromeIn = ap (==) . reverseNumIn

-- | Returns the reversed representation of the number.
--
-- ==== Examples
--
-- >>> reverseNum 1234
-- 4321
reverseNum :: Integral a => a -> a
reverseNum = reverseNumIn 10

-- | Returns the reversed representation of the number in the given base.
--
-- ==== Examples
--
-- >>> reverseNumIn 10 1234
-- 4321
reverseNumIn :: Integral a
             => a   -- ^ Base
             -> a   -- ^ /n/
             -> a   -- ^ Reversed representation of /n/ in the given base
reverseNumIn base n = go 0 n
  where
    go acc n'
      | n' < base = base * acc + n'
      | otherwise =
        let (q,r) = n' `quotRem` base
         in go (acc * base + r) q

-- | Factorial of /n/.
factorial :: (Enum a, Num a) => a -> a
factorial = product . enumFromTo 1

-- | Calculates the factorial of each digit of /n/.
--
-- ==== Examples
--
-- >>> digitFactorial 169
-- [1,720,362880]
digitFactorial :: Integer -> [Integer]
digitFactorial = map (memoFactorials !) . digits

-- | Sum of the factorial of each digit of /n/.
--
-- Essentially this is @sum . 'digitFactorial'@.
--
-- ==== Examples
--
-- >>> sumDigitFactorial 169
-- 363301
sumDigitFactorial :: Integer -> Integer
sumDigitFactorial = sum . digitFactorial

-- | Calculates the chain of numbers such that, when 'sumDigitFactorial' is
-- applied to each subsequent link in the chain, the chain begins to repeat
-- again.
--
-- It is known that /every/ starting number will eventually get stuck in a
-- loop. The chain returned by this function will contain all the unique
-- elements of that sequence, in order, before it begins to loop again.
factorialChain :: Integer -> [Integer]
factorialChain = go []
  where
    go memo n
      | n `elem` memo = reverse memo
      | otherwise = go (n : memo) (sdf n)
    sdf n
      | inRange (bounds memoSumDigitFactorials) n = memoSumDigitFactorials ! n
      | otherwise = sumDigitFactorial n

-- | True if /a/ divides /b/, that is, /b/ divided by /a/ yields no remainder.
--
-- This is best used as an infix operator.
--
-- ==== Examples
--
-- >>> 10 `divides` 100
-- True
-- >>> 9 `divides` 100
-- False
divides :: Integral a
        => a      -- ^ /a/
        -> a      -- ^ /b/
        -> Bool   -- ^ True if /a/ divides /b/
divides = ((== 0) .) . flip rem

-- | True if a number is divisible by /all/ the numbers in a list.
--
-- ==== Examples
--
-- >>> divisibleBy [3,8] 24
-- True
divisibleBy :: Integral a
            => [a]    -- ^ List of numbers that the number in question /must/ be divisible by
            -> a      -- ^ Number
            -> Bool   -- ^ True if a number is divisible by the entire list of numbers
divisibleBy = (. (flip divides)) . flip all

-- | List of all factors of the given number.
--
-- The result may include duplicates, since it includes /all/ the prime
-- factors that can be multiplied together to get the original number. To
-- get only the unique prime factors, use 'primeFactors' instead.
--
-- ==== Examples
--
-- >>> factorization 28
-- [2,2,7]
-- >>> (product . factorization) 28
-- 28
factorization :: Integer -> [Integer]
factorization = unfoldr f
  where
    f n = listToMaybe [(x, n `div` x) | x <- [2..n], x `divides` n]

-- | List of all the prime factors of a given number.
--
-- These are the /unique/ prime factors of the given number. For the
-- full factorization, use 'factorization' instead.
--
-- ==== Examples
--
-- >>> primeFactors 28
-- [2,7]
primeFactors :: Integer -> [Integer]
primeFactors = nub . factorization

-- | Number of prime factors of /n/.
--
-- For the number of /distinct/ prime factors, use 'littleOmega'.
bigOmega :: Integer -> Integer
bigOmega = genericLength . factorization

-- | True if the given number is greater than or equal to the lower bound
-- and less than or equal to the upper bound.
between :: Ord a
        => a      -- ^ Lower bound, inclusive
        -> a      -- ^ Upper bound, inclusive
        -> a      -- ^ /n/
        -> Bool   -- ^ True if lower bound <= /n/ <= upper bound
between lower upper n = n >= lower && n <= upper

-- | Number of /distinct/ prime factors of /n/.
littleOmega :: Integer -> Integer
littleOmega = genericLength . primeFactors

-- | Calculates pairs of numbers that can be multiplied together to produce
-- the given number.
multiplicands :: Integral a => a -> [(a, a)]
multiplicands n = nub $ map zipT $ filter (flip divides n) $ enumFromTo 1 n
  where
    zipT = sortT . ap (,) (div n)

-- | Number of divisors of a given number.
numDivisors :: Integral a => Integer -> a
numDivisors = product . map (succ . genericLength) . group . factorization

-- | Sum of the divisors of a given number.
sumDivisors :: Integer -> Integer
sumDivisors = (-) =<< go
  where
    pow' = liftM2 (,) head genericLength
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

-- | List of all abundant numbers.
abundantNumbers :: [Integer]
abundantNumbers = filter (memoAbundantNumbers !) [1..28123]

-- | True if the number is the sum of two abundant numbers
isAbundantSum :: Integer -> Bool
isAbundantSum n = any (isAbundant . (n -)) $ takeWhile (< n) abundantNumbers

-- | The /nth/ Fibonacci number.
fibonacci :: Integral a => a -> a
fibonacci = genericIndex fibonaccis

-- | Infinite list of Fibonacci numbers
fibonaccis :: Integral a => [a]
fibonaccis = 0 : 1 : zipWith (+) fibonaccis (tail fibonaccis)

-- | List of all the truncated numbers associated with the given number.
--
-- ==== Examples
--
-- >>> truncateN 3797
-- [3797, 797, 97, 7, 379, 37, 3]
--
-- That is, the returned list consists of the original number, plus all
-- the numbers formed by successively removing digits from both ends.
truncateN :: Integral a => a -> [a]
truncateN n = n : go' tail n ++ go' init n
  where
    go' f n = go f ((f . digits) n)
    go _ [] = []
    go f ns = unDigits ns : go f (f ns)

-- | Evaluates the result of the quadratic equation /n^2/ + /an/ + /b/.
quadratic :: Num a
          => a    -- ^ /a/
          -> a    -- ^ /b/
          -> a    -- ^ /n/
          -> a    -- ^ /n ^ 2/ + /an/ + /b/
quadratic a b n = n ^ 2 + a * n + b

-- | Returns true if the result of evaluating the quadratic expression
-- /n ^ 2/ + /an/ + /b/ is a prime number.
isPrimeQuadratic :: Integral a
                 => a       -- ^ /a/
                 -> a       -- ^ /b/
                 -> a       -- ^ /n/
                 -> Bool    -- ^ True if /n ^ 2/ + /an/ + /b/ is prime
isPrimeQuadratic = ((isPrime .) .) . quadratic

-- | Number of consecutive primes generated by a quadratic expression
-- when fed /a/ and /b/.
consecutiveQuadraticPrimes :: (Num a, Ord a)
                           => Integer     -- ^ /a/
                           -> Integer     -- ^ /b/
                           -> a           -- Number of consecutive primes
consecutiveQuadraticPrimes a b = go 0 0
  where
    go acc n
      | (not . isPrimeQuadratic a b) n = acc
      | otherwise = go (1 + acc) (n + 1)

-- | True if a number, along with all its associated truncated numbers, are prime.
isTruncatablePrime :: Integer -> Bool
isTruncatablePrime = all isPrime . truncateN

-- | <https://en.wikipedia.org/wiki/Binomial_coefficient Binomial coefficient> of /n/.
binomialCoefficient :: Integral a => a -> a
binomialCoefficient n = factorial (2 * n) `div` (factorial n * factorial n)

-- | Multiplies two numbers, modulo some other number.
modMult :: Integral a
        => a  -- ^ /m/
        -> a  -- ^ /a/
        -> a  -- ^ /b/
        -> a  -- ^ /(a * b) % m/
modMult m a b = (a * b) `mod` m

-- | A 'product' function that uses modular multiplication.
modProduct :: Integral a => a -> [a] -> a
modProduct m = foldl' (modMult m) 1 . map (`mod` m)

-- | Raises a number to a given power, modulo another number.
modPower :: Integral a
         => a     -- ^ /m/
         -> a     -- ^ /a/
         -> a     -- ^ /b/
         -> a     -- ^ /bth/ power of /a/, modulo /m/
modPower m a b = (modProduct m . genericTake b . repeat) a

-- | Adds two numbers, modulo some other number.
modAdd :: Integral a
       => a   -- ^ /m/
       -> a   -- ^ /a/
       -> a   -- ^ /b/
       -> a   -- ^ /(a + b) % m/
modAdd m a b = (a + b) `mod` m

-- | A 'sum' function that uses modular addition.
modSum :: Integral a => a -> [a] -> a
modSum m = foldr (modAdd m) 0 . map (flip mod m)

-- | <https://en.wikipedia.org/wiki/Combination Combination> function.
--
-- This is best used as an infix operator.
--
-- ==== Examples
--
-- >>> 10 `choose` 5
-- 252
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
isCoprime a b = (null . uncurry intersect) (factorization a, factorization b)

-- | <https://en.wikipedia.org/wiki/Euler's_totient_function Euler's totient function>.
--
-- The totient function counts the number of positive integers up to /n/ that
-- are relatively prime to /n/.
totient :: Integer -> Integer
totient n =
  let ratio = foldr ((*) . (1 -) . (1 %)) (n % 1) $ primeFactors n
   in (liftM2 div numerator denominator) ratio

-- | Calculates /n/ \/ 'totient'(n).
totientRatio :: Fractional a => Integer -> a
totientRatio n = fromIntegral n / fromIntegral (totient n)

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

-- | True if a number is a /Lychrel number/, i.e., if you reverse its digits
-- and add them together, and keep applying that operation, you will never
-- reach a palindromic number.
--
-- Lychrel numbers are theoretical. 'isLychrel' assumes that a number will
-- either produce a palindrome or prove to be Lychrel in 50 iterations or
-- less, as described in <https://projecteuler.net/problem=55 Euler Problem #55>.
isLychrel :: Integral a => a -> Bool
isLychrel = go 0
  where
    go 50 _ = True
    go i n =
      let n' = (unDigits . reverse . digits) n
          n'' = n + n'
       in if isPalindrome n'' then False else go (succ i) n''

-- | Number of digits in the base 10 representation of the number.
digitsIn :: (Integral a, Integral b) => a -> b
digitsIn 0 = 1
digitsIn n
  | n < 0 = digitsIn (abs n)
  | n > 0 = floor $ logBase 10 (fromIntegral n) + 1

-- | Extracts the /nth/ digit from the number. Digit numbering starts from
-- the left at 1.
extract1 :: Integral a
         => a   -- ^ /n/
         -> a   -- ^ The number
         -> a   -- ^ The /nth/ digit in the base 10 representation of the number
extract1 n x =
  let n' = digitsIn x - n + 1
   in x `rem` (10 ^ n') `div` (10 ^ (n' - 1))

-- | Extracts the number formed by taking the 3 consecutive digits starting
-- at /n/ from the number. Digit number starts from the left at 1
extract3 :: Integral a
         => a   -- ^ /n/
         -> a   -- ^ The number
         -> a   -- ^ The /nth/ digit in the base 10 representation of the number
extract3 n x =
  let a = extract1 n x
      b = extract1 (n + 1) x
      c = extract1 (n + 2) x
   in 100 * a + 10 * b + c


--  Stored values for memoization
-------------------------------------------------------------------------------

memoCollatzLengths :: Array Integer Integer
memoCollatzLengths = listArray (1, 1000000) $ map collatzLength [1..1000000]

memoSumDivisors :: Array Integer Integer
memoSumDivisors = listArray (1, 30000) $ map sumDivisors [1..30000]

memoAbundantNumbers :: Array Integer Bool
memoAbundantNumbers = listArray (1, 28123) $ map isAbundant [1..28123]

memoFactorials :: Array Integer Integer
memoFactorials = listArray (0, 1000000) $ map factorial [0..1000000]

memoSumDigitFactorials :: Array Integer Integer
memoSumDigitFactorials = listArray (0, 1000000) $ map sumDigitFactorial [0..1000000]
