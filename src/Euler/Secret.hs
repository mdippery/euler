{- This is free and unencumbered software released into the public domain.
   For the full text of the license, see the file LICENSE.
   Additional licensing information may be found at http://unlicense.org. -}

{-|
  Module      : Euler.Secret
  Description : Encryption algorithms
  License     : UNLICENSE
  Maintainer  : michael@monkey-robot.com

  Encryption, decryption, and other ways to handle secure messages.
-}
module Euler.Secret
  (
    -- * Decryption
    decrypt
  , decryptFile
  , tryDecrypt
  , tryDecryptFile

    -- * Keys
  , allKeys

    -- * File I/O
  , loadFile
  ) where

import Data.Bits (xor)
import Data.Char (chr, ord, isLetter, isNumber, isPunctuation, isSeparator, isSpace)
import Data.List (cycle, words)

import Data.List.Split (splitOn)

import Euler.Bool (($||))
import Euler.Text (toInt)

-- | List of all possible three-letter encryption keys.
allKeys :: [String]
allKeys = [[a, b, c] | a <- ['a'..'z'], b <- ['a'..'z'], c <- ['a'..'z']]

-- | Decrypts a list of "bytes" (integers) into a string using the given
-- encryption key.
decrypt :: String     -- ^ Encryption key
        -> [Int]      -- ^ Encrypted message, as a list of bytes
        -> String     -- ^ Decrypted message
decrypt key bytes = (map chr . zipWith xor bytes . (map ord . cycle)) key

-- | Decrypts the message stored in the given file using the given key.
decryptFile :: FilePath     -- ^ Path to file
            -> String       -- ^ Encryption key
            -> IO String    -- ^ Decrypted message
decryptFile path key = fmap (decrypt key) (loadFile path)

-- | Passes the encryped message through every key returned by 'allKeys'.
tryDecrypt :: [Int]       -- ^ Encrypted message
           -> [String]    -- ^ Result of decrypting the message using every key in 'allKeys'
tryDecrypt bytes = filter (all isValid) (msgs bytes)
  where
    isValid = flip ($||) [isLetter, isNumber, isPunctuation, isSeparator]
    msgs = flip map allKeys . flip decrypt

-- | Passes the contents of the encrypted file through every key returned by 'allKeys'.
tryDecryptFile :: FilePath      -- ^ Path to the encrypted file
               -> IO [String]   -- ^ Result of decrypting the encryped message using every key in 'allKeys'
tryDecryptFile path = fmap tryDecrypt (loadFile path)

-- | Loads an "encrypted" file into a list of "bytes" (integers).
--
-- An encrypted file is just a comma-separated list of integers representing
-- ASCII code points, as described in
-- <https://projecteuler.net/problem=59 Problem #59>.
loadFile :: FilePath    -- ^ File to open
         -> IO [Int]    -- ^ Contents of file as a list of ASCII code points
loadFile path =
  fmap
    (map toInt
     . splitOn ","
     . takeWhile (not . isSpace)
    )
  (readFile path)
