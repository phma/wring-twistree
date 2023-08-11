module Cryptography.WringTwistree.KeySchedule
  ( swap13mult
  , extendKey
  ) where

{- This module is used in both Wring and Twistree.
 - It is part of the keying algorithm, which turns a byte string
 - into three s-boxes. KeySchedule takes a ByteString and returns a
 -- sequence of 96 Word16.
 --
 -- To convert a String to a ByteString, put "- utf8-string" in your
 -- package.yaml dependencies, import Data.ByteString.UTF8, and use
 -- fromString.
 -}

import Data.Bits
import Data.Word
import Data.List
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)), update)
import qualified Data.ByteString as B

-- This sequence was used as the PRNG in an Apple implementation of Forth.
-- Its cycle length is 64697.
swap13mult :: [Word16]
swap13mult = 1 : map ((`rotate` 8) . (* 13)) swap13mult

extendKey_ :: [Word8] -> Int -> Int -> [Word16]
extendKey_ str i n
  | i >= n = []
  | otherwise = (map (+ (fromIntegral (256*i))) $ map fromIntegral str)
		++ (extendKey_ str (i+1) n)

extendKey :: B.ByteString -> [Word16]
-- Extends the key, if it isn't empty, to be at least as long as 96 words.
extendKey str = extendKey_ (B.unpack str) 0 n where
  n = if (B.length str)>0 then -((-96) `div` (B.length str)) else 0
