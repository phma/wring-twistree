module Cryptography.WringTwistree.KeySchedule
  ( extendKey
  , keySchedule
  , reschedule
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
import Data.Foldable (toList,foldl')
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
-- Extends the key, if it isn't empty, to be at least as long as 384 words.
extendKey str = extendKey_ (B.unpack str) 0 n where
  n = if (B.length str)>0 then -((-384) `div` (B.length str)) else 0

mul65537 :: Word16 -> Word16 -> Word16
mul65537 a b = fromIntegral ((((fromIntegral a)+1) * ((fromIntegral b)+1)) `mod` 65537 - 1)

alter :: Seq.Seq Word16 -> (Word16,Int) -> Seq.Seq Word16
-- subkey is 96 long. Alters the element at position inx.
alter subkey (keyWord,inx) = update inx newval subkey where
  i1 = mul65537 (Seq.index subkey inx) keyWord
  i2 = i1 + ((Seq.index subkey (mod (inx+59) 96)) `xor`
	     (Seq.index subkey (mod (inx+36) 96)) `xor`
	     (Seq.index subkey (mod (inx+62) 96)))
  newval = rotate i2 8

keySchedule :: B.ByteString -> Seq.Seq Word16
keySchedule key = foldl' alter initial extended where
  extended = zip (extendKey key) (map (`mod` 96) [0..])
  initial = Seq.fromList (take 96 swap13mult)

reschedule :: Seq.Seq Word16 -> Seq.Seq Word16
reschedule subkey = foldl' alter subkey (zip (repeat 40504) [0..95])
-- 40505 is a primitive root near 65537/φ
