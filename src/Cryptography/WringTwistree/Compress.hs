module Cryptography.WringTwistree.Compress
  ( blockSize
  , relPrimes
  , lfsr
  , backCrc
  , compress
  , compress2
  , compress3
  ) where

{- This module is used in Twistree.
 - It compresses two or three 32-byte blocks into one, using three s-boxes in
 - an order specified by the sboxalt argument.
 -}

import Data.Bits
import Data.Word
import Data.List (mapAccumR)
import Data.Array.Unboxed
import Cryptography.WringTwistree.Mix3
import Cryptography.WringTwistree.RotBitcount
import Cryptography.WringTwistree.Sboxes

blockSize :: Integral a => a
blockSize = 32
twistPrime :: Integral a => a
twistPrime = 37
-- blockSize must be a multiple of 4. Blocks in the process of compression
-- can be any size from blockSize to 3*blockSize in steps of 4. twistPrime is
-- the smallest prime greater than blockSize, which is relatively prime to all
-- block sizes during compression.

relPrimes :: UArray Word16 Word16
-- 3/4 of this is waste. The numbers are Word16, because the last number is
-- 19, and the program will multiply 31 by 19, which doesn't fit in Word8.
relPrimes = listArray (blockSize,3*blockSize)
  (map (fromIntegral . findMaxOrder . (`div` 3))
       [blockSize..3*blockSize])

lfsr1 :: Word32 -> Word32
lfsr1 n = xor ((n .&. 1) * 0x84802140) (n .>>. 1)

lfsr :: UArray Word32 Word32
lfsr = listArray (0,255) (map (\n -> (iterate lfsr1 (fromIntegral n)) !! 8) [0..255])

backCrc1 :: Word32 -> Word32 -> Word32
backCrc1 a b = (a .>>. 8) `xor` (lfsr ! (a .&. 255)) `xor` b

backCrcM :: Word32 -> Word8 -> (Word32,Word8)
backCrcM a b = (c,(fromIntegral c)) where
  c = backCrc1 a (fromIntegral b)

backCrc :: [Word8] -> [Word8]
backCrc bytes = snd $ mapAccumR backCrcM 0xdeadc0de bytes

roundCompress ::
  UArray (Word8,Word8) Word8 ->
  UArray Int Word8 ->
  Int ->
  UArray Int Word8
roundCompress sbox buf sboxalt = i4 where
  bnd = bounds buf
  len = snd bnd + 1
  rprime = relPrimes ! (fromIntegral len)
  i1 = mix3Parts buf (fromIntegral rprime)
  i2 = listArray bnd $ map (sbox !) $ zip (drop sboxalt cycle3) (elems i1)
  i3 = rotBitcount i2 twistPrime
  i4 = listArray (0,len-5) $ backCrc (elems i3)

compress :: UArray (Word8,Word8) Word8 -> UArray Int Word8 -> Int -> UArray Int Word8
compress sbox buf sboxalt
  | len <= blockSize = buf
  | len `mod` twistPrime == 0 = error "bad block size"
  | otherwise = compress sbox (roundCompress sbox buf sboxalt) sboxalt
  where len = snd (bounds buf) + 1

{-
compress2 takes 100x operations, compress3 takes 264x operations.
Twistree does twice as many compress2 calls as compress3 calls.
So it spends 100 ms in compress2 for every 132 ms in compress3, or 43% and 57%.
Profiling shows 42.5% and 56.0%, with the rest being blockize.
Hashing 1 MiB takes 12.5 s on my box, using two threads, one for the 2-tree and
one for the 3-tree. The 3-tree takes longer, so that's 16384 compress3 calls
(ignoring the few compress2 calls in the 3-tree) in 12.5 s, or 763 ms for compress3
and 289 ms for compress2.
-}

compress2 ::
  UArray (Word8,Word8) Word8 ->
  UArray Int Word8 ->
  UArray Int Word8 ->
  Int ->
  UArray Int Word8
compress2 sbox buf0 buf1 sboxalt = compress sbox buf sboxalt where
  (beg0,end0) = bounds buf0
  (beg1,end1) = bounds buf1
  len = end0 + end1 + 2 - beg0 - beg1
  buf = listArray (0,len-1) (elems buf0 ++ elems buf1)

compress3 ::
  UArray (Word8,Word8) Word8 ->
  UArray Int Word8 ->
  UArray Int Word8 ->
  UArray Int Word8 ->
  Int ->
  UArray Int Word8
compress3 sbox buf0 buf1 buf2 sboxalt = compress sbox buf sboxalt where
  (beg0,end0) = bounds buf0
  (beg1,end1) = bounds buf1
  (beg2,end2) = bounds buf2
  len = end0 + end1 + end2 + 3 - beg0 - beg1 - beg2
  buf = listArray (0,len-1) (elems buf0 ++ elems buf1 ++ elems buf2)
