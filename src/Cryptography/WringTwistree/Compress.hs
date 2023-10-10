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
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.ST
import Control.Monad

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

-- Original purely functional version, modified to use vectors

roundCompressFun :: SBox -> V.Vector Word8 -> Int -> V.Vector Word8
roundCompressFun sbox buf sboxalt = i4 where
  len = V.length buf
  rprime = relPrimes ! (fromIntegral len)
  i1 = mix3Parts buf (fromIntegral rprime)
  i2 = V.fromListN len $ map (sbox V.!) $ zipWith sboxInx (drop sboxalt cycle3) (V.toList i1)
  i3 = rotBitcount i2 twistPrime
  i4 = V.fromListN (len-4) $ backCrc (V.toList i3)

compressFun :: V.Vector Word8 -> V.Vector Word8 -> Int -> V.Vector Word8
compressFun sbox buf sboxalt
  | len <= blockSize = buf
  | len `mod` twistPrime == 0 = error "bad block size"
  | otherwise = compressFun sbox (roundCompressFun sbox buf sboxalt) sboxalt
  where len = V.length buf

-- ST monad version modifies memory in place

roundCompressST ::
  SBox ->
  MV.MVector s Word8 ->
  MV.MVector s Word8 ->
  Int ->
  ST s (MV.MVector s Word8)
roundCompressST sbox buf tmp sboxalt = do
  let len = MV.length buf
  let rprime = relPrimes ! (fromIntegral len)
  mix3Parts' buf (fromIntegral rprime)
  forM_ [0..len-1] $ \i -> do
    a <- MV.read buf i
    MV.write tmp i (sbox V.! (sboxInx ((i + sboxalt) `rem` 3) a))
  rotBitcount' tmp twistPrime buf
  crcVec <- MV.new (len+1)
  MV.write crcVec len 0xdeadc0de
  forM_ (reverse [0..len-1]) $ \i -> do
    a <- MV.read buf i
    c <- MV.read crcVec (i+1)
    let (c',a') = backCrcM c a
    MV.write buf i a'
    MV.write crcVec i c'
  return (MV.take (len-4) buf)

compressST :: V.Vector Word8 -> V.Vector Word8 -> Int -> V.Vector Word8
compressST sbox buf sboxalt = V.create $ do
  let len = V.length buf
  let nr = (len - blockSize) `div` 4 - 1
  let rounds = [0 .. nr]
  buf <- V.thaw buf
  tmp <- MV.new len
  res <- foldM (\b r -> roundCompressST sbox b tmp sboxalt) buf rounds
  pure res

compress = compressFun

{-
compress2 takes 100x operations, compress3 takes 264x operations.
Twistree does twice as many compress2 calls as compress3 calls.
So it spends 100 ms in compress2 for every 132 ms in compress3, or 43% and 57%.
Profiling shows 42.5% and 56.0%, with the rest being blockize.
Hashing 1 MiB takes 12.5 s on my box, using two threads, one for the 2-tree and
one for the 3-tree. The 3-tree takes longer, so that's 16384 compress3 calls
(ignoring the few compress2 calls in the 3-tree) in 12.5 s, or 763 µs for compress3
and 289 µs for compress2.
-}

compress2 ::
  SBox ->
  V.Vector Word8 ->
  V.Vector Word8 ->
  Int ->
  V.Vector Word8
compress2 sbox buf0 buf1 sboxalt = compress sbox buf sboxalt where
  buf = buf0 <> buf1

compress3 ::
  SBox ->
  V.Vector Word8 ->
  V.Vector Word8 ->
  V.Vector Word8 ->
  Int ->
  V.Vector Word8
compress3 sbox buf0 buf1 buf2 sboxalt = compress sbox buf sboxalt where
  buf = buf0 <> buf1 <> buf2
