module Cryptography.WringTwistree.Compress
  ( exp4_2adic
  , binaryStr
  , blockSize
  , relPrimes
  , lfsr
  , backCrc
  , compress
  , compress2
  , compress3
  ) where

{-
$ stack ghci --package padic
> import Math.NumberTheory.Padic
> :set -XDataKinds
> exp 4 :: Q' 2 280 -- add 24 extra bits so that bit 255 is correct
11011110 00010001 11110010
00001001 00010100 10010100 01111111 11111110 01011100 01101101 11001010
00110100 10010101 00111100 10100101 11101110 01000101 10110110 00010111
11100000 10011100 01111111 10100011 01111111 00001000 11100000 00011000
11101011 00111010 00011010 01110010 00111000 10001110 01000001 01001101.0
-}

import Data.Bits
import Data.Word
import Data.List
import Data.Array.Unboxed
import Cryptography.WringTwistree.Mix3
import Cryptography.WringTwistree.RotBitcount
import Cryptography.WringTwistree.Sboxes
import Text.Printf

-- e⁴, in two binary representations, is prepended to the
-- blocks being hashed, so that if the message is only one block,
-- two different compressed blocks are combined at the end.
exp4_2adic :: (Num a,Ix a) => UArray a Word8
exp4_2adic = listArray (0,31)
  [ 0x4d, 0x41, 0x8e, 0x38, 0x72, 0x1a, 0x3a, 0xeb
  , 0x18, 0xe0, 0x08, 0x7f, 0xa3, 0x7f, 0x9c, 0xe0
  , 0x17, 0xb6, 0x45, 0xee, 0xa5, 0x3c, 0x95, 0x34
  , 0xca, 0x6d, 0x5c, 0xfe, 0x7f, 0x94, 0x14, 0x09
  ]

exp4_base2 :: (Num a,Ix a) => UArray a Word8
exp4_base2 = listArray (0,31)
  [ 0xe8, 0xa7, 0x66, 0xce, 0x5b, 0x2e, 0x8a, 0x39
  , 0x4b, 0xb7, 0x89, 0x2e, 0x0c, 0xd5, 0x94, 0x05
  , 0xda, 0x72, 0x7b, 0x72, 0xfb, 0x77, 0xda, 0x1a
  , 0xcf, 0xb0, 0x74, 0x4e, 0x5c, 0x20, 0x99, 0x36
  ]

{-
                 01 =    1/1   = 01
                 04 =    4/1   = 04
                 08 =   16/2   = 08
...5555555555555560 =   64/6   = 0a.aaaaaaaaaaaaaa...
...5555555555555560 =  256/24  = 0a.aaaaaaaaaaaaaa...
...7777777777777780 = 1024/120 = 08.88888888888888...
...a4fa4fa4fa4fa500 = 4096/720 = 05.b05b05b05b05b0...
   ----------------      ...     -----------------
...eb3a1a72388e414d =  exp(4)  = 36.99205c4e74b0cf...
-}

binaryStr :: [Word8] -> String
binaryStr [] = ""
binaryStr (a:as) = (printf "%08b " a)++(binaryStr as)

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
  (map (fromIntegral . findMaxOrder . fromIntegral . (`div` 3))
       [blockSize..3*blockSize])

lfsr1 :: Word32 -> Word32
lfsr1 n = xor ((n .&. 1) * 0x84802140) (shiftR n 1)

lfsr :: UArray Word32 Word32
lfsr = listArray (0,255) (map (\n -> (iterate lfsr1 (fromIntegral n)) !! 8) [0..255])

backCrc1 :: Word32 -> Word32 -> Word32
backCrc1 a b = (shiftR a 8) `xor` (lfsr ! (a .&. 255)) `xor` b

backCrcM :: Word32 -> Word8 -> (Word32,Word8)
backCrcM a b = (c,(fromIntegral c)) where
  c = backCrc1 a (fromIntegral b)

backCrc :: [Word8] -> [Word8]
backCrc bytes = snd $ mapAccumR backCrcM 0xdeadc0de bytes

roundCompress :: (Ix a,Integral a,Bits a) =>
  UArray (Word8,Word8) Word8 -> UArray a Word8 -> a -> UArray a Word8
roundCompress sbox buf sboxalt = i4 where
  bnd = bounds buf
  len = snd bnd + 1
  rprime = relPrimes ! (fromIntegral len)
  i1 = mix3Parts buf (fromIntegral rprime)
  i2 = listArray bnd $ map (sbox !) $ zip (drop (fromIntegral sboxalt) cycle3) (elems i1)
  i3 = rotBitcount i2 1
  i4 = listArray (0,len-5) $ backCrc (elems i3)

compress :: (Ix a,Integral a,Bits a) =>
  UArray (Word8,Word8) Word8 -> UArray a Word8 -> a -> UArray a Word8
compress sbox buf sboxalt
  | len <= blockSize = buf
  | len `mod` twistPrime == 0 = error "bad block size"
  | otherwise = compress sbox (roundCompress sbox buf sboxalt) sboxalt
  where len = snd (bounds buf) + 1

compress2 :: (Ix a,Integral a,Bits a) =>
  UArray (Word8,Word8) Word8 ->
  UArray a Word8 ->
  UArray a Word8 ->
  a -> UArray a Word8
compress2 sbox buf0 buf1 sboxalt = compress sbox buf sboxalt where
  (beg0,end0) = bounds buf0
  (beg1,end1) = bounds buf1
  len = end0 + end1 + 2 - beg0 - beg1
  buf = listArray (0,len-1) (elems buf0 ++ elems buf1)

compress3 :: (Ix a,Integral a,Bits a) =>
  UArray (Word8,Word8) Word8 ->
  UArray a Word8 ->
  UArray a Word8 ->
  UArray a Word8 ->
  a -> UArray a Word8
compress3 sbox buf0 buf1 buf2 sboxalt = compress sbox buf sboxalt where
  (beg0,end0) = bounds buf0
  (beg1,end1) = bounds buf1
  (beg2,end2) = bounds buf2
  len = end0 + end1 + end2 + 2 - beg0 - beg1 - beg2
  buf = listArray (0,len-1) (elems buf0 ++ elems buf1 ++ elems buf2)
