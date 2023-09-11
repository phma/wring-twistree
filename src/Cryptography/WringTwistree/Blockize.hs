module Cryptography.WringTwistree.Blockize
  ( exp4_2adic
  , exp4_base2
  , binaryStr
  , blockize
  ) where

import Data.Bits
import Data.Word
import Data.Array.Unboxed
import Data.List.Split (chunksOf)
import qualified Data.ByteString.Lazy as BL
import Cryptography.WringTwistree.Compress
import Text.Printf

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

pad :: BL.ByteString -> [Word8]
pad bs = (BL.unpack bs) ++ [0..(blockSize-1)]

-- Breaks into blocks of 32 bytes, padding the last one to 32 bytes.
-- If the last block is already 32 bytes, adds another block of 32 bytes.
blockize :: BL.ByteString -> [UArray Int Word8]
blockize bs = map (listArray (0,(blockSize-1))) $ filter ((== blockSize) . length) $
  chunksOf blockSize $ pad bs
