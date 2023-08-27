module Cryptography.WringTwistree.Compress
  ( exp4_2adic
  , binaryStr
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

binaryStr :: [Word8] -> String
binaryStr [] = ""
binaryStr (a:as) = (printf "%08b " a)++(binaryStr as)
