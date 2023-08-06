module Cryptography.WringTwistree.Permute
  ( swapOrder
  ) where

{- This module is used in both Wring and Twistree.
 - It is part of the keying algorithm, which turns a byte string
 - into three s-boxes. permSbox takes a sequence of 96 Word16 (the high
 - bit is ignored) and returns a permutation of [0x00..0xff], which is
 - one of the three s-boxes.
 -}

import Data.Bits
import Data.Array.Unboxed
import Data.Word
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))

swapOrder :: Word16 -> [Int]
swapOrder n = map fromIntegral [x2,x3,x4,x5,x6,x7,x8] where
  x2 = n .&. 1
  x4 = (n `div` 2) .&. 3
  x8 = (n `div` 8) .&. 7
  x18 = (n `div` 64) .&. 15 + 1
  x3 = x18 `mod` 3
  x6 = x18 `div` 3
  x35' = (n `div` 1024) .&. 31 + 1
  x35 = if x35' > 16 then x35' + 1 else x35'
  x5 = x35 `mod` 5
  x7 = x35 `div` 5
