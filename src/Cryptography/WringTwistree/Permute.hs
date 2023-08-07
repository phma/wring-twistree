module Cryptography.WringTwistree.Permute
  ( permut8
  , dealInx
  , invDealInx
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
import Data.List
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)), update)

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

swapmute :: Seq.Seq a -> [Int] -> Int -> Seq.Seq a
swapmute ys [] _ = ys
swapmute ys (x:xs) n = swapmute ys' xs (n+1) where
  b = Seq.index ys x
  c = Seq.index ys n
  ys' = update x c (update n b ys)

permut8 :: Seq.Seq a -> Word16 -> Seq.Seq a
permut8 ys n = swapmute ys (swapOrder n) 1

dealInxA :: Int -> Int
dealInxA n = ((n+1)*10) `mod` 257-1

invDealInxA :: Int -> Int
invDealInxA n = ((n+1)*180) `mod` 257-1

-- polynomial 100011101, 3 bit overflow table
shift3    = listArray (0,7) [0x00,0x1d,0x3a,0x27,0x74,0x69,0x4e,0x53] :: UArray Int Int
invShift3 = listArray (0,7) [0x00,0xad,0x47,0xea,0x8e,0x23,0xc9,0x64] :: UArray Int Int

dealInxB :: Int -> Int
dealInxB n = ((n .&. 0x1f) `shiftL` 3) `xor` (shift3 ! ((n .&. 0xe0) `shiftR` 5))

invDealInxB :: Int -> Int
invDealInxB n = ((n .&. 0xf8) `shiftR` 3) `xor` (invShift3 ! (n .&. 0x07))

dealInx = dealInxB
invDealInx = invDealInxB
