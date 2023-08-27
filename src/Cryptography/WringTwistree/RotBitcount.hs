module Cryptography.WringTwistree.RotBitcount
  ( rotBitcount
  ) where

{- This module is used in both Wring and Twistree.
 - It rotates an array of bytes by a multiple of its bitcount,
 - producing another array of the same size. As long as the multiplier
 - is relatively prime to the number of bits in the array, this
 - operation satisfies the strict avalanche criterion. Changing *two*
 - bits, however, has half a chance of changing only two bits in
 - the output.
 -
 - Bit 0 of byte 0 is bit 0 of the array. Bit 0 of byte 1 is bit 8 of the array.
 - e1 00 00 00 00 00 00 00, rotated by its bitcount (4), becomes
 - 10 0e 00 00 00 00 00 00.
 -}

import Data.Bits
import Data.Word
import Data.Array.Unboxed

rotBitcount :: (Integral a,Ix a,Bits a) => UArray a Word8 -> a -> UArray a Word8
-- The type a may be signed or unsigned, but the array index must begin at 0.
-- a should hold the square of eight times the bounds; so if the bounds are
-- (0..31), Word16 is adequate, but Int16 and Word8 are not.
rotBitcount src mult = array bnd
  [ (i, (src ! ((i+len-byte)   `mod` len) `shift` bit) .|.
        (src ! ((i+len-byte-1) `mod` len) `shift` (bit-8))) | i <- [0..(len-1)]]
  where
    bnd = bounds src
    len = (snd bnd) +1
    multmod = mult `mod` (len * 8)
    bitcount = fromIntegral $ sum $ map popCount $ elems src
    rotcount = (bitcount * multmod) `mod` (len * 8)
    byte = rotcount `shift` (-3)
    bit = fromIntegral (rotcount .&. 7)
