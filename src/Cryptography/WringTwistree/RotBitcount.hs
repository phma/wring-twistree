{-# LANGUAGE BangPatterns #-}
module Cryptography.WringTwistree.RotBitcount
  ( rotBitcount
  , rotBitcount'
  , rotFixed
  , rotFixed'
  )  where

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
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.ST
import Control.Monad
import Debug.Trace

rotBitcount :: V.Vector Word8 -> Int -> V.Vector Word8
-- See Rust code for a timing leak which may be present in (.>>.).
rotBitcount src mult = V.fromListN len
  [ (src V.! ((i+len-byte)   `mod` len) .<<. bit) .|.
    (src V.! ((i+len-byte-1) `mod` len) .>>. (8-bit)) | i <- [0..(len-1)]]
  where
    len = V.length src
    multmod = mult `mod` (len * 8)
    bitcount = sum $ map popCount $ V.toList src
    rotcount = (bitcount * multmod) `mod` (len * 8)
    !byte = rotcount .>>. 3
    !bit = rotcount .&. 7

rotBitcount' :: MV.MVector s Word8 -> Int -> MV.MVector s Word8 -> ST s ()
rotBitcount' src mult dst = do
    bitcount <- MV.foldl' (\acc x -> acc + popCount x) 0 src
    let len = MV.length src
        !multmod = mult `mod` (len * 8)
        rotcount = (bitcount * multmod) `rem` (len * 8)
        !byte = rotcount .>>. 3
        !bit = rotcount .&. 7
    forM_ [0..MV.length src - 1] $ \i -> do
        l <- MV.read src ((i+len-byte) `rem` len)
        r <- MV.read src ((i+len-byte-1) `rem` len)
        MV.write dst i ((l .<<. bit) .|. (r .>>. (8-bit)))

-- For cryptanalyzing a weakened version which replaces the SACful rotBitcount
-- with a linear fixed rotation. It rotates by two more than half the number
-- of bits in the buffer, times mult.
rotFixed :: V.Vector Word8 -> Int -> V.Vector Word8
rotFixed src mult = V.fromListN len
  [ (src V.! ((i+len-byte)   `mod` len) .<<. bit) .|.
    (src V.! ((i+len-byte-1) `mod` len) .>>. (8-bit)) | i <- [0..(len-1)]]
  where
    len = V.length src
    multmod = mult `mod` (len * 8)
    bitcount = len * 4 + 2
    rotcount = (bitcount * multmod) `mod` (len * 8)
    !byte = rotcount .>>. 3
    !bit = rotcount .&. 7

rotFixed' :: MV.MVector s Word8 -> Int -> MV.MVector s Word8 -> ST s ()
rotFixed' src mult dst = do
    let len = MV.length src
        bitcount = len * 4 + 2
        !multmod = mult `mod` (len * 8)
        rotcount = (bitcount * multmod) `rem` (len * 8)
        !byte = rotcount .>>. 3
        !bit = rotcount .&. 7
    forM_ [0..MV.length src - 1] $ \i -> do
        l <- MV.read src ((i+len-byte) `rem` len)
        r <- MV.read src ((i+len-byte-1) `rem` len)
        MV.write dst i ((l .<<. bit) .|. (r .>>. (8-bit)))
