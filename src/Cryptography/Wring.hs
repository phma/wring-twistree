module Cryptography.Wring
  ( Wring (..)
  , linearSbox
  , roundEncrypt
  ) where

import Cryptography.WringTwistree.Mix3
import Cryptography.WringTwistree.RotBitcount
import Data.Word
import Data.Bits
import Data.Array.Unboxed

data Wring = Wring
  { sbox    :: UArray (Word8,Word8) Word8
  , invSbox :: UArray (Word8,Word8) Word8
  }

nRounds :: Integral a => a -> a
nRounds len
  | len < 3 = 1
  | otherwise = (nRounds (len `div` 3)) +1

linearSbox = array ((0,0),(2,255))
  [ ((i,j),xor i j) | i <- [0..2], j <- [0..255] ]
  :: UArray (Word8,Word8) Word8

{- A round of encryption consists of four steps:
 - mix3Parts
 - sboxes (omitted)
 - rotBitcount
 - add byte index xor round number (omitted)
 -}

roundEncrypt :: (Ix a,Integral a,Bits a) => 
  a -> UArray (Word8,Word8) Word8 ->UArray a Word8 -> a -> UArray a Word8
roundEncrypt rprime sbox buf rond = rotBitcount (mix3Parts buf (fromIntegral rprime)) 1

roundDecrypt :: (Ix a,Integral a,Bits a) => 
  a -> UArray (Word8,Word8) Word8 ->UArray a Word8 -> a -> UArray a Word8
roundDecrypt rprime sbox buf rond = mix3Parts (rotBitcount buf (-1)) (fromIntegral rprime)
