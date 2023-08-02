module Cryptography.Wring
  ( Wring (..)
  , linearWring
  , encrypt
  , decrypt
  ) where

import Cryptography.WringTwistree.Mix3
import Cryptography.WringTwistree.RotBitcount
import Data.Word
import Data.Bits
import Data.Array.Unboxed
import Data.Foldable (toList,foldl')

data Wring = Wring
  { sbox    :: UArray (Word8,Word8) Word8
  , invSbox :: UArray (Word8,Word8) Word8
  }

nRounds :: Integral a => a -> a
nRounds len
  | len < 3 = 1
  | otherwise = (nRounds (len `div` 3)) +1

linearSbox = array ((0,0),(2,255))
  [ ((i,j),rotate i (fromIntegral (3*j+1))) | i <- [0..2], j <- [0..255] ]
  :: UArray (Word8,Word8) Word8

linearInvSbox = array ((0,0),(2,255))
  [ ((i,j),rotate i (fromIntegral (7-3*j))) | i <- [0..2], j <- [0..255] ]
  :: UArray (Word8,Word8) Word8

linearWring = Wring linearSbox linearInvSbox

{- A round of encryption consists of four steps:
 - mix3Parts
 - sboxes (omitted)
 - rotBitcount
 - add byte index xor round number (omitted)
 -}

roundEncrypt :: (Ix a,Integral a,Bits a) => 
  a -> UArray (Word8,Word8) Word8 -> UArray a Word8 -> a -> UArray a Word8
roundEncrypt rprime sbox buf rond = i4 where
  i1 = mix3Parts buf (fromIntegral rprime)
  i2 = i1
  i3 = rotBitcount i2 1
  i4 = i3

roundDecrypt :: (Ix a,Integral a,Bits a) => 
  a -> UArray (Word8,Word8) Word8 -> UArray a Word8 -> a -> UArray a Word8
roundDecrypt rprime sbox buf rond = i4 where
  i1 = buf
  i2 = rotBitcount i1 (-1)
  i3 = i2
  i4 = mix3Parts i3 (fromIntegral rprime)

encrypt :: (Ix a,Integral a,Bits a) => Wring -> UArray a Word8 -> UArray a Word8
encrypt wring buf = foldl' (roundEncrypt rprime (sbox wring)) buf rounds
  where
    len = fromIntegral $ snd (bounds buf) +1
    rprime = fromIntegral $ findMaxOrder (len `div` 3)
    rounds = [0 .. (fromIntegral (nRounds len) -1)]

decrypt :: (Ix a,Integral a,Bits a) => Wring -> UArray a Word8 -> UArray a Word8
decrypt wring buf = foldl' (roundDecrypt rprime (invSbox wring)) buf rounds
  where
    len = fromIntegral $ snd (bounds buf) +1
    rprime = fromIntegral $ findMaxOrder (len `div` 3)
    rounds = reverse [0 .. (fromIntegral (nRounds len) -1)]
