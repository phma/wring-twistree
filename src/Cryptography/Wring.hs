module Cryptography.Wring
  ( Wring (..)
  , linearSbox
  , linearInvSbox
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
  } deriving Show

nRounds :: Integral a => a -> a
nRounds len
  | len < 3 = 3
  | otherwise = (nRounds (len `div` 3)) +1

linearSbox = array ((0,0),(2,255))
  [ ((i,j),rotate j (fromIntegral (3*i+1))) | i <- [0..2], j <- [0..255] ]
  :: UArray (Word8,Word8) Word8

linearInvSbox = array ((0,0),(2,255))
  [ ((i,j),rotate j (fromIntegral (7-3*i))) | i <- [0..2], j <- [0..255] ]
  :: UArray (Word8,Word8) Word8

linearWring = Wring linearSbox linearInvSbox

{- A round of encryption consists of four steps:
 - mix3Parts
 - sboxes
 - rotBitcount
 - add byte index xor round number (omitted)
 -}

cycle3 = 0 : 1 : 2 : cycle3

roundEncrypt :: (Ix a,Integral a,Bits a) => 
  a -> UArray (Word8,Word8) Word8 -> UArray a Word8 -> a -> UArray a Word8
roundEncrypt rprime sbox buf rond = i4 where
  bnd = bounds buf
  i1 = mix3Parts buf (fromIntegral rprime)
  i2 = listArray bnd $ map (sbox !) $ zip (drop (fromIntegral rond) cycle3) (elems i1)
  i3 = rotBitcount i2 1
  i4 = i3

roundDecrypt :: (Ix a,Integral a,Bits a) => 
  a -> UArray (Word8,Word8) Word8 -> UArray a Word8 -> a -> UArray a Word8
roundDecrypt rprime sbox buf rond = i4 where
  bnd = bounds buf
  i1 = buf
  i2 = rotBitcount i1 (-1)
  i3 = listArray bnd $ map (sbox !) $ zip (drop (fromIntegral rond) cycle3) (elems i2)
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
