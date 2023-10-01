module Cryptography.WringTwistree.Sboxes
  ( SBox
  , sboxInx
  , cycle3
  , sboxes
  , invert
  , linearSbox
  , linearInvSbox
  ) where

{- This module is used in both Wring and Twistree.
 - It is part of the keying algorithm, which turns a byte string
 - into three s-boxes. It takes a ByteString and returns a 3×256
 - array of bytes.
 -
 - To convert a String to a ByteString, put "- utf8-string" in your
 - package.yaml dependencies, import Data.ByteString.UTF8, and use
 - fromString.
 -}

import Data.Bits
import Data.Word
import Data.Foldable (toList)
import qualified Data.ByteString as B
import Cryptography.WringTwistree.Permute
import Cryptography.WringTwistree.KeySchedule
import qualified Data.Vector.Unboxed as V

type SBox = V.Vector Word8

sboxInx :: Word8 -> Word8 -> Int
sboxInx whichBox n = fromIntegral whichBox*256 + fromIntegral n

cycle3 :: [Word8]
cycle3 = 0 : 1 : 2 : cycle3

sboxes :: B.ByteString -> SBox
sboxes key = V.fromListN (3*256) (box0 ++ box1 ++ box2) where
  box0seq = keySchedule key
  box1seq = reschedule box0seq
  box2seq = reschedule box1seq
  box0 = toList (permute256 box0seq)
  box1 = toList (permute256 box1seq)
  box2 = toList (permute256 box2seq)

invert ::SBox -> SBox
invert sbox = V.replicate (3*256) 0 V.//
  [(i*256 + fromIntegral (sbox V.! (i*256 + j)), fromIntegral j) | i <- [0..2], j <- [0..255]]

linearSbox, linearInvSbox :: SBox
linearSbox = V.fromListN (3*256)
  [ rotate j (fromIntegral (3*i+1)) | i <- [0..2], j <- [0..255] ]

linearInvSbox = V.fromListN (3*256)
  [ (rotate j (fromIntegral (7-3*i))) | i <- [0..2], j <- [0..255] ]
