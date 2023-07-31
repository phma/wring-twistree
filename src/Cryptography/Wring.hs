module Cryptography.Wring
  ( Wring (..)
  , linearSbox
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

linearSbox = array ((0,0),(2,255))
  [ ((i,j),xor i j) | i <- [0..2], j <- [0..255] ]
  :: UArray (Word8,Word8) Word8
