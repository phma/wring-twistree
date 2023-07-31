module Cryptography.Wring
  ( Wring (..)
  ) where

import Cryptography.WringTwistree.Mix3
import Cryptography.WringTwistree.RotBitcount
import Data.Word
import Data.Array.Unboxed

data Wring = Wring
  { sbox    :: UArray (Word8,Word8) Word8
  , invSbox :: UArray (Word8,Word8) Word8
  }
