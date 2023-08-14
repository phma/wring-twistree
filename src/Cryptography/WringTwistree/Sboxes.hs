module Cryptography.WringTwistree.Sboxes
  ( sboxes
  ) where

{- This module is used in both Wring and Twistree.
 - It is part of the keying algorithm, which turns a byte string
 - into three s-boxes. It takes a ByteString and returns a 3×256
 -- array of bytes.
 --
 -- To convert a String to a ByteString, put "- utf8-string" in your
 -- package.yaml dependencies, import Data.ByteString.UTF8, and use
 -- fromString.
 -}

import Data.Bits
import Data.Word
import Data.List
import Data.Array.Unboxed
import Data.Foldable (toList,foldl')
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)), update)
import qualified Data.ByteString as B
import Cryptography.WringTwistree.Permute
import Cryptography.WringTwistree.KeySchedule


sboxes :: B.ByteString -> UArray (Word8,Word8) Word8
sboxes key = listArray ((0,0),(2,255)) (box0 ++ box1 ++ box2) where
  box0seq = keySchedule key
  box1seq = reschedule box0seq
  box2seq = reschedule box1seq
  box0 = toList (permute256 box0seq)
  box1 = toList (permute256 box1seq)
  box2 = toList (permute256 box2seq)
