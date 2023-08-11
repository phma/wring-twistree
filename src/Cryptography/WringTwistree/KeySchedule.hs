module Cryptography.WringTwistree.KeySchedule
  ( swap13mult
  ) where

{- This module is used in both Wring and Twistree.
 - It is part of the keying algorithm, which turns a byte string
 - into three s-boxes. KeySchedule takes a ByteString and returns a
 -- sequence of 96 Word16.
 -}

import Data.Bits
import Data.Word
import Data.List
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)), update)
import qualified Data.ByteString as B

-- This sequence was used as the PRNG in an Apple implementation of Forth.
-- Its cycle length is 64697.
swap13mult :: [Word16]
swap13mult = 1 : map ((`rotate` 8) . (* 13)) swap13mult
