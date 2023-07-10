module Cryptography.WringTwistree.Mix3
  ( mix
  ) where

import Data.Bits
import Data.Word
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))

mix :: (Num t,Bits t) => t -> t -> t -> t
mix a b c = xor a mask
  where mask = (a .|. b .|. c) - (a .&. b .&. c)
