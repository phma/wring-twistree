module Cryptanalysis
  ( key96_0
  , key96_1
  , key96_2
  , key96_3
  ) where

import Data.Word
import Data.Bits
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))
import Data.Foldable (toList,foldl')
import Control.Parallel
import Control.Parallel.Strategies
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (fromString)
import Cryptography.WringTwistree.KeySchedule

key96_0 = "Водворетраванатраведрова.Нерубидрованатраведвора!"
key96_1 = "Водворетраванатраведрова.Нерубидрованатраведвора "
key96_2 = "Водворетраванатраведрова,Нерубидрованатраведвора!"
key96_3 = "Водворетраванатраведрова,Нерубидрованатраведвора "
-- Russian tongue twister
-- In the yard is grass, on the grass is wood.
-- Do not chop the wood on the grass of yard.
-- 96 bytes in UTF-8 with single bit changes.
