module Cryptography.Twistree
  ( Twistree
  , linearTwistree
  , keyedTwistree
  , hashPairs
  , hashTriples
  , hash
  ) where

{-
This hash function uses a double-tree construction, as shown in this drawing:

                                                  2
                               -------------------+-------------------
               ----------------+---------------                      |
       --------+--------               -------+---------             |
   ----+----       ----+----       ----+----       ----+----       --+---
 --+--   --+--   --+--   --+--   --+--   --+--   --+--   --+--   --+--  |
-+- -+- -+- -+- -+- -+- -+- -+- -+- -+- -+- -+- -+- -+- -+- -+- -+- -+- |
4 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
--+-- --+-- --+-- --+-- --+-- --+-- --+-- --+-- --+-- --+-- --+-- --+-- |
  ------+------     ------+------     ------+------     ------+------   |
        ------------------+------------------                 -----+-----
                          ---------------------+--------------------
                                               3
2 3
-+-
 H

*   A block of the message to be hashed, including padding at the end.
4   Binary representation of exp(4). One is used in the binary tree and the
    other in the ternary tree.
3   Output of the ternary tree
2   Output of the binary tree
H   Final hash output
-}

import Cryptography.WringTwistree.Compress
import Cryptography.WringTwistree.Blockize
import Cryptography.WringTwistree.Sboxes
import Control.Parallel
import Data.Word
import Data.Bits
import Data.Array.Unboxed
import Data.Foldable (foldl')
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

data Twistree = Twistree
  { sbox    :: UArray (Word8,Word8) Word8
  } deriving Show

compressPairs :: UArray (Word8,Word8) Word8 -> [UArray Int Word8] -> [UArray Int Word8]
compressPairs _ [] = []
compressPairs _ [x] = [x]
compressPairs sbox (x:y:xs) = pseq (compress2 sbox x y 0) $
  ((compress2 sbox x y 0) : compressPairs sbox xs)

hashPairs :: UArray (Word8,Word8) Word8 -> [UArray Int Word8] -> UArray Int Word8
hashPairs _ [] = undefined -- can't happen, there's always at least exp(4)
hashPairs _ [x] = x
hashPairs sbox x = par (compressPairs sbox x) $
  hashPairs sbox (compressPairs sbox x)

compressTriples :: UArray (Word8,Word8) Word8 -> [UArray Int Word8] -> [UArray Int Word8]
compressTriples _ [] = []
compressTriples _ [x] = [x]
compressTriples sbox [x,y] = [compress2 sbox x y 1]
compressTriples sbox (x:y:z:xs) = pseq (compress3 sbox x y z 1) $
  ((compress3 sbox x y z 1) : compressTriples sbox xs)

hashTriples :: UArray (Word8,Word8) Word8 -> [UArray Int Word8] -> UArray Int Word8
hashTriples _ [] = undefined -- can't happen, there's always at least exp(4)
hashTriples _ [x] = x
hashTriples sbox x = par (compressTriples sbox x) $
  hashTriples sbox (compressTriples sbox x)

linearTwistree = Twistree linearSbox

-- | Creates a Twistree with the given key.
-- To convert a String to a ByteString, put "- utf8-string" in your
-- package.yaml dependencies, import Data.ByteString.UTF8, and use
-- fromString.
keyedTwistree :: B.ByteString -> Twistree
keyedTwistree key = Twistree sbox where
  sbox = sboxes key

hash :: Twistree -> BL.ByteString -> UArray Int Word8
hash twistree stream = par blocks $ par h2 $ par h3 $
  compress2 (sbox twistree) h2 h3 2 where
    blocks = blockize stream
    h2 = hashPairs (sbox twistree) (exp4_2adic : blocks)
    h3 = hashTriples (sbox twistree) (exp4_base2 : blocks)
