module Cryptography.Twistree
  ( hashPairs
  , hashTriples
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
import Data.Word
import Data.Bits
import Data.Array.Unboxed
import Data.Foldable (foldl')
import qualified Data.ByteString.Lazy as BL

compressPairs :: UArray (Word8,Word8) Word8 -> [UArray Int Word8] -> [UArray Int Word8]
compressPairs _ [] = []
compressPairs _ [x] = [x]
compressPairs sbox (x:y:xs) = ((compress2 sbox x y 0) : compressPairs sbox xs)

hashPairs :: UArray (Word8,Word8) Word8 -> [UArray Int Word8] -> UArray Int Word8
hashPairs _ [] = undefined -- can't happen, there's always at least exp(4)
hashPairs _ [x] = x
hashPairs sbox x = hashPairs sbox (compressPairs sbox x)

compressTriples :: UArray (Word8,Word8) Word8 -> [UArray Int Word8] -> [UArray Int Word8]
compressTriples _ [] = []
compressTriples _ [x] = [x]
compressTriples sbox [x,y] = [compress2 sbox x y 1]
compressTriples sbox (x:y:z:xs) = ((compress3 sbox x y z 1) : compressTriples sbox xs)

hashTriples :: UArray (Word8,Word8) Word8 -> [UArray Int Word8] -> UArray Int Word8
hashTriples _ [] = undefined -- can't happen, there's always at least exp(4)
hashTriples _ [x] = x
hashTriples sbox x = hashTriples sbox (compressTriples sbox x)
