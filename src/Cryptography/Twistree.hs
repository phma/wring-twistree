module Cryptography.Twistree
  ( Twistree
  , SBox -- reexported for use in Cryptanalysis.hs
  , sboxes -- "
  , sameBitcount -- "
  , compress -- "
  , linearSbox -- "
  , linearTwistree -- Only for cryptanalysis and testing
  , parListDeal
  , keyedTwistree
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
import Control.Parallel.Strategies
import Data.List (transpose)
import Data.List.Split
import Data.Word
import Data.Bits
import Data.Array.Unboxed
import Data.Foldable (foldl')
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Unboxed as V

data Twistree = Twistree
  { sbox    :: SBox
  } deriving Show

deal n = transpose . chunksOf n -- to be used as a parallel strategy

-- | Deals a list among threads; each thread evaluates the items of its part
-- of the list in sequence, and all the threads run in parallel. If n is 12,
-- the 0th, 12th, 24th, ... items are evaluated by one thread, the 1st, 13rd,
-- 25th, ... items are evaluated by another thread, and so on.
parListDeal
  :: Int -- ^ n, the number of threads
  -> Strategy a
  -> Strategy [a]
parListDeal n strat xs
  | n <= 1    = evalList strat xs
  | otherwise = concat `fmap` parList (evalList strat) (deal n xs)

compressPairs :: SBox -> [V.Vector Word8] -> [V.Vector Word8]
compressPairs _ [] = []
compressPairs _ [x] = [x]
compressPairs sbox (x:y:xs) = pseq (compress2 sbox x y 0) $
  ((compress2 sbox x y 0) : compressPairs sbox xs)

hashPairs :: SBox -> [V.Vector Word8] -> V.Vector Word8
hashPairs _ [] = undefined -- can't happen, there's always at least exp(4)
hashPairs _ [x] = x
hashPairs sbox x = par (compressPairs sbox x) $
  hashPairs sbox (compressPairs sbox x)

compressTriples :: SBox -> [V.Vector Word8] -> [V.Vector Word8]
compressTriples _ [] = []
compressTriples _ [x] = [x]
compressTriples sbox [x,y] = [compress2 sbox x y 1]
compressTriples sbox (x:y:z:xs) = pseq (compress3 sbox x y z 1) $
  ((compress3 sbox x y z 1) : compressTriples sbox xs)

hashTriples :: SBox -> [V.Vector Word8] -> V.Vector Word8
hashTriples _ [] = undefined -- can't happen, there's always at least exp(4)
hashTriples _ [x] = x
hashTriples sbox x = par (compressTriples sbox x) $
  hashTriples sbox (compressTriples sbox x)

-- | A `Twistree` with linear `SBox`. Used only for testing and cryptanalysis.
linearTwistree = Twistree linearSbox

-- | Creates a `Twistree` with the given key.
-- To convert a `String` to a `ByteString`, put @- utf8-string@ in your
-- package.yaml dependencies, @import Data.ByteString.UTF8@, and use
-- `fromString`.
keyedTwistree :: B.ByteString -> Twistree
keyedTwistree key = Twistree sbox where
  sbox = sboxes key

hash
  :: Twistree -- ^ The `Twistree` made with the key to hash with
  -> BL.ByteString -- ^ The text to be hashed. It's a lazy `ByteString`,
    -- so you can hash a file bigger than RAM.
  -> V.Vector Word8 -- ^ The returned hash, 32 bytes.
hash twistree stream = par blocks $ par h2 $ par h3 $
  compress2 (sbox twistree) h2 h3 2 where
    blocks = blockize stream
    h2 = hashPairs (sbox twistree) (exp4_2adic : blocks)
    h3 = hashTriples (sbox twistree) (exp4_base2 : blocks)
