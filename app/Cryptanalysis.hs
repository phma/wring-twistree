module Cryptanalysis
  ( key96_0
  , key96_1
  , key96_2
  , key96_3
  , key30_0
  , key30_1
  , key30_2
  , key30_3
  , key6_0
  , key6_1
  , key6_2
  , key6_3
  , wring96_0
  , wring96_1
  , wring96_2
  , wring96_3
  , wring30_0
  , wring30_1
  , wring30_2
  , wring30_3
  , wring6_0
  , wring6_1
  , wring6_2
  , wring6_3
  , thueMorse
  , byteArray
  , diff1Related
  , diffRelated
  , relatedKeyHisto
  , plaintextHisto
  , sixStats
  , relatedKey
  ) where

import Data.Word
import Data.Bits
import Data.Array.Unboxed
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))
import Data.Foldable (toList,foldl')
import Control.Parallel
import Control.Parallel.Strategies
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (fromString)
import Cryptography.Wring
import Stats

key96_0 = "Водворетраванатраведрова.Нерубидрованатраведвора!"
key96_1 = "Водворетраванатраведрова.Нерубидрованатраведвора "
key96_2 = "Водворетраванатраведрова,Нерубидрованатраведвора!"
key96_3 = "Водворетраванатраведрова,Нерубидрованатраведвора "
-- Russian tongue twister
-- In the yard is grass, on the grass is wood.
-- Do not chop the wood on the grass of yard.
-- 96 bytes in UTF-8 with single bit changes.

key30_0 = "Παντοτε χαιρετε!"
key30_1 = "Πάντοτε χαιρετε!"
key30_2 = "Παντοτε χαίρετε!"
key30_3 = "Πάντοτε χαίρετε!"
-- Always rejoice! 1 Thess. 5:16.

key6_0 = "aerate"
key6_1 = "berate"
key6_2 = "cerate"
key6_3 = "derate"

samples = 1048576

wring96_0 = keyedWring $ fromString key96_0
wring96_1 = keyedWring $ fromString key96_1
wring96_2 = keyedWring $ fromString key96_2
wring96_3 = keyedWring $ fromString key96_3
wring30_0 = keyedWring $ fromString key30_0
wring30_1 = keyedWring $ fromString key30_1
wring30_2 = keyedWring $ fromString key30_2
wring30_3 = keyedWring $ fromString key30_3
wring6_0 = keyedWring $ fromString key6_0
wring6_1 = keyedWring $ fromString key6_1
wring6_2 = keyedWring $ fromString key6_2
wring6_3 = keyedWring $ fromString key6_3

log2 :: Integral a => a -> Int
log2 0 = (-1)
log2 (-1) = (-1771476585)
log2 n = 1 + (log2 (n `div` 2))

thueMorse_ :: Int -> Integer
thueMorse_ 0 = 1
thueMorse_ n = (shift ((shift 1 nbits) - 1 - (thueMorse_ (n-1))) nbits)
               + (thueMorse_ (n-1)) where
  nbits = (shift 1 (n-1))

thueMorse :: (Bits a,Integral a) => Int -> a
-- Returns a Thue-Morse word of at least n bits ending in 1.
thueMorse n = fromIntegral (thueMorse_ ((log2 n)+1))

byteArray :: (Bits a,Integral a) => Int -> a -> UArray Int Word8
byteArray len n = listArray (0,(len-1)) bytes where
  bytes = map (\x -> (fromIntegral (n `shift` (-8*x)) .&. 255)) [0..(len-1)]

eightByteArray :: Word64 -> UArray Int Word8
eightByteArray = byteArray 8

makeListInt :: (Bits a,Integral a) => [Word8] -> a
makeListInt [] = 0
makeListInt (n:ns) = ((makeListInt ns) .<<. 8) .|. (fromIntegral n)

makeArrayInt :: (Bits a,Integral a) => UArray Int Word8 -> a
makeArrayInt = makeListInt . elems

diff1Related :: Wring -> Wring -> Word64 -> Word64
diff1Related w0 w1 pt = ct0 .^. ct1 where
  ct0 = makeArrayInt $ encrypt w0 $ eightByteArray pt
  ct1 = makeArrayInt $ encrypt w1 $ eightByteArray pt

diffRelated :: Wring -> Wring -> [Word64]
diffRelated w0 w1 = map ((diff1Related w0 w1) . ((thueMorse 64) *)) [0..]

plaintextHisto :: Histo
plaintextHisto = foldl' hCountBits (emptyHisto 64)
  (take samples (map ((thueMorse 64) *) [0..]))

relatedKeyHisto :: Wring -> Wring -> Histo
relatedKeyHisto w0 w1 = foldl' hCountBits (emptyHisto 64)
  (take samples (diffRelated w0 w1))

relatedKeyStat :: Wring -> Wring -> Double
relatedKeyStat w0 w1 = binomial (relatedKeyHisto w0 w1) samples

sixStats :: Wring -> Wring -> Wring -> Wring -> [Double]
sixStats w0 w1 w2 w3 = par s01 $ par s23 $ par s02 $ par s13 $ par s03 $
  [s01,s23,s02,s13,s03,s12] where
    s01 = relatedKeyStat w0 w1
    s23 = relatedKeyStat w2 w3
    s02 = relatedKeyStat w0 w2
    s13 = relatedKeyStat w1 w3
    s03 = relatedKeyStat w0 w3
    s12 = relatedKeyStat w1 w2

tellStat :: Double -> String
-- 0.635 and 1.456 are 1% tails at 64 degrees of freedom, divided by 64.
tellStat stat
  | stat < 0.635 = "Too smooth. They look like a low-discrepancy sequence."
  | stat < 1.456 = "The differences look random."
  | otherwise    = "Too much variation. Check for outliers."

relatedKey4 :: Wring -> Wring -> Wring -> Wring -> IO ()
relatedKey4 w0 w1 w2 w3 = do
  let sixS = sixStats w0 w1 w2 w3
  putStrLn (show sixS)
  putStrLn ("0,1: " ++ tellStat (sixS !! 0))
  putStrLn ("2,3: " ++ tellStat (sixS !! 1))
  putStrLn ("0,2: " ++ tellStat (sixS !! 2))
  putStrLn ("1,3: " ++ tellStat (sixS !! 3))
  putStrLn ("0,3: " ++ tellStat (sixS !! 4))
  putStrLn ("1,2: " ++ tellStat (sixS !! 5))

relatedKey :: IO ()
relatedKey = do
  putStrLn "96-byte key, 8-byte data:"
  relatedKey4 wring96_0 wring96_1 wring96_2 wring96_3
  putStrLn "30-byte key, 8-byte data:"
  relatedKey4 wring30_0 wring30_1 wring30_2 wring30_3
  putStrLn "6-byte key, 8-byte data:"
  relatedKey4 wring6_0 wring6_1 wring6_2 wring6_3
