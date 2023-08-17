module Stats
  ( Histo (..)
  , emptyHisto
  , hCount
  , χ²
  , sacCountBit
  , bitFold
  , sacStats
  ) where

import Data.Bits
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))
import Data.Foldable (toList,foldl')
import Control.Parallel.Strategies

newtype Histo = Histo (Seq.Seq Word) deriving (Show)

emptyHisto :: Int -> Histo
emptyHisto n = Histo (Seq.replicate n 0)

hCount :: Integral a => Histo -> a -> Histo
hCount (Histo h) n = Histo (Seq.adjust' (+1) (fromIntegral n) h)

χ² :: Histo -> Double
χ² (Histo h) = sum $ map (\x -> ((fromIntegral x) - mean) ^(2::Int) / mean) (toList h)
  where mean = fromIntegral (sum h) / fromIntegral (length h) :: Double

squareWave :: Int -> [Int]
squareWave n = map ((.&. (1::Int)) . (`shift` (-n))) [0..]

squareWaveInt :: Int -> [Int]
squareWaveInt n = map ((-1) ^) (squareWave n)

squareWaveBool :: Int -> [Bool]
squareWaveBool n = map (== 1) (squareWave n)

sacCountBit :: Histo -> Int -> Int
-- The histogram is produced by a strict avalanche criterion count; i.e. each
-- number counted is the xor of two outputs whose inputs differ by one bit.
sacCountBit (Histo h) n = sum $ zipWith (*) (squareWaveInt n) (map fromIntegral (toList h))

bitFoldSq :: Bits a => [a] -> [Bool] -> Seq.Seq a -> [a]
bitFoldSq [] _ _ = []
bitFoldSq _ [] _ = []
bitFoldSq (x:xs) (False:bs) as = bitFoldSq xs bs (as |> x)
bitFoldSq (x:xs) (True:bs) Seq.Empty = x:bitFoldSq xs bs Seq.Empty -- should never happen
bitFoldSq (x:xs) (True:bs) (a:<|as) = (xor x a):bitFoldSq xs bs as

bitFold :: Bits a => [a] -> Int -> [a]
bitFold xs n = bitFoldSq xs (squareWaveBool n) Seq.Empty

sacRow :: Histo -> Int -> [Int]
sacRow h nbits = parMap rpar (sacCountBit h) [0..(nbits-1)]

sacHistos' :: (Integral a,Bits a) => [a] -> Int -> Int -> [Histo]
sacHistos' xs wid b
  | null bf   = []
  | otherwise = h:(sacHistos' xs wid (b+1))
  where bf = (bitFold xs b)
        h = foldl' hCount (emptyHisto (shift 1 wid)) bf

sacHistos :: (Integral a,Bits a) => [a] -> Int -> [Histo]
sacHistos xs wid = sacHistos' xs wid 0

sacStats :: (Integral a,FiniteBits a) => [a] -> [[Int]]
-- Takes a list of Word8, Word16, or the like, whose length is a power of 2,
-- and computes the deviations from the strict avalanche criterion.
sacStats [] = []
sacStats (x:xs) = parMap rpar (\h -> sacRow h wid) (sacHistos (x:xs) wid)
  where wid=finiteBitSize (x)
