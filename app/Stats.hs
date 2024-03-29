{-# LANGUAGE BangPatterns #-}
module Stats
  ( pairwiseSumCount
  , Histo (..)
  , emptyHisto
  , hCount
  , hCountBits
  , χ²
  , binomial
  , sacCountBit
  , bitFold
  , sacStats
  , normμσ
  , jhCount
  , emptyJiggleHisto
  , jiggleHisto
  , listStats
  , jiggleStats
  ) where

import Data.Bits
import Data.Word
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))
import qualified Data.IntMap.Strict as Map
import Data.Foldable (toList,foldl')
import Control.Parallel
import Control.Parallel.Strategies

addTriple :: Num a => (Int,a,a) -> (Int,a,a) -> (Int,a,a)
addTriple (!b,!c,!d) (e,f,g) = (b+e,c+f,d+g)

sumPairs :: Num a => [(Int,a,a)] -> [(Int,a,a)]
sumPairs [] = []
sumPairs [x] = [x]
sumPairs (x:y:xs) = pseq (addTriple x y) $ ((addTriple x y) : sumPairs xs)

pairwiseSumCount :: (Num a) => [(Int,a,a)] -> (Int,a,a)
pairwiseSumCount [] = (0,0,0)
pairwiseSumCount [x] = x
pairwiseSumCount xs = pairwiseSumCount (sumPairs xs)

newtype Histo = Histo (Seq.Seq Word) deriving (Show)

emptyHisto :: Int -> Histo
emptyHisto n = Histo (Seq.replicate n 0)

hCount :: Integral a => Histo -> a -> Histo
hCount (Histo h) n = Histo (Seq.adjust' (+1) (fromIntegral n) h)

listBits :: Word64 -> [Int]
listBits 0 = []
listBits n = b:listBits (clearBit n b) where
  b = countTrailingZeros n

hCountBits :: Histo -> Word64 -> Histo
hCountBits h n = foldl' hCount h (listBits n)

χ² :: Histo -> Double
χ² (Histo h) = sum $ map (\x -> ((fromIntegral x) - mean) ^(2::Int) / mean) (toList h)
  where mean = fromIntegral (sum h) / fromIntegral (length h) :: Double

-- Use this if the histo bars count one-bits and half of the n trials should
-- yield one.
-- Each bar is a binomial random variable with p=q=1/2. The mean is half the
-- number of trials, and the variance is half the mean. Returns what should be
-- a χ² random number with (length h) degrees of freedom, divided by (length h).
binomial :: Histo -> Int -> Double
binomial (Histo h) n = (sum $ map
  (\x -> ((fromIntegral x) - mean) ^(2::Int) / sdev) (toList h))
  / fromIntegral (length h)
  where
    mean = fromIntegral n / 2 :: Double
    sdev = mean/2

squareWave :: Int -> [Int]
squareWave n = map ((.&. (1::Int)) . (.>>. n)) [0..]

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
        h = foldl' hCount (emptyHisto (1 .<<. wid)) bf

sacHistos :: (Integral a,Bits a) => [a] -> Int -> [Histo]
sacHistos xs wid = sacHistos' xs wid 0

sacStats :: (Integral a,FiniteBits a) => [a] -> [[Int]]
-- Takes a list of Word8, Word16, or the like, whose length is a power of 2,
-- and computes the deviations from the strict avalanche criterion.
sacStats [] = []
sacStats (x:xs) = parMap rpar (\h -> sacRow h wid) (sacHistos (x:xs) wid)
  where wid=finiteBitSize (x)

normμσ :: Double -> Double -> [Double] -> (Double,Double)
-- Takes a list of numbers, assumed to have the specified mean and standard
-- deviation, and returns the actual mean, as an offset from the specified
-- mean in specified standard deviations, and the average square deviation,
-- in specified variances. The result should be near (0,1).
normμσ μ σ devs = (mean,var) where
  ndevs = map (\x -> (x-μ)/σ) devs
  ndevsqs = map (\x -> (1,x,x^2)) ndevs
  (n,total,total2) = pairwiseSumCount ndevsqs
  mean = total / (fromIntegral n)
  var = total2 / (fromIntegral n)

type JiggleHisto = Map.IntMap [Int]
-- The key is sideways; the value is the list of matches from jiggleMatch.

jhCount :: JiggleHisto -> (Int,Int) -> JiggleHisto
jhCount jh (sideways,matches) = Map.insertWith (++) sideways [matches] jh

emptyJiggleHisto :: JiggleHisto
emptyJiggleHisto = Map.empty

jiggleHisto :: [(Int,Int)] -> JiggleHisto
jiggleHisto sidematches = foldl' jhCount emptyJiggleHisto sidematches

listStats :: (Real a,Bounded a) => [a] -> (Int,Double,Double,a,a)
listStats xs = (n,mean,sqrt var,minxs,maxxs) where
  minxs = if null xs then maxBound else minimum xs
  maxxs = if null xs then minBound else maximum xs
  devs = map realToFrac xs
  devsqs = map (\x -> (1,x,x^2)) devs
  (n,total,total2) = pairwiseSumCount devsqs
  mean = total / (fromIntegral n)
  var = (total2 - total * mean) / (fromIntegral n-1)

jiggleStats :: JiggleHisto -> [(Int,(Int,Double,Double,Int,Int))]
jiggleStats jh = Map.toList $ Map.map listStats jh
