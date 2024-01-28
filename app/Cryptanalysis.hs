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
  , sbox30_3
  , sbox96_0
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
  , same30_3
  , same96_0
  , match
  , rot1Bit
  , bias
  , convolve
  , unbiasedConvolve
  , convolveDiff
  , thueMorse
  , priminal
  , byteArray
  , b125
  , b250
  , b375
  , b500
  , b625
  , b750
  , b875
  , varConvolveDiff
  , diff1Related
  , conDiff1Related
  , diffRelated
  , conDiffRelated
  , sum1Wring
  , relatedKeyHisto
  , plaintextHisto
  , sixStatsBit
  , relatedKey
  , integralHisto
  , eightStats
  , integralCr
  , integralCrFixed
  , compressBoth
  , sixtyFourNybbleArray
  , changeEachNybble
  , compressChanges
  , collisions1
  , hashColl
  , hashCollLinear
  , cumRot
  , rotations1
  , rotations256
  , clutch
  ) where

import Data.Word
import Data.Bits
import Data.List (sort,sortOn,group,inits,transpose)
import Math.NumberTheory.Primes
import Data.Array.Unboxed
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))
import Data.Foldable (toList,foldl')
import Control.DeepSeq
import Control.Parallel
import Control.Parallel.Strategies
import GHC.Conc (numCapabilities)
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (fromString)
import Debug.Trace
import Cryptography.Wring
import Cryptography.Twistree
import Stats
import qualified Data.Vector.Unboxed as V

type Crypt = Wring -> V.Vector Word8 -> V.Vector Word8

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

sbox30_3 = sboxes (fromString key30_3)
sbox96_0 = sboxes (fromString key96_0)
same30_3 = sameBitcount sbox30_3
same96_0 = sameBitcount sbox96_0
-- These two are the longest, 17, of the sameBitcount of the S-boxes made from
-- the above keys. sameBitcount linearSbox is all 256 bytes.
-- The bitcounts of the S-box entries are:
-- same30_3: [3,4,4,2,3,4,4,4,4,4,4,3,4,4,6,4,5]
-- same96_0: [2,3,3,3,5,5,4,5,3,5,4,3,4,3,5,4,3]
-- Using 16 of the 17 bytes, drop the 5 from same30_3 (and risk the 6)
-- or drop the 2 from same96_0.

samples = 16777216
chunkSize = 1 + div samples (2 * numCapabilities)
smallChunkSize = 1 + div samples (961 * numCapabilities)

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

-- Because of rotBitcount, fixed-position bit differences are inadequate to
-- telling whether two outputs of Wring are similar. Convolve them instead.

match :: [Word8] -> [Word8] -> Int
match as bs = sum $ zipWith (\a b -> 4 - popCount (a .^. b)) as bs

rot1Bit' :: Word8 -> [Word8] -> [Word8]
rot1Bit' b [a] = [(a .>>. 1) .|. (b .<<. 7)]
rot1Bit' b (a0:a1:as) = ((a0 .>>. 1) .|. (a1 .<<. 7)) : (rot1Bit' b (a1:as))

rot1Bit :: [Word8] -> [Word8]
rot1Bit [] = []
rot1Bit (a:as) = rot1Bit' a (a:as)

-- Ranges from -1 to 1, with 0 meaning half the bits are ones.
bias :: [Word8] -> Double
bias as = 1 - ((fromIntegral $ sum $ map popCount as) / (4 * fromIntegral (length as)))

convolve :: [Word8] -> [Word8] -> [Int]
-- The lists should be equally long. Returns a list 8 times as long.
convolve as bs = take nBits $ zipWith match (repeat as) (iterate rot1Bit bs) where
  nBits = 8 * length bs

unbiasedConvolve :: [Word8] -> [Word8] -> [Double]
unbiasedConvolve as bs = map ((+(- prodBias)) . (/ halfBits) . fromIntegral)
  (convolve as bs) where
    prodBias = (bias as) * (bias bs)
    halfBits = fromIntegral (4 * (length as))

-- The numbers returned by convolveDiff appear to have mean 1 and variance
-- 0.0078125, i.e. 2/256 where 256 is the number of bits.
convolveDiff :: [Word8] -> [Word8] -> Double
convolveDiff as bs = if (scale == 0) then 1 else
  (sum $ map (^2) $ unbiasedConvolve as bs) / scale where
  scale = ((1+(bias as))*(1+(bias bs))*(1-(bias as))*(1-(bias bs)))

-- Thue-Morse word

log2 :: Integral a => a -> Int
log2 0 = (-1)
log2 (-1) = (-1771476585)
log2 n = 1 + (log2 (n `div` 2))

thueMorse_ :: Int -> Integer
thueMorse_ 0 = 1
thueMorse_ n = (((1 .<<. nbits) - 1 - (thueMorse_ (n-1))) .<<. nbits)
               + (thueMorse_ (n-1)) where
  nbits = (1 .<<. (n-1))

thueMorse :: Integral a => Int -> a
-- Returns a Thue-Morse word of at least n bits ending in 1.
thueMorse n = fromIntegral (thueMorse_ ((log2 n)+1))

-- Multiples of the priminal word for spreading plaintexts around the
-- space of plaintext

halfPrimes :: Int -> [Int]
halfPrimes n = takeWhile (< n) (map ((`div` 2) . (+(-3)) . unPrime) [nextPrime 3 ..])

priminal_ :: Int -> Integer
priminal_ n = sum $ map (1 .<<.) (halfPrimes n)

priminal :: Integral a => Int -> a
-- Returns a priminal word of at least n bits ending in 1.
priminal n = fromIntegral (priminal_ n)

byteArray :: (Bits a,Integral a) => Int -> a -> V.Vector Word8
byteArray len n = V.fromListN len bytes where
  bytes = map (\x -> (fromIntegral (n .>>. (8*x)) .&. 255)) [0..(len-1)]

eightByteArray :: Word64 -> V.Vector Word8
eightByteArray = byteArray 8

makeListInt :: (Bits a,Integral a) => [Word8] -> a
makeListInt [] = 0
makeListInt (n:ns) = ((makeListInt ns) .<<. 8) .|. (fromIntegral n)

makeArrayInt :: (Bits a,Integral a) => V.Vector Word8 -> a
makeArrayInt = makeListInt . V.toList

-- Random-looking bit vectors with proportions of bits set,
-- for testing convolveDiff on biased bit vectons

land :: [Word8] -> [Word8] -> [Word8]
land as bs = zipWith (.&.) as bs

lor :: [Word8] -> [Word8] -> [Word8]
lor as bs = zipWith (.|.) as bs

b125 :: Integer -> [Word8]
b125 n = land a (land b c) where
  a = V.toList $ encrypt wring6_0 $ byteArray 32 n
  b = V.toList $ encrypt wring30_0 $ byteArray 32 n
  c = V.toList $ encrypt wring96_0 $ byteArray 32 n

b250 :: Integer -> [Word8]
b250 n = land b c where
  b = V.toList $ encrypt wring30_0 $ byteArray 32 n
  c = V.toList $ encrypt wring96_0 $ byteArray 32 n

b375 :: Integer -> [Word8]
b375 n = land a (lor b c) where
  a = V.toList $ encrypt wring6_0 $ byteArray 32 n
  b = V.toList $ encrypt wring30_0 $ byteArray 32 n
  c = V.toList $ encrypt wring96_0 $ byteArray 32 n

b500 :: Integer -> [Word8]
b500 n = a where
  a = V.toList $ encrypt wring6_0 $ byteArray 32 n

b625 :: Integer -> [Word8]
b625 n = lor a (land b c) where
  a = V.toList $ encrypt wring6_0 $ byteArray 32 n
  b = V.toList $ encrypt wring30_0 $ byteArray 32 n
  c = V.toList $ encrypt wring96_0 $ byteArray 32 n

b750 :: Integer -> [Word8]
b750 n = lor b c where
  b = V.toList $ encrypt wring30_0 $ byteArray 32 n
  c = V.toList $ encrypt wring96_0 $ byteArray 32 n

b875 :: Integer -> [Word8]
b875 n = lor a (lor b c) where
  a = V.toList $ encrypt wring6_0 $ byteArray 32 n
  b = V.toList $ encrypt wring30_0 $ byteArray 32 n
  c = V.toList $ encrypt wring96_0 $ byteArray 32 n

varConvolveDiff :: (Integer -> [Word8]) -> Double
-- Assumes the mean of convolveDiff is 1
varConvolveDiff b = sum (map (\x -> (x-1)^2) sims) / 4096 where
  sims = map (\n -> convolveDiff (b (2*n)) (b (2*n+1))) [0..4095]

-- Exact bitcount:
-- b125 [21,27,48,58,61,74,82,87,91,104,112,120,142,153,156,175,180,210,213,214,251,254]
-- b250 [10,12,14,17,19,35,37,49,54,96,123,137,144,145,152,155,157,181,185,192,227,239,250]
-- b375 [15,16,63,75,79,94,99,113,141,181,186,215,249,251]
-- b500 [66,184,186,190,191,207,225,228,234,235]
-- b625 [34,45,51,53,62,77,109,164,207,249,250]
-- b750 [8,18,77,84,87,109,130,147,152,163,172,189,232,246]
-- b875 [6,13,39,45,48,52,66,71,94,95,106,108,111,122,133,161,166,173,200,203,221,228,251,254]

-- Related-key cryptanalysis
-- Take four keys which differ by one or two bytes, in pairs, and encrypt
-- the same plaintext with both and compute the difference.

-- diffRelated produces a list of integers, which are then histogrammed,
-- and the histo bars are turned into a chi-squared random variable.
-- conDiffRelated produces a list of chi-squared random variables, whose
-- mean and variance are then checked against what's expected. In both
-- cases, the chi-squared variables are scaled to have a mean of 1.

name2 :: Wring -> String -> Wring -> String
name2 w0 s w1 = (wringName w0) ++ s ++ (wringName w1)

diff1Related :: Wring -> Wring -> Word64 -> Word64
diff1Related w0 w1 pt = ct0 .^. ct1 where
  ct0 = makeArrayInt $ encrypt w0 $ eightByteArray pt
  ct1 = makeArrayInt $ encrypt w1 $ eightByteArray pt

conDiff1Related :: Wring -> Wring -> Word64 -> Double
conDiff1Related w0 w1 pt = par ct0 $ convolveDiff ct0 ct1 where
  ct0 = V.toList $ encrypt w0 $ eightByteArray pt
  ct1 = V.toList $ encrypt w1 $ eightByteArray pt

diffRelated :: Wring -> Wring -> [Word64]
diffRelated w0 w1 = traceEvent (name2 w0 "-diff-" w1) $ map ((diff1Related w0 w1) . ((priminal 64) *)) [0..]

conDiffRelated :: Wring -> Wring -> [Double]
conDiffRelated w0 w1 = traceEvent (name2 w0 "-con-" w1) $ map ((conDiff1Related w0 w1) . ((priminal 64) *)) [0..]

plaintextHisto :: Histo
plaintextHisto = foldl' hCountBits (emptyHisto 64)
  (take (div samples 2) (map ((priminal 64) *) [0..]))

relatedKeyHisto :: Wring -> Wring -> Histo
relatedKeyHisto w0 w1 = foldl' hCountBits (emptyHisto 64)
  (take (div samples 2) (diffRelated w0 w1) `using` parListDeal numCapabilities rdeepseq)

relatedKeyStatBit :: Wring -> Wring -> Double
relatedKeyStatBit w0 w1 = binomial (relatedKeyHisto w0 w1) (div samples 2)

relatedKeyStatConv :: Wring -> Wring -> (Double,Double)
relatedKeyStatConv w0 w1 = normμσ 1 (sqrt (1/32))
  (take (div samples 2) (conDiffRelated w0 w1) `using` parListDeal numCapabilities rdeepseq)

sixStatsBit :: Wring -> Wring -> Wring -> Wring -> [Double]
sixStatsBit w0 w1 w2 w3 = par s01 $ par s23 $ par s02 $ par s13 $ par s03 $ par s12 $
  [s01,s23,s02,s13,s03,s12] where
    s01 = relatedKeyStatBit w0 w1
    s23 = relatedKeyStatBit w2 w3
    s02 = relatedKeyStatBit w0 w2
    s13 = relatedKeyStatBit w1 w3
    s03 = relatedKeyStatBit w0 w3
    s12 = relatedKeyStatBit w1 w2

sixStatsConv :: Wring -> Wring -> Wring -> Wring -> [(Double,Double)]
sixStatsConv w0 w1 w2 w3 =
  par (force s01) $ par (force s23) $ par (force s02) $
  par (force s13) $ par (force s03) $ par (force s12) $
  [s01,s23,s02,s13,s03,s12] where
    s01 = relatedKeyStatConv w0 w1
    s23 = relatedKeyStatConv w2 w3
    s02 = relatedKeyStatConv w0 w2
    s13 = relatedKeyStatConv w1 w3
    s03 = relatedKeyStatConv w0 w3
    s12 = relatedKeyStatConv w1 w2

tellStat64 :: Double -> String
-- 0.635 and 1.456 are 1% tails at 64 degrees of freedom, divided by 64.
tellStat64 stat
  | stat < 0.635 = "Too smooth. They look like a low-discrepancy sequence."
  | stat < 1.456 = "The differences look random."
  | otherwise    = "Too much variation. Check for outliers."

tellStat256 :: Double -> String
-- 0.806 and 1.217 are 1% tails at 256 degrees of freedom, divided by 256.
tellStat256 stat
  | stat < 0.806 = "Too smooth. They look like a low-discrepancy sequence."
  | stat < 1.217 = "The differences look random."
  | otherwise    = "Too much variation. Check for outliers."

tellStatμσ :: (Double,Double) -> String
tellStatμσ (mean,var)
  | mean < -0.5 = "The bit patterns are close to periodic."
  | mean > 0.5  = "The bit patterns are too similar."
  | var  > 2    = "The variance is too high."
  | var  < 0.5  = "The variance is too low."
  | otherwise   = "The differences look random."

relatedKey4Bit :: Wring -> Wring -> Wring -> Wring -> IO ()
relatedKey4Bit w0 w1 w2 w3 = do
  let sixS = sixStatsBit w0 w1 w2 w3
  putStrLn (show sixS)
  putStrLn ("0,1: " ++ tellStat64 (sixS !! 0))
  putStrLn ("2,3: " ++ tellStat64 (sixS !! 1))
  putStrLn ("0,2: " ++ tellStat64 (sixS !! 2))
  putStrLn ("1,3: " ++ tellStat64 (sixS !! 3))
  putStrLn ("0,3: " ++ tellStat64 (sixS !! 4))
  putStrLn ("1,2: " ++ tellStat64 (sixS !! 5))

relatedKey4Conv :: Wring -> Wring -> Wring -> Wring -> IO ()
relatedKey4Conv w0 w1 w2 w3 = do
  let sixS = sixStatsConv w0 w1 w2 w3
  putStrLn (show sixS)
  putStrLn ("0,1: " ++ tellStatμσ (sixS !! 0))
  putStrLn ("2,3: " ++ tellStatμσ (sixS !! 1))
  putStrLn ("0,2: " ++ tellStatμσ (sixS !! 2))
  putStrLn ("1,3: " ++ tellStatμσ (sixS !! 3))
  putStrLn ("0,3: " ++ tellStatμσ (sixS !! 4))
  putStrLn ("1,2: " ++ tellStatμσ (sixS !! 5))

relatedKey :: IO ()
relatedKey = do
  traceMarkerIO "96bit"
  putStrLn "96-byte key, 8-byte data, bitwise differences:"
  relatedKey4Bit wring96_0 wring96_1 wring96_2 wring96_3
  traceMarkerIO "96conv"
  putStrLn "96-byte key, 8-byte data, convolutional differences:"
  relatedKey4Conv wring96_0 wring96_1 wring96_2 wring96_3
  traceMarkerIO "30bit"
  putStrLn "30-byte key, 8-byte data, bitwise differences:"
  relatedKey4Bit wring30_0 wring30_1 wring30_2 wring30_3
  traceMarkerIO "30conv"
  putStrLn "30-byte key, 8-byte data, convolutional differences:"
  relatedKey4Conv wring30_0 wring30_1 wring30_2 wring30_3
  traceMarkerIO "6bit"
  putStrLn "6-byte key, 8-byte data, bitwise differences:"
  relatedKey4Bit wring6_0 wring6_1 wring6_2 wring6_3
  traceMarkerIO "6conv"
  putStrLn "6-byte key, 8-byte data, convolutional differences:"
  relatedKey4Conv wring6_0 wring6_1 wring6_2 wring6_3
  traceMarkerIO "relkey done"

-- Integral cryptanalysis
-- Take one key, and each of the eight bytes of the plaintext in parallel,
-- and xor all the ciphertexts produced by changing the byte of the plaintext
-- to all 256 possibilities.

sum1Wring :: Crypt -> Wring -> Word64 -> Int -> Word64
-- Take pt with all values in the bth byte, encrypt them all,
-- and xor the ciphertexts.
sum1Wring enc w pt b = foldl' xor 0 cts where
  pts = map eightByteArray $ zipWith xor (repeat pt) (map (.<<. (b .<<. 3)) [0..255])
  cts = map makeArrayInt $ map (enc w) pts

integralHisto :: Crypt -> Wring -> Int -> Histo
integralHisto enc w b = foldl' hCountBits (emptyHisto 64)
  (take (div samples 256)
  (map ((\pt -> sum1Wring enc w pt b) . ((priminal 64) *)) [0..])
  `using` parListDeal numCapabilities rdeepseq)

integralStat :: Crypt -> Wring -> Int -> Double
integralStat enc w b = binomial (integralHisto enc w b) (div samples 256)

eightStats :: Crypt -> Wring -> [Double]
eightStats enc w =
  par s0 $ par s1 $ par s2 $ par s3 $ par s4 $ par s5 $ par s6 $ par s7 $
  [s0,s1,s2,s3,s4,s5,s6,s7] where
    s0 = integralStat enc w 0
    s1 = integralStat enc w 1
    s2 = integralStat enc w 2
    s3 = integralStat enc w 3
    s4 = integralStat enc w 4
    s5 = integralStat enc w 5
    s6 = integralStat enc w 6
    s7 = integralStat enc w 7

integral1 :: Crypt -> Wring -> IO ()
integral1 enc w = do
  let eightS = eightStats enc w
  putStrLn (show eightS)
  putStrLn ("Byte 0: " ++ tellStat64 (eightS !! 0))
  putStrLn ("Byte 1: " ++ tellStat64 (eightS !! 1))
  putStrLn ("Byte 2: " ++ tellStat64 (eightS !! 2))
  putStrLn ("Byte 3: " ++ tellStat64 (eightS !! 3))
  putStrLn ("Byte 4: " ++ tellStat64 (eightS !! 4))
  putStrLn ("Byte 5: " ++ tellStat64 (eightS !! 5))
  putStrLn ("Byte 6: " ++ tellStat64 (eightS !! 6))
  putStrLn ("Byte 7: " ++ tellStat64 (eightS !! 7))

integralCr :: IO ()
integralCr = do
  putStrLn "96-byte key, 8-byte data:"
  integral1 encrypt wring96_0
  putStrLn "30-byte key, 8-byte data:"
  integral1 encrypt wring30_0
  putStrLn "6-byte key, 8-byte data:"
  integral1 encrypt wring6_0
  putStrLn "Linear key, 8-byte data:"
  integral1 encrypt linearWring
  putStrLn "96-byte key, byte 7:" -- This byte came out as "too much variation".
  putStrLn $ show $ integralHisto encrypt wring96_0 7
  putStrLn "Linear key, byte 3:" -- This byte came out as "low-discrepancy".
  putStrLn $ show $ integralHisto encrypt linearWring 3

integralCrFixed :: IO ()
integralCrFixed = do
  putStrLn "96-byte key, 8-byte data:"
  integral1 encryptFixed wring96_0
  putStrLn "30-byte key, 8-byte data:"
  integral1 encryptFixed wring30_0
  putStrLn "6-byte key, 8-byte data:"
  integral1 encryptFixed wring6_0
  putStrLn "Linear key, 8-byte data:"
  integral1 encryptFixed linearWring

-- Attempt to find a collision of the hash compression function

compressBoth :: SBox -> V.Vector Word8 -> V.Vector Word8
-- Takes a 64-byte block, compresses it as in both the 2-tree and the 3-tree,
-- and concatenates the results.
compressBoth sbox buf = (compress sbox buf 0) <> (compress sbox buf 1)

nybbleArray :: (Bits a,Integral a) => Int -> a -> V.Vector Word8
-- Like byteArray, but the nybbles are looked up in same30_3 to get the bytes,
-- which are then assembled into a vector.
nybbleArray len n = V.fromListN len bytes where
  bytes = map (\x -> same30_3 !! (fromIntegral (n .>>. (4*x)) .&. 15)) [0..(len-1)]

sixtyFourNybbleArray :: Integer -> V.Vector Word8
sixtyFourNybbleArray = nybbleArray 64

changeOneNybble :: Integer -> Int -> [Integer]
changeOneNybble n k = map (\x -> n .^. (x .<<. (4*k))) [1..15]

changeEachNybble :: Integer -> [Integer]
-- Returns a list of 961 numbers
changeEachNybble n = foldl' (++) [n] (map (changeOneNybble n) [0..63])

compressChanges :: SBox -> Integer -> [(V.Vector Word8,V.Vector Word8)]
compressChanges sbox n = map (\x -> (x,(compressBoth sbox x))) $
  map (\x -> mix3Parts (sixtyFourNybbleArray x) 11) (changeEachNybble n)
  -- 11 is rprime for a 64-byte block

dups :: [(V.Vector Word8,V.Vector Word8)] -> [(V.Vector Word8,V.Vector Word8)]
dups [] = []
dups [x] = []
dups ((a,h0):(b,h1):xs)
  | h0==h1    = (a,b):(dups ((b,h1):xs))
  | otherwise = dups ((b,h1):xs)

collisions1 :: SBox -> Integer -> [(V.Vector Word8,V.Vector Word8)]
-- Given an S-box and a 64-nybble integer, tries compressing all 961 blocks
-- made from the integer changed by 0 or 1 nybble, and returns any collisions.
collisions1 sbox n = dups $ sortOn snd $ compressChanges sbox n

collisions :: SBox -> [(V.Vector Word8,V.Vector Word8)]
collisions sbox = foldl' (++) []
  (take (div samples 961)
  (map ((collisions1 sbox) . ((priminal 256) *)) [0..])
  `using` parListDeal numCapabilities rdeepseq)

hashColl :: IO ()
hashColl = do
  putStrLn "Hash collisions, 30-byte key:"
  let colls = collisions sbox30_3
  putStrLn (show colls)
  putStrLn ((show (length colls)) ++ " collisions")

hashCollLinear :: IO ()
hashCollLinear = do
  putStrLn "Hash collisions, linear S-box:"
  let colls = collisions linearSbox
  putStrLn (show colls)
  putStrLn ((show (length colls)) ++ " collisions")

-- Clutch cryptanalysis: find out how often two messages are rotated together

clutchSamples = 256
clutchMsgLen = 10000--00
clutchRounds = 8
clutchParNum = 2 + numCapabilities `div` 256 -- this 256 is from rotations256

countPairs :: (Ord a, Eq a) => [a] -> Int
-- Given a list of numbers representing the amount by which a message has been
-- rotated by some number of rounds, returns the number of pairs of
-- equal numbers.
countPairs ns = sum $ map (\n -> n*(n-1)) $ map length $ group $ sort ns

cumRot :: [Int] -> [Int]
cumRot rotations = map (`mod` (8*clutchMsgLen)) $ map sum $ tail $ inits rotations

messageArray :: Integer -> V.Vector Word8
messageArray pt = byteArray clutchMsgLen pt

rotations1 :: Wring -> Integer -> [Int]
rotations1 wring pt = cumRot $ snd $ encryptN wring clutchRounds $ messageArray pt

rotations256 :: Wring -> Integer -> Int -> [[Int]]
rotations256 wring pt n = (map (rotations1 wring) $ map (xor pt) $
  map (xor pt) $ map (.<<. (8*n)) [0..255])
  `using` parListDeal numCapabilities rdeepseq

clutch1 :: Fractional a => Wring -> (Integer,Int) -> ([a],[a])
clutch1 wring (pt,n) = (totalRotStats,togetherRotStats) where
  rotations = trace "." $ rotations256 wring pt n
  rotTogether = map (tail . inits) rotations
  totalRotStats = map (/(256*255)) $ map (fromIntegral . countPairs) $ transpose rotations
  togetherRotStats = map (/(256*255)) $ map (fromIntegral . countPairs) $ transpose rotTogether

addClutch :: Num a => ([a],[a]) -> ([a],[a]) -> ([a],[a])
addClutch (a,b) (m,n) = (zipWith (+) a m,zipWith (+) b n)

divClutch :: Fractional a => ([a],[a]) -> ([a],[a])
divClutch (a,b) = (map (/(fromIntegral clutchSamples)) a,
		   map (/(fromIntegral clutchSamples)) b)

clutch :: Fractional a => Wring -> ([a],[a])
clutch wring = divClutch $ foldl' addClutch (repeat 0,repeat 0) $
  map (clutch1 wring) $
  map (\x -> ((priminal (8*clutchMsgLen))^2*(fromIntegral x),
	      (x*(fromIntegral (findMaxOrder (fromIntegral clutchMsgLen))) `mod` clutchMsgLen)))
  ([1..clutchSamples] `using` parListDeal clutchParNum rdeepseq)
