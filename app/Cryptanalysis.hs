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
  , match
  , rot1Bit
  , bias
  , convolve
  , unbiasedConvolve
  , convolveDiff
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
  , sum1Wring
  , relatedKeyHisto
  , plaintextHisto
  , sixStats
  , relatedKey
  , integralHisto
  , eightStats
  , integralCr
  ) where

import Data.Word
import Data.Bits
import Math.NumberTheory.Primes
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
import qualified Data.Vector.Unboxed as V

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

samples = 16384 -- 16777216

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

diff1Related :: Wring -> Wring -> Word64 -> Word64
diff1Related w0 w1 pt = ct0 .^. ct1 where
  ct0 = makeArrayInt $ encrypt w0 $ eightByteArray pt
  ct1 = makeArrayInt $ encrypt w1 $ eightByteArray pt

conDiff1Related :: Wring -> Wring -> Word64 -> Double
conDiff1Related w0 w1 pt = convolveDiff ct0 ct1 where
  ct0 = V.toList $ encrypt w0 $ eightByteArray pt
  ct1 = V.toList $ encrypt w1 $ eightByteArray pt

diffRelated :: Wring -> Wring -> [Word64]
diffRelated w0 w1 = map ((diff1Related w0 w1) . ((priminal 64) *)) [0..]

plaintextHisto :: Histo
plaintextHisto = foldl' hCountBits (emptyHisto 64)
  (take (div samples 2) (map ((priminal 64) *) [0..]))

relatedKeyHisto :: Wring -> Wring -> Histo
relatedKeyHisto w0 w1 = foldl' hCountBits (emptyHisto 64)
  (take (div samples 2) (diffRelated w0 w1))

relatedKeyStat :: Wring -> Wring -> Double
relatedKeyStat w0 w1 = binomial (relatedKeyHisto w0 w1) (div samples 2)

sixStats :: Wring -> Wring -> Wring -> Wring -> [Double]
sixStats w0 w1 w2 w3 = par s01 $ par s23 $ par s02 $ par s13 $ par s03 $
  [s01,s23,s02,s13,s03,s12] where
    s01 = relatedKeyStat w0 w1
    s23 = relatedKeyStat w2 w3
    s02 = relatedKeyStat w0 w2
    s13 = relatedKeyStat w1 w3
    s03 = relatedKeyStat w0 w3
    s12 = relatedKeyStat w1 w2

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

relatedKey4 :: Wring -> Wring -> Wring -> Wring -> IO ()
relatedKey4 w0 w1 w2 w3 = do
  let sixS = sixStats w0 w1 w2 w3
  putStrLn (show sixS)
  putStrLn ("0,1: " ++ tellStat64 (sixS !! 0))
  putStrLn ("2,3: " ++ tellStat64 (sixS !! 1))
  putStrLn ("0,2: " ++ tellStat64 (sixS !! 2))
  putStrLn ("1,3: " ++ tellStat64 (sixS !! 3))
  putStrLn ("0,3: " ++ tellStat64 (sixS !! 4))
  putStrLn ("1,2: " ++ tellStat64 (sixS !! 5))

relatedKey :: IO ()
relatedKey = do
  putStrLn "96-byte key, 8-byte data:"
  relatedKey4 wring96_0 wring96_1 wring96_2 wring96_3
  putStrLn "30-byte key, 8-byte data:"
  relatedKey4 wring30_0 wring30_1 wring30_2 wring30_3
  putStrLn "6-byte key, 8-byte data:"
  relatedKey4 wring6_0 wring6_1 wring6_2 wring6_3

-- Integral cryptanalysis
-- Take one key, and each of the eight bytes of the plaintext in parallel,
-- and xor all the ciphertexts produced by changing the byte of the plaintext
-- to all 256 possibilities.

sum1Wring :: Wring -> Word64 -> Int -> Word64
-- Take pt with all values in the bth byte, encrypt them all,
-- and xor the ciphertexts.
sum1Wring w pt b = foldl' xor 0 cts where
  pts = map eightByteArray $ zipWith xor (repeat pt) (map (.<<. (b .<<. 3)) [0..255])
  cts = map makeArrayInt $ map (encrypt w) pts

integralHisto :: Wring -> Int -> Histo
integralHisto w b = foldl' hCountBits (emptyHisto 64)
  (take (div samples 256) $
  map ((\pt -> sum1Wring w pt b) . ((priminal 64) *)) [0..])

integralStat :: Wring -> Int -> Double
integralStat w b = binomial (integralHisto w b) (div samples 256)

eightStats :: Wring -> [Double]
eightStats w = par s0 $ par s1 $ par s2 $ par s3 $ par s4 $ par s5 $ par s6 $
  [s0,s1,s2,s3,s4,s5,s6,s7] where
    s0 = integralStat w 0
    s1 = integralStat w 1
    s2 = integralStat w 2
    s3 = integralStat w 3
    s4 = integralStat w 4
    s5 = integralStat w 5
    s6 = integralStat w 6
    s7 = integralStat w 7

integral1 :: Wring -> IO ()
integral1 w = do
  let eightS = eightStats w
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
  integral1 wring96_0
  putStrLn "30-byte key, 8-byte data:"
  integral1 wring30_0
  putStrLn "6-byte key, 8-byte data:"
  integral1 wring6_0
  putStrLn "Linear key, 8-byte data:"
  integral1 linearWring
  putStrLn "96-byte key, byte 1:" -- These two bytes came out as
  putStrLn $ show $ integralHisto wring96_0 1
  putStrLn "96-byte key, byte 7:" -- "too much variation".
  putStrLn $ show $ integralHisto wring96_0 7

