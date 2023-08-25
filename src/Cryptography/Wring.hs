module Cryptography.Wring
  ( Wring
  , linearWring
  , keyedWring
  , encrypt
  , decrypt
  ) where

{- This cipher is intended to be used with short random keys (32 bytes or less,
 - no hard limit) or long human-readable keys (up to 96 bytes). keyedWring
 - takes arbitrarily long keys, but do not use keys longer than 96 bytes as they
 - make the cipher more vulnerable to related-key attacks.
 -}

import Cryptography.WringTwistree.Mix3
import Cryptography.WringTwistree.RotBitcount
import Cryptography.WringTwistree.Sboxes
import Data.Word
import Data.Bits
import Data.Array.Unboxed
import Data.Foldable (foldl')
import qualified Data.ByteString as B

data Wring = Wring
  { sbox    :: UArray (Word8,Word8) Word8
  , invSbox :: UArray (Word8,Word8) Word8
  } deriving Show

nRounds :: Integral a => a -> a
nRounds len
  | len < 3 = 3
  | otherwise = (nRounds (len `div` 3)) +1

xorn :: (Integral a,Bits a) => a -> a
xorn 0 = 0
xorn (-1) = error "xorn: negative"
xorn a = (a .&. 255) `xor` (xorn (a `shiftR` 8))

linearWring = Wring linearSbox linearInvSbox

-- | Creates a Wring with the given key.
-- To convert a String to a ByteString, put "- utf8-string" in your
-- package.yaml dependencies, import Data.ByteString.UTF8, and use
-- fromString.
keyedWring :: B.ByteString -> Wring
keyedWring key = Wring sbox (invert sbox) where
  sbox = sboxes key

{- A round of encryption consists of four steps:
 - mix3Parts
 - sboxes
 - rotBitcount
 - add byte index xor round number
 -}

cycle3 :: [Word8]
cycle3 = 0 : 1 : 2 : cycle3

roundEncrypt :: (Ix a,Integral a,Bits a) => 
  a -> UArray (Word8,Word8) Word8 -> UArray a Word8 -> a -> UArray a Word8
roundEncrypt rprime sbox buf rond = i4 where
  bnd = bounds buf
  i1 = mix3Parts buf (fromIntegral rprime)
  i2 = listArray bnd $ map (sbox !) $ zip (drop (fromIntegral rond) cycle3) (elems i1)
  i3 = rotBitcount i2 1
  i4 = listArray bnd $ zipWith (+) (elems i3)
       (map (fromIntegral . xorn . (xor rond)) [0..snd bnd])

roundDecrypt :: (Ix a,Integral a,Bits a) => 
  a -> UArray (Word8,Word8) Word8 -> UArray a Word8 -> a -> UArray a Word8
roundDecrypt rprime sbox buf rond = i4 where
  bnd = bounds buf
  i1 = listArray bnd $ zipWith (-) (elems buf)
       (map (fromIntegral . xorn . (xor rond)) [0..snd bnd])
  i2 = rotBitcount i1 (-1)
  i3 = listArray bnd $ map (sbox !) $ zip (drop (fromIntegral rond) cycle3) (elems i2)
  i4 = mix3Parts i3 (fromIntegral rprime)

encrypt :: (Ix a,Integral a,Bits a) => Wring -> UArray a Word8 -> UArray a Word8
encrypt wring buf = foldl' (roundEncrypt rprime (sbox wring)) buf rounds
  where
    len = fromIntegral $ snd (bounds buf) +1
    rprime = fromIntegral $ findMaxOrder (len `div` 3)
    rounds = [0 .. (fromIntegral (nRounds len) -1)]

decrypt :: (Ix a,Integral a,Bits a) => Wring -> UArray a Word8 -> UArray a Word8
decrypt wring buf = foldl' (roundDecrypt rprime (invSbox wring)) buf rounds
  where
    len = fromIntegral $ snd (bounds buf) +1
    rprime = fromIntegral $ findMaxOrder (len `div` 3)
    rounds = reverse [0 .. (fromIntegral (nRounds len) -1)]
