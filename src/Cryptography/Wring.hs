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
 -
 - The Haskell implementation needs four times as much RAM as the message size,
 - plus a constant overhead.
 -}

import Cryptography.WringTwistree.Mix3
import Cryptography.WringTwistree.RotBitcount
import Cryptography.WringTwistree.Sboxes
import Data.Word
import Data.Bits
import Data.Foldable (foldl')
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.ST
import Control.Monad

data Wring = Wring
  { sbox    :: SBox
  , invSbox :: SBox
  } deriving Show

nRounds :: Integral a => a -> a
nRounds len
  | len < 3 = 3
  | otherwise = (nRounds (len `div` 3)) +1

xorn :: (Integral a,Bits a) => a -> Word8
xorn 0 = 0
xorn (-1) = error "xorn: negative"
xorn a = (fromIntegral a) `xor` (xorn (a .>>. 8))

xornArray :: Int -> V.Vector Word8
xornArray n = V.fromListN n (map xorn [0..(n-1)])

linearWring = Wring linearSbox linearInvSbox

-- | Creates a Wring with the given key.
-- To convert a String to a ByteString, put "- utf8-string" in your
-- package.yaml dependencies, import Data.ByteString.UTF8, and use
-- fromString.
keyedWring :: B.ByteString -> Wring
keyedWring key = Wring sbox (invert sbox)
 where
  sbox = sboxes key

{- A round of encryption consists of four steps:
 - mix3Parts
 - sboxes
 - rotBitcount
 - add byte index xor round number
 -}

-- Original purely functional version, modified to use vectors

roundEncryptFun ::
  Int ->
  SBox ->
  V.Vector Word8 ->
  V.Vector Word8 ->
  Int ->
  V.Vector Word8
roundEncryptFun rprime sbox xornary buf rond = i4 where
  len = V.length buf
  xornrond = xorn rond
  i1 = mix3Parts buf rprime
  i2 = V.fromListN len $ map (sbox V.!) $
    zipWith sboxInx (drop rond cycle3) (V.toList i1)
  i3 = rotBitcount i2 1
  i4 = V.fromListN len $ zipWith (+) (V.toList i3)
    (map (xor xornrond) (V.toList xornary))

roundDecryptFun ::
  Int ->
  SBox ->
  V.Vector Word8 ->
  V.Vector Word8 ->
  Int ->
  V.Vector Word8
roundDecryptFun rprime sbox xornary buf rond = i4 where
  len = V.length buf
  xornrond = xorn rond
  i1 = V.fromListN len $ zipWith (-) (V.toList buf)
    (map (xor xornrond) (V.toList xornary))
  i2 = rotBitcount i1 (-1)
  i3 = V.fromListN len $ map (sbox V.!) $
    zipWith sboxInx (drop rond cycle3) (V.toList i2)
  i4 = mix3Parts i3 rprime

encryptFun :: Wring -> V.Vector Word8 -> V.Vector Word8
encryptFun wring buf = foldl' (roundEncryptFun rprime (sbox wring) xornary)
  buf rounds
  where
    len = V.length buf
    xornary = xornArray len
    rprime = fromIntegral $ findMaxOrder (len `div` 3)
    rounds = [0 .. (nRounds len) -1]

decryptFun :: Wring -> V.Vector Word8 -> V.Vector Word8
decryptFun wring buf = foldl' (roundDecryptFun rprime (invSbox wring) xornary)
  buf rounds
  where
    len = V.length buf
    xornary = xornArray len
    rprime = fromIntegral $ findMaxOrder (len `div` 3)
    rounds = reverse [0 .. (nRounds len) -1]

-- ST monad version modifies memory in place
-- by int-e

{-# NOINLINE roundEncryptST #-}
roundEncryptST ::
  Int ->
  SBox ->
  V.Vector Word8 ->
  MV.MVector s Word8 ->
  MV.MVector s Word8 ->
  Int ->
  ST s ()
roundEncryptST rprime sbox xornary buf tmp rond = do
  let len = MV.length buf
      xornrond = xorn rond
  mix3Parts' buf rprime
  forM_ [0..len-1] $ \i -> do
      a <- MV.read buf i
      MV.write tmp i (sbox V.! (sboxInx ((i + rond) `rem` 3) a))
  rotBitcount' tmp 1 buf
  forM_ [0..len-1] $ \i -> do
      a <- MV.read buf i
      let a' = a + (xornrond `xor` (xornary V.! i))
      MV.write buf i a'

{-# NOINLINE roundDecryptST #-}
roundDecryptST ::
  Int ->
  SBox ->
  V.Vector Word8 ->
  MV.MVector s Word8 ->
  MV.MVector s Word8 ->
  Int ->
  ST s ()
roundDecryptST rprime sbox xornary buf tmp rond = do
  let len = MV.length buf
      xornrond = xorn rond
  forM_ [0..len-1] $ \i -> do
      a <- MV.read buf i
      let a' = a - (xornrond `xor` (xornary V.! i))
      MV.write tmp i a'
  rotBitcount' tmp (-1) buf
  forM_ [0..len-1] $ \i -> do
      a <- MV.read buf i
      MV.write buf i (sbox V.! (sboxInx ((i + rond) `rem` 3) a))
  mix3Parts' buf rprime

encryptST :: Wring -> V.Vector Word8 -> V.Vector Word8
encryptST wring buf = V.create $ do
  let len = V.length buf
      xornary = xornArray len
      rprime = findMaxOrder (len `div` 3)
      rounds = [0 .. nRounds len - 1]
  buf <- V.thaw buf
  tmp <- MV.new len
  forM_ [0..nRounds len - 1] $ \rond -> do
    roundEncryptST rprime (sbox wring) xornary buf tmp rond
  pure buf

decryptST :: Wring -> V.Vector Word8 -> V.Vector Word8
decryptST wring buf = V.create $ do
  let len = V.length buf
      xornary = xornArray len
      rprime = findMaxOrder (len `div` 3)
      nr = nRounds len - 1
      rounds = [0 .. nr]
  buf <- V.thaw buf
  tmp <- MV.new len
  forM_ [0..nRounds len - 1] $ \rond -> do
    roundDecryptST rprime (invSbox wring) xornary buf tmp (nr - rond)
  pure buf

-- Use either the ST version or the Fun version.

encrypt = encryptST
decrypt = decryptST
