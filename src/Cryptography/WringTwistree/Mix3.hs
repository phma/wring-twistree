module Cryptography.WringTwistree.Mix3
  ( findMaxOrder
  , mix3Parts
  ) where

{- This module is used in Wring.
 - This module splits a buffer (an array of bytes) into three equal parts, with
 - 0, 1, or 2 bytes left over, and mixes them as follows:
 -
 - The mix function takes three bytes and flips each bit which is not the same
 - in all three bytes. This is a self-inverse, nonlinear operation.
 -
 - The 0th third is traversed forward, the 1st third is traversed backward,
 - and the 2nd third is traversed by steps close to 1/φ the length of a third.
 - Taking a 16-byte buffer as an example:
 - 00 0d 1a 27 34|41 4e 5b 68 75|82 8f 9c a9 b6|c3
 - <>            |            <>|<>            |
 -    <>         |         <>   |         <>   |
 -       <>      |      <>      |   <>         |
 -          <>   |   <>         |            <>|
 -             <>|<>            |      <>      |
 - f7 e8 cf de c9|bc b7 8e 8d 82|75 5a 61 4c 4f|c3
 -}

import Data.Bits
import Data.Word
import Data.Array.Unboxed
import Math.NumberTheory.ArithmeticFunctions
import GHC.Natural
import Math.NumberTheory.Primes

{-# INLINE mix #-}
{-# SPECIALIZE mix :: Word8 -> Word8 -> Word8 -> Word8 #-}
mix :: (Num t,Bits t) => t -> t -> t -> t
mix a b c = xor a mask
  where mask = (a .|. b .|. c) - (a .&. b .&. c)

fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

fiboPair :: Integer -> [Integer]
fiboPair n = take 2 $ dropWhile (<= n) fibonacci

searchDir :: Integer -> (Integer,Int)
-- fst=n/φ rounded to nearest. snd=+1 or -1, indicating search direction.
-- e.g. if n=89, returns (55,1). Search 55,56,54,57,53...
-- if n=144, returns (89,(-1)). Search 89,88,90,87,91...
searchDir n
  | r*2 < den = (q,1)
  | otherwise = (q+1,(-1))
  where [num,den] = fiboPair (2*n)
        (q,r) = (n*num) `divMod` den

isMaxOrder :: Integral a => a -> a -> [a] -> a -> Bool
-- isMaxOrder modl car fac n
-- where modl is the modulus, car is its Carmichael function,
-- fac is the set of prime factors of car (without multiplicities),
-- and n is the number being tested.
-- Returns true if n has maximum order, which implies it's a primitive root
-- if modulus has any primitive roots.
isMaxOrder modl car fac n = (powModNatural nn ncar nmodl) == 1 && allnot1
  where nn = (fromIntegral n) :: Natural
        ncar = (fromIntegral car) :: Natural
        nmodl = (fromIntegral modl) :: Natural
        powns = map ((\x -> powModNatural nn (fromIntegral x) nmodl) . (car `div`)) fac
        allnot1 = foldl (&&) True (map (/= 1) powns)

searchSeq = map (\n -> if (odd n) then (n `div` 2 + 1) else (-n `div` 2)) [0..]

searchFrom :: (Integer,Int) -> [Integer]
searchFrom (start,dir) = map (\x -> x*(fromIntegral dir)+start) searchSeq

findMaxOrder :: Integer -> Integer
-- n must be positive.
-- Returns the number of maximum multiplicative order mod n closest to n/φ.
-- n=1 is a special case, as (isMaxOrder 1 1 [] i) returns False for all i>=0.
findMaxOrder 1 = 1
findMaxOrder n = head $ filter (isMaxOrder n car fac) $ searchFrom $ searchDir n
  where car = carmichael n
        fac = map (unPrime . fst) $ factorise car

triplicate :: [(a,a,a)] -> [(a,a,a)]
triplicate [] = []
triplicate ((a,b,c):xs) = (a,b,c):(b,c,a):(c,a,b):triplicate xs

tailpiece :: Integral a => a -> [(a,a,a)]
tailpiece len
  | (len `mod` 3 == 0) = []
  | otherwise = (len-1,len-1,len-1) : (tailpiece (len-1))

{-# SPECIALIZE mixOrder :: Int -> Int -> [(Int,Int,Int)] #-}
mixOrder :: Integral a => a -> a -> [(a,a,a)]
-- rprime is relatively prime to len `div` 3
mixOrder len rprime
  | len < 3 = tailpiece len
  | otherwise = (tailpiece len) ++ (triplicate mixord)
  where
    third = len `div` 3
    mixord = take (fromIntegral third) $ zip3
      [0..]
      (map ((2*third-1) -) [0..])
      (map ((2*third) +) (iterate (\x -> (x + rprime) `mod` third) 0))

{-# INLINABLE mix3Parts #-}
{-# SPECIALIZE mix3Parts :: UArray Int Word8 -> Int -> UArray Int Word8 #-}
mix3Parts :: (Ix a,Integral a) => UArray a Word8 -> a -> UArray a Word8
-- The index of buf must start at 0.
-- Compute rprime once (findMaxOrder (fromIntegral (div len 3)))
-- and pass it to mix3Parts on every round.
mix3Parts buf rprime = array (bounds buf) mixed
  where
    mixed = map (\(a,b,c) -> (a, mix (buf ! a) (buf ! b) (buf ! c))) order
    order = mixOrder len rprime
    bnd = bounds buf
    len = (snd bnd) +1
