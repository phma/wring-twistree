module Cryptography.WringTwistree.Mix3
  ( mix
  , fiboPair
  , searchDir
  , isMaxOrder
  , searchSeq
  ) where

import Data.Bits
import Data.Word
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))
import Math.NumberTheory.ArithmeticFunctions
import GHC.Natural
import Math.NumberTheory.Primes

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
