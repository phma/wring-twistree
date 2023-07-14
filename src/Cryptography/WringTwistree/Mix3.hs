module Cryptography.WringTwistree.Mix3
  ( mix
  , fiboPair
  , searchDir
  ) where

import Data.Bits
import Data.Word
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))
import Math.NumberTheory.ArithmeticFunctions
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
