module Main (main) where

import Cryptography.Wring
import Text.Printf
import Data.List.Split
import Data.Word
import Data.Bits
import Data.Array.Unboxed

lineStr :: [Word8] -> String
lineStr [] = ""
lineStr (a:as) = (printf "%02x " a)++(lineStr as)

blockStr :: [[Word8]] -> String
blockStr [] = ""
blockStr (a:as) = (lineStr a) ++ "\n" ++ (blockStr as)

block16str :: [Word8] -> String
-- Takes a list of 256 bytes and formats them 16 to a line.
block16str a = blockStr $ chunksOf 16 a

wrungZeros = encrypt linearWring (listArray (0,255::Int) (replicate 256 0))

main :: IO ()
main = putStr $ block16str $ elems wrungZeros
