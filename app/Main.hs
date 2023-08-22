module Main (main) where

import Cryptography.Wring
import Cryptography.WringTwistree.KeySchedule
import Text.Printf
import Data.List.Split
import Data.Word
import Data.Bits
import Data.Array.Unboxed
import Data.Foldable (toList,foldl')
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (fromString)
import System.IO
import Multiarg

lineStr :: [Word8] -> String
lineStr [] = ""
lineStr (a:as) = (printf "%02x " a)++(lineStr as)

lineStr16 :: [Word16] -> String
lineStr16 [] = ""
lineStr16 (a:as) = (printf "%04x " a)++(lineStr16 as)

blockStr :: [[Word8]] -> String
blockStr [] = ""
blockStr (a:as) = (lineStr a) ++ "\n" ++ (blockStr as)

blockStr16 :: [[Word16]] -> String
blockStr16 [] = ""
blockStr16 (a:as) = (lineStr16 a) ++ "\n" ++ (blockStr16 as)

block16str :: [Word8] -> String
-- Takes a list of 256 bytes and formats them 16 to a line.
block16str a = blockStr $ chunksOf 16 a

block16str16 :: [Word16] -> String
-- Takes a list of 96 words and formats them 16 to a line.
block16str16 a = blockStr16 $ chunksOf 16 a

wrungZeros :: UArray Int Word8
wrungZeros = encrypt linearWring (listArray (0,255::Int) (replicate 256 0))

dumpSbox :: UArray (Word8,Word8) Word8 -> IO ()
dumpSbox sbox = putStr $ block16str $ take 256 (elems sbox)

readFileEager :: String -> IO (UArray Int Word8)
readFileEager fileName = do
  h <- openBinaryFile fileName ReadMode
  contents <- B.hGetContents h
  let contArray = listArray (0,(B.length contents - 1)) (B.unpack contents)
  return contArray

--encryptFile :: String -> String -> String -> IO ()

data WtOpt
  = Infile String
  | Encrypt
  | Decrypt
  | Hash
  | Key String
  | Outfile String
    deriving (Show,Eq)

doWhich :: [WtOpt] -> Maybe WtOpt
-- Returns Just the action, if exactly one action is specified, else Nothing.
doWhich lst = if (length actions == 1) then Just (head actions) else Nothing where
  actions = filter (\x -> (x == Encrypt) || (x == Decrypt) || (x == Hash)) lst

optSpecs :: [OptSpec WtOpt]
optSpecs =
  [ optSpec "e" ["encrypt"] (ZeroArg Encrypt)
  , optSpec "d" ["decrypt"] (ZeroArg Decrypt)
  , optSpec "H" ["hash"] (ZeroArg Hash)
  , optSpec "k" ["key"] (OneArg Key)
  , optSpec "o" ["output"] (OneArg Outfile)
  ]

help :: String -> String
help progName = unlines
  [ progName ++ " - Wring cipher and Twistree hash"
  , "Usage:"
  , progName ++ " [options] INPUTFILE ..."
  , ""
  , "Options:"
  , ""
  , "--encrypt,     -e       Encrypt a file."
  , "--decrypt,     -d       Decrypt a file."
  , "--hash,        -H       Hash a file."
  , "--output FILE, -o FILE  Output results to FILE."
  , ""
  , "At most one of -e, -d, and -H may be specified. If -o is not specified,"
  , "the ciphertext or plaintext overwrites the original file, and the hash"
  , "is written to standard output."
  ]

testExtendKey :: IO ()
testExtendKey = do
  putStr $ block16str16 $ toList $ extendKey $ fromString "roygbiv"
  putStr $ block16str16 $ toList $ extendKey $ fromString "aerate"

main :: IO ()
main = do
  parse <- parseCommandLine help optSpecs Infile
  putStrLn $ show parse
