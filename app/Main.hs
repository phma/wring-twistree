module Main (main) where

import Cryptography.Wring
import Cryptography.Twistree
import Cryptography.WringTwistree.KeySchedule
import Cryptography.WringTwistree.Compress
import Cryptography.WringTwistree.Blockize
import Cryptography.WringTwistree.Sboxes
import Cryptanalysis
import Text.Printf
import Data.List.Split
import Data.Word
import Data.Array.Unboxed
import Data.Foldable (toList)
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

readFileLazy :: String -> IO (BL.ByteString)
readFileLazy fileName = do
  h <- openBinaryFile fileName ReadMode
  contents <- BL.hGetContents h
  return contents

writeFileArray :: String -> UArray Int Word8 -> IO ()
writeFileArray fileName ary = do
  h <- openBinaryFile fileName WriteMode
  BL.hPut h (BL.pack $ elems ary)
  hClose h

encryptFile :: String -> String -> String -> IO ()
encryptFile key plainfile cipherfile = do
  let wring = keyedWring (fromString key)
  plaintext <- readFileEager plainfile
  let ciphertext = encrypt wring plaintext
  writeFileArray cipherfile ciphertext

decryptFile :: String -> String -> String -> IO ()
decryptFile key cipherfile plainfile = do
  let wring = keyedWring (fromString key)
  ciphertext <- readFileEager cipherfile
  let plaintext = decrypt wring ciphertext
  writeFileArray plainfile plaintext

hashFile :: String -> String -> String -> IO ()
hashFile key plainfile outfile = do
  let twistree = keyedTwistree (fromString key)
  plaintext <- readFileLazy plainfile
  let hashtext = hash twistree plaintext
  if null outfile
    then putStrLn $ block16str $ elems hashtext
    else writeFileArray outfile hashtext

testHash :: IO ()
testHash = do
  let twistree = keyedTwistree (fromString "")
  let plaintext = (take 8 (repeat 105)) ++ (take 9 (repeat 150))
  let hashtext = hash twistree $ BL.pack plaintext
  putStrLn $ block16str $ elems hashtext

cryptanalyze :: String -> IO ()
cryptanalyze arg = case arg of
  "relkey"  -> relatedKey
  otherwise -> putStrLn ("Available cryptanalyses are:\n" ++
    "relkey  Related-key cryptanalysis")

data WtOpt
  = Infile String
  | Analyze String
  | Encrypt
  | Decrypt
  | Hash
  | Test
  | Key String
  | Outfile String
    deriving (Show,Eq)

doWhich :: [WtOpt] -> Maybe WtOpt
-- Returns Just the action, if exactly one action is specified, else Nothing.
doWhich lst = if (length actions == 1) then Just (head actions) else Nothing where
  actions = filter
    (\x -> case x of
      Encrypt -> True
      Decrypt -> True
      Hash -> True
      Analyze _ -> True
      Test -> True
      otherwise -> False)
    lst

strings_ :: [WtOpt] -> (String,String,String)
strings_ [] = ("","","")
strings_ (Key s:ws)     = (s,infile,outfile)
  where (key,infile,outfile) = strings_ ws
strings_ (Infile s:ws)  = (key,s,outfile)
  where (key,infile,outfile) = strings_ ws
strings_ (Outfile s:ws) = (key,infile,s)
  where (key,infile,outfile) = strings_ ws
strings_ (_:ws)         = (key,infile,outfile)
  where (key,infile,outfile) = strings_ ws

strings :: [WtOpt] -> (String,String,String)
-- If no outfile is specified, write encrypted or decrypted file back to input,
-- but output hash to stdout.
strings ws = (key,infile,outfile) where
  (key,infile,outfile_) = strings_ ws
  outfile = if (null outfile_) && (doWhich ws) /= Just Hash
            then infile
            else outfile_

optSpecs :: [OptSpec WtOpt]
optSpecs =
  [ optSpec "e" ["encrypt"] (ZeroArg Encrypt)
  , optSpec "d" ["decrypt"] (ZeroArg Decrypt)
  , optSpec "c" ["cryptanalyze"] (OneArg Analyze)
  , optSpec "H" ["hash"] (ZeroArg Hash)
  , optSpec "t" ["test"] (ZeroArg Test) -- for running code I'm testing
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
  , "--encrypt,      -e       Encrypt a file."
  , "--decrypt,      -d       Decrypt a file."
  , "--cryptanalyze, -c NUM   Run cryptanalysis number NUM."
  , "--hash,         -H       Hash a file."
  , "--test,         -t       Test the latest code."
  , "--output FILE,  -o FILE  Output results to FILE."
  , ""
  , "At most one of -e, -d, -c, and -H may be specified. If -o is not specified,"
  , "the ciphertext or plaintext overwrites the original file, and the hash"
  , "is written to standard output."
  ]

testExtendKey :: IO ()
testExtendKey = do
  putStr $ block16str16 $ toList $ extendKey $ fromString "roygbiv"
  putStr $ block16str16 $ toList $ extendKey $ fromString "aerate"

testCompress :: IO ()
testCompress = do
  putStr $ block16str $ elems $
    compress2 linearSbox exp4_2adic (exp4_base2 :: UArray Int Word8) 0

doCommandLine :: [WtOpt] -> IO ()
doCommandLine parse = case action of
    Just Encrypt     -> encryptFile key infile outfile
    Just Decrypt     -> decryptFile key infile outfile
    Just Test        -> testHash
    Just (Analyze a) -> cryptanalyze a
    Just Hash        -> hashFile key infile outfile
    Nothing          -> putStrLn "Please specify one of -e, -d, and -H"
    _                -> error "can't happen"
  where
    action = doWhich parse
    (key,infile,outfile) = strings parse

main :: IO ()
main = do
  parse <- parseCommandLine help optSpecs Infile
  doCommandLine parse
