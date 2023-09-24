{-# LANGUAGE InstanceSigs #-}
import Data.Word
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Cryptography.WringTwistree.Permute
import Cryptography.WringTwistree.KeySchedule
import Cryptography.Wring
import Cryptography.Twistree
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.Array.Unboxed
import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

key96 = "Водворетраванатраведрова.Нерубидрованатраведвора!"
key30 = "Πάντοτε χαίρετε!"
key6 = "aerate"
wring96 = keyedWring $ fromString key96
wring30 = keyedWring $ fromString key30
wring6 = keyedWring $ fromString key6

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests, testVectors]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

wrapPermut8 :: [a] -> Word16 -> [a]
wrapPermut8 as n = toList $ permut8 (Seq.fromList as) n

stringToBytes :: String -> [Word8]
stringToBytes = map (fromIntegral . fromEnum)

seq40504 = 0 : map (mul65537 40504) seq40504

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "repaints" $
      \x -> wrapPermut8 "repaints" x /= "pantries"
  , QC.testProperty "mul65537" $
      \x y -> mul65537 x y == mul65537 (65535-x) (65535-y)
  ]

unitTests = testGroup "Unit tests"
  [ testCase "calipers" $
      (wrapPermut8 "calipers" 0x3c45) @?= "spiracle"
  , testCase "recounts" $
      (wrapPermut8 "recounts" 0x595b) @?= "construe"
  , testCase "thousand" $
      (wrapPermut8 "thousand" 0x30da) @?= "handouts"
  , testCase "mul65537 40504" $
      (length $ takeWhile (/= 0) $ tail seq40504) @?= 65535
  , testCase "mul65537 65535" $
      (mul65537 65535 65535) @?= 0
  ]

testVectors = testGroup "Test vectors"
  [ testCase "lin8nulls" $
      (encrypt linearWring (listArray (0,7) [0,0,0,0,0,0,0,0]))
      @?= listArray (0,7::Int) [0x04,0xd7,0x16,0x6a,0xca,0x70,0x57,0xbc]
  , testCase "lin8ff" $
      (encrypt linearWring (listArray (0,7) [255,255,255,255,255,255,255,255]))
      @?= listArray (0,7::Int) [0x84,0x91,0x95,0xa0,0x9f,0x48,0x4b,0x59]
  , testCase "lin8Twistree" $
      (encrypt linearWring (listArray (0,7) $ stringToBytes "Twistree"))
      @?= listArray (0,7::Int) [0xed,0x4b,0x2e,0xc6,0xc4,0x39,0x65,0x2b]
  , testCase "lin9nulls" $
      (encrypt linearWring (listArray (0,8) [0,0,0,0,0,0,0,0,0]))
      @?= listArray (0,8::Int) [0xad,0x93,0x5e,0x85,0xe1,0x49,0x45,0xca,0xe2]
  , testCase "lin9ff" $
      (encrypt linearWring (listArray (0,8) [255,255,255,255,255,255,255,255,255]))
      @?= listArray (0,8::Int) [0x36,0xdf,0x60,0xae,0xf5,0xbd,0x1a,0xaf,0x6e]
  , testCase "lin9AllOrNone" $
      (encrypt linearWring (listArray (0,8) $ stringToBytes "AllOrNone"))
      @?= listArray (0,8::Int) [0x53,0x28,0xe7,0xc7,0xe0,0x71,0xa5,0x2e,0x8c]
  ]
