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
import Data.Vector.Unboxed (fromListN)
import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

key96 = "Водворетраванатраведрова.Нерубидрованатраведвора!"
key30 = "Πάντοτε χαίρετε!"
key6 = "aerate"
wring96 = keyedWring $ fromString key96
wring30 = keyedWring $ fromString key30
wring6 = keyedWring $ fromString key6
wring0 = keyedWring $ fromString ""

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests, testVectorsWring]

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
  , QC.testProperty "roundtrip" $
      \a b c d e f g h ->
        (decrypt wring0 $ encrypt wring0 (fromListN 8 [a,b,c,d,e,f,g,h]))
        == (fromListN 8 [a,b,c,d,e,f,g,h])
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

testVectorsWring = testGroup "Test vectors for Wring"
  [ testCase "lin8nulls" $
      (encrypt linearWring (fromListN 8 [0,0,0,0,0,0,0,0]))
      @?= fromListN 8 [0x04,0xd7,0x16,0x6a,0xca,0x70,0x57,0xbc]
  , testCase "lin8ff" $
      (encrypt linearWring (fromListN 8 [255,255,255,255,255,255,255,255]))
      @?= fromListN 8 [0x84,0x91,0x95,0xa0,0x9f,0x48,0x4b,0x59]
  , testCase "lin8Twistree" $
      (encrypt linearWring (fromListN 8 $ stringToBytes "Twistree"))
      @?= fromListN 8 [0xed,0x4b,0x2e,0xc6,0xc4,0x39,0x65,0x2b]
  , testCase "lin9nulls" $
      (encrypt linearWring (fromListN 9 [0,0,0,0,0,0,0,0,0]))
      @?= fromListN 9 [0xad,0x93,0x5e,0x85,0xe1,0x49,0x45,0xca,0xe2]
  , testCase "lin9ff" $
      (encrypt linearWring (fromListN 9 [255,255,255,255,255,255,255,255,255]))
      @?= fromListN 9 [0x36,0xdf,0x60,0xae,0xf5,0xbd,0x1a,0xaf,0x6e]
  , testCase "lin9AllOrNone" $
      (encrypt linearWring (fromListN 9 $ stringToBytes "AllOrNone"))
      @?= fromListN 9 [0x53,0x28,0xe7,0xc7,0xe0,0x71,0xa5,0x2e,0x8c]
  , testCase "null8nulls" $
      (encrypt wring0 (fromListN 8 [0,0,0,0,0,0,0,0]))
      @?= fromListN 8 [0x77,0x3e,0x34,0x8f,0x48,0xa1,0x24,0x1a]
  , testCase "null8ff" $
      (encrypt wring0 (fromListN 8 [255,255,255,255,255,255,255,255]))
      @?= fromListN 8 [0xc7,0xa7,0x58,0xed,0x5c,0x2b,0xb6,0xec]
  , testCase "null8Twistree" $
      (encrypt wring0 (fromListN 8 $ stringToBytes "Twistree"))
      @?= fromListN 8 [0xa3,0xcf,0xd4,0xa1,0x0d,0x7e,0xb7,0xb3]
  , testCase "null9nulls" $
      (encrypt wring0 (fromListN 9 [0,0,0,0,0,0,0,0,0]))
      @?= fromListN 9 [0x10,0x10,0x95,0x96,0x90,0xb5,0x97,0xeb,0x38]
  , testCase "null9ff" $
      (encrypt wring0 (fromListN 9 [255,255,255,255,255,255,255,255,255]))
      @?= fromListN 9 [0x09,0x0f,0xf3,0x66,0x36,0xa4,0xac,0x8d,0x5c]
  , testCase "null9AllOrNone" $
      (encrypt wring0 (fromListN 9 $ stringToBytes "AllOrNone"))
      @?= fromListN 9 [0xee,0x15,0x02,0x05,0xdd,0xa9,0x77,0xe4,0x23]
  , testCase "aerate8nulls" $
      (encrypt wring6 (fromListN 8 [0,0,0,0,0,0,0,0]))
      @?= fromListN 8 [0x23,0x44,0x2e,0x6e,0xf3,0xd7,0xa0,0x7e]
  , testCase "aerate8ff" $
      (encrypt wring6 (fromListN 8 [255,255,255,255,255,255,255,255]))
      @?= fromListN 8 [0x7e,0x05,0xae,0x5c,0x64,0xdd,0xf4,0xeb]
  , testCase "aerate8Twistree" $
      (encrypt wring6 (fromListN 8 $ stringToBytes "Twistree"))
      @?= fromListN 8 [0x36,0x39,0x14,0x22,0x40,0x7f,0xc3,0x79]
  , testCase "aerate9nulls" $
      (encrypt wring6 (fromListN 9 [0,0,0,0,0,0,0,0,0]))
      @?= fromListN 9 [0x41,0xc1,0x44,0x0f,0x07,0x2d,0x92,0xbf,0x43]
  , testCase "aerate9ff" $
      (encrypt wring6 (fromListN 9 [255,255,255,255,255,255,255,255,255]))
      @?= fromListN 9 [0x46,0xb0,0x57,0x43,0xfb,0xdb,0x9d,0x32,0x88]
  , testCase "aerate9AllOrNone" $
      (encrypt wring6 (fromListN 9 $ stringToBytes "AllOrNone"))
      @?= fromListN 9 [0x5e,0x3b,0x49,0xd4,0xb8,0x70,0xdd,0x07,0xac]
  , testCase "χαίρετε8nulls" $
      (encrypt wring30 (fromListN 8 [0,0,0,0,0,0,0,0]))
      @?= fromListN 8 [0x90,0x4d,0x00,0x3e,0x39,0x75,0x9e,0xe4]
  , testCase "χαίρετε8ff" $
      (encrypt wring30 (fromListN 8 [255,255,255,255,255,255,255,255]))
      @?= fromListN 8 [0x85,0x56,0x3d,0x4e,0x84,0x5a,0x14,0xe3]
  , testCase "χαίρετε8Twistree" $
      (encrypt wring30 (fromListN 8 $ stringToBytes "Twistree"))
      @?= fromListN 8 [0x96,0x89,0x48,0x50,0x98,0x26,0xeb,0x03]
  , testCase "χαίρετε9nulls" $
      (encrypt wring30 (fromListN 9 [0,0,0,0,0,0,0,0,0]))
      @?= fromListN 9 [0x04,0xb0,0x1e,0xdf,0xd3,0xf0,0x39,0xa3,0x3c]
  , testCase "χαίρετε9ff" $
      (encrypt wring30 (fromListN 9 [255,255,255,255,255,255,255,255,255]))
      @?= fromListN 9 [0x7c,0x67,0xeb,0xc8,0x40,0x97,0xc2,0x5f,0x82]
  , testCase "χαίρετε9AllOrNone" $
      (encrypt wring30 (fromListN 9 $ stringToBytes "AllOrNone"))
      @?= fromListN 9 [0x4b,0xb2,0x68,0xe9,0xf1,0x64,0x0a,0x44,0xc4]
  , testCase "двор8nulls" $
      (encrypt wring96 (fromListN 8 [0,0,0,0,0,0,0,0]))
      @?= fromListN 8 [0x9c,0xc1,0x3c,0xe7,0x8a,0xc5,0x6f,0x18]
  , testCase "двор8ff" $
      (encrypt wring96 (fromListN 8 [255,255,255,255,255,255,255,255]))
      @?= fromListN 8 [0x37,0x42,0x00,0x47,0xd5,0x2f,0x9d,0x7f]
  , testCase "двор8Twistree" $
      (encrypt wring96 (fromListN 8 $ stringToBytes "Twistree"))
      @?= fromListN 8 [0x15,0x06,0xc6,0xa6,0x3d,0xef,0x19,0xf1]
  , testCase "двор9nulls" $
      (encrypt wring96 (fromListN 9 [0,0,0,0,0,0,0,0,0]))
      @?= fromListN 9 [0xd6,0x7b,0x2c,0xcc,0x71,0x24,0x5d,0x06,0x07]
  , testCase "двор9ff" $
      (encrypt wring96 (fromListN 9 [255,255,255,255,255,255,255,255,255]))
      @?= fromListN 9 [0xd3,0xa8,0x3f,0xb6,0x9a,0x5e,0x0f,0x07,0x11]
  , testCase "двор9AllOrNone" $
      (encrypt wring96 (fromListN 9 $ stringToBytes "AllOrNone"))
      @?= fromListN 9 [0x53,0x6c,0x7e,0x2d,0xcd,0xda,0xdc,0xf2,0x70]
  ]
