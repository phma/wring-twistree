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
import Data.Vector.Unboxed (fromListN,fromList)
import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

key96 = "Водворетраванатраведрова.Нерубидрованатраведвора!"
key30 = "Πάντοτε χαίρετε!"
key6 = "aerate"
-- key96 is also used as a plaintext for hashing because 32|96.
text31 = "בראשית ברא אלהים "  --start of Bible
text33 = "árvíztűrő tükörfúrógépek"
wring96 = keyedWring $ fromString key96
wring30 = keyedWring $ fromString key30
wring6 = keyedWring $ fromString key6
wring0 = keyedWring $ fromString ""
twistree96 = keyedTwistree $ fromString key96
twistree30 = keyedTwistree $ fromString key30
twistree6 = keyedTwistree $ fromString key6
twistree0 = keyedTwistree $ fromString ""

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests, testVectorsWring, testVectorsTwistree]

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
  , testCase "searchDir" $
      (encrypt wring96 (fromList $ B.unpack $ fromString key96))
      -- This comes from a typo when translating to Julia. All the 8- and 9-byte
      -- tests passed, but the relPrimes entry which is findMaxOrder(32) was
      -- wrong, because it was searching in the wrong direction.
      -- This typo would also cause all hashes of strings >31 bytes to be wrong.
      @?= fromList
	[ 0xed, 0xbb, 0x75, 0xc4, 0xb2, 0xc9, 0xd2, 0x82
	, 0xa5, 0xbe, 0x37, 0x62, 0x4f, 0x5e, 0x7c, 0x1e 
	, 0x22, 0x59, 0xca, 0x7a, 0x66, 0xb3, 0xa7, 0x91
	, 0x34, 0xec, 0x50, 0x2e, 0x45, 0x3e, 0xc5, 0xc3
	, 0xcc, 0xf3, 0xa3, 0x49, 0x72, 0x38, 0x3e, 0x5c
	, 0xf6, 0x91, 0x44, 0x1c, 0x04, 0xf7, 0x80, 0x45 
	, 0xdb, 0x9c, 0xcb, 0xe6, 0x08, 0xa2, 0x8d, 0x6a
	, 0x5d, 0x28, 0x68, 0x93, 0x43, 0xa8, 0xb2, 0x65
	, 0x2b, 0xc3, 0x1e, 0xa3, 0x70, 0xcc, 0x1e, 0x40
	, 0xee, 0x7d, 0xa8, 0x19, 0x00, 0x72, 0x1e, 0x19 
	, 0x46, 0xb1, 0x18, 0x6d, 0x9c, 0x7d, 0x88, 0x59
	, 0x1b, 0x25, 0x01, 0x5d, 0x9d, 0xd7, 0x7c, 0x6d
	]
  ]

testVectorsTwistree = testGroup "Test vectors for Twistree"
  [ testCase "linearnull" $
      (hash linearTwistree (BL.fromStrict $ fromString ""))
      @?= fromList
	[ 0x51, 0xce, 0xe6, 0x7e, 0xbc, 0x77, 0xf0, 0xc8
	, 0xab, 0xa4, 0x8c, 0x39, 0x9a, 0x33, 0x46, 0xe7 
	, 0xe2, 0x1e, 0xc9, 0xc1, 0x39, 0x8f, 0xdb, 0x99
	, 0xb2, 0x9c, 0x41, 0x37, 0xd0, 0xd7, 0x53, 0xc2
	]
  , testCase "linearברא" $
      (hash linearTwistree (BL.fromStrict $ fromString text31))
      @?= fromList
	[ 0x20, 0x8d, 0x61, 0x88, 0x7c, 0x13, 0x27, 0x9f
	, 0x41, 0x26, 0x48, 0x31, 0x5e, 0x64, 0x2b, 0x0e 
	, 0xb1, 0xf1, 0xb6, 0xd2, 0xf9, 0x8f, 0x20, 0x29
	, 0x15, 0xf7, 0xf1, 0xe2, 0x31, 0xf3, 0x74, 0x18
	]
  , testCase "lineargép" $
      (hash linearTwistree (BL.fromStrict $ fromString text33))
      @?= fromList
	[ 0x61, 0x6b, 0x91, 0xe4, 0x6f, 0xab, 0x4c, 0x69
	, 0x03, 0xea, 0x9f, 0x2b, 0xc0, 0x0e, 0x90, 0xa2 
	, 0xe4, 0x40, 0xc3, 0x87, 0x1c, 0xa2, 0x27, 0x5a
	, 0x12, 0x10, 0x59, 0x4d, 0x8b, 0x33, 0x7f, 0x9e
	]
  , testCase "linearдвор" $
      (hash linearTwistree (BL.fromStrict $ fromString key96))
      @?= fromList
	[ 0x34, 0x82, 0x7e, 0xbe, 0x74, 0x06, 0x8e, 0xe1
	, 0xe7, 0x37, 0xaf, 0x31, 0x93, 0x2c, 0xd1, 0x85 
	, 0x35, 0x8c, 0x32, 0x1c, 0x0c, 0xc1, 0xda, 0x47
	, 0x97, 0x57, 0x18, 0xca, 0x57, 0xe3, 0xc8, 0xf8
	]
  , testCase "nullnull" $
      (hash twistree0 (BL.fromStrict $ fromString ""))
      @?= fromList
	[ 0x5b, 0x62, 0x5d, 0xeb, 0x4f, 0xa6, 0x92, 0xae
	, 0x56, 0xf9, 0xba, 0x20, 0xf6, 0xb4, 0xc1, 0x05 
	, 0xff, 0x92, 0x92, 0x3c, 0x7e, 0x84, 0xee, 0x2f
	, 0x83, 0xc1, 0x0d, 0xc2, 0x8f, 0xda, 0xa3, 0x7b
	]
  , testCase "nullברא" $
      (hash twistree0 (BL.fromStrict $ fromString text31))
      @?= fromList
	[ 0x4c, 0xc9, 0x2e, 0x58, 0xf2, 0x80, 0xaf, 0x58
	, 0x3e, 0x39, 0x6f, 0x3a, 0x9b, 0x7a, 0xdb, 0x59 
	, 0x65, 0x63, 0xb1, 0x28, 0x96, 0x68, 0x29, 0x83
	, 0xe4, 0x38, 0x1f, 0x79, 0x62, 0x78, 0x44, 0x0b
	]
  , testCase "nullgép" $
      (hash twistree0 (BL.fromStrict $ fromString text33))
      @?= fromList
	[ 0x3b, 0x8c, 0x2e, 0x6d, 0x7a, 0x29, 0x0a, 0x84
	, 0xf4, 0x40, 0xe8, 0x37, 0xc9, 0xd5, 0x8c, 0x64 
	, 0x11, 0x2a, 0x42, 0x17, 0x92, 0xd3, 0x33, 0xa0
	, 0x24, 0xbe, 0xa3, 0x3f, 0x4a, 0x4b, 0x18, 0x1f
	]
  , testCase "nullдвор" $
      (hash twistree0 (BL.fromStrict $ fromString key96))
      @?= fromList
	[ 0x4c, 0x19, 0x30, 0x4a, 0xae, 0x2a, 0x92, 0xba
	, 0x9c, 0x05, 0x69, 0x37, 0xd7, 0xfc, 0x36, 0x2d 
	, 0x29, 0x94, 0x8e, 0xdc, 0x3c, 0x56, 0x4f, 0x50
	, 0x08, 0xa7, 0x5b, 0x0a, 0x06, 0x95, 0x90, 0xee
	]
  , testCase "aeratenull" $
      (hash twistree6 (BL.fromStrict $ fromString ""))
      @?= fromList
	[ 0x0a, 0x4b, 0x98, 0x44, 0x15, 0xe7, 0x8b, 0xe2
	, 0xfe, 0xba, 0xf5, 0xe5, 0x51, 0x46, 0xe0, 0x05 
	, 0xc8, 0x0c, 0x13, 0x6b, 0xfb, 0x2f, 0x6f, 0xa4
	, 0xf6, 0x08, 0xbb, 0xa6, 0xe9, 0xf3, 0x35, 0xda
	]
  , testCase "aerateברא" $
      (hash twistree6 (BL.fromStrict $ fromString text31))
      @?= fromList
	[ 0x73, 0xd6, 0xe9, 0xc0, 0x63, 0xd5, 0x3c, 0xec
	, 0x4d, 0xd8, 0x3f, 0x89, 0x9f, 0x15, 0xf3, 0xf8 
	, 0xe6, 0x7e, 0xfb, 0xc5, 0x46, 0x4e, 0x11, 0x60
	, 0x9c, 0x0b, 0x75, 0xed, 0x35, 0x23, 0x56, 0x60
	]
  , testCase "aerategép" $
      (hash twistree6 (BL.fromStrict $ fromString text33))
      @?= fromList
	[ 0x77, 0x0b, 0xe3, 0xbe, 0xc4, 0x9c, 0xf9, 0xd0
	, 0xd1, 0x46, 0xda, 0x03, 0xea, 0xe5, 0x60, 0x4e 
	, 0x47, 0xec, 0xf1, 0x54, 0xe8, 0x6b, 0x63, 0x93
	, 0x59, 0x52, 0xf0, 0x95, 0xb7, 0x32, 0x64, 0x0f
	]
  , testCase "aerateдвор" $
      (hash twistree6 (BL.fromStrict $ fromString key96))
      @?= fromList
	[ 0x00, 0x0a, 0x4c, 0x5e, 0x61, 0xd8, 0xb0, 0x14
	, 0xfc, 0xe6, 0x46, 0xf9, 0xd3, 0x0f, 0xb2, 0x71 
	, 0x43, 0x8c, 0xc4, 0x3f, 0x7f, 0x72, 0x0a, 0xfe
	, 0xe1, 0xa3, 0xff, 0xd9, 0x5d, 0xe1, 0x65, 0x76
	]
  , testCase "χαίρετεnull" $
      (hash twistree30 (BL.fromStrict $ fromString ""))
      @?= fromList
	[ 0x33, 0x69, 0xba, 0x2b, 0x72, 0x58, 0x6a, 0x78
	, 0x6f, 0xc5, 0xd7, 0xbe, 0x3c, 0x80, 0xac, 0x24 
	, 0x87, 0xb8, 0x6e, 0x2e, 0x1c, 0x4f, 0xee, 0x76
	, 0x71, 0x49, 0x51, 0x7c, 0x58, 0xfb, 0x2e, 0x5a
	]
  , testCase "χαίρετεברא" $
      (hash twistree30 (BL.fromStrict $ fromString text31))
      @?= fromList
	[ 0xef, 0x78, 0x8f, 0x13, 0xb6, 0xb7, 0xd7, 0x9c
	, 0xae, 0xce, 0xbe, 0x56, 0x80, 0x14, 0x4a, 0x37 
	, 0x49, 0x26, 0xd6, 0x88, 0x69, 0x3e, 0x66, 0xd1
	, 0xc6, 0xeb, 0x82, 0x37, 0x57, 0x53, 0xe1, 0x13
	]
  , testCase "χαίρετεgép" $
      (hash twistree30 (BL.fromStrict $ fromString text33))
      @?= fromList
	[ 0xf5, 0xde, 0x17, 0xa3, 0xc6, 0xde, 0x7e, 0x4f
	, 0x17, 0xf6, 0xef, 0x3a, 0xe5, 0x7b, 0x1e, 0xc3 
	, 0xa4, 0x9d, 0x9c, 0x7d, 0x85, 0x42, 0xf4, 0xb3
	, 0xd4, 0x85, 0x94, 0x4d, 0x73, 0x98, 0x79, 0x80
	]
  , testCase "χαίρετεдвор" $
      (hash twistree30 (BL.fromStrict $ fromString key96))
      @?= fromList
	[ 0x51, 0x35, 0x1a, 0xb4, 0x7b, 0x42, 0x96, 0xab
	, 0x8c, 0xcd, 0xb7, 0xca, 0x12, 0x1b, 0xe2, 0x26 
	, 0x73, 0x0e, 0x43, 0xd4, 0x42, 0x70, 0xd6, 0x93
	, 0x0c, 0xee, 0xe2, 0xfe, 0x87, 0x88, 0x26, 0xee
	]
  , testCase "дворnull" $
      (hash twistree96 (BL.fromStrict $ fromString ""))
      @?= fromList
	[ 0x4e, 0xfa, 0x85, 0x3a, 0xa1, 0xd2, 0x57, 0x43
	, 0x87, 0x44, 0xf4, 0x37, 0x6d, 0x11, 0x40, 0x73 
	, 0x38, 0x22, 0xc8, 0xd2, 0x2f, 0x0b, 0xb1, 0xba
	, 0x06, 0x3b, 0x8e, 0x55, 0x54, 0x70, 0x76, 0x4c
	]
  , testCase "дворברא" $
      (hash twistree96 (BL.fromStrict $ fromString text31))
      @?= fromList
	[ 0xf9, 0x1d, 0x93, 0x79, 0x4e, 0x6e, 0xdd, 0x91
	, 0x92, 0xcd, 0x47, 0xc4, 0xe8, 0xd2, 0xf9, 0xba 
	, 0x0b, 0x5e, 0xca, 0x38, 0x92, 0xa8, 0xf9, 0x6e
	, 0x2a, 0xdc, 0x49, 0xaa, 0x0c, 0x0b, 0x4d, 0xff
	]
  , testCase "дворgép" $
      (hash twistree96 (BL.fromStrict $ fromString text33))
      @?= fromList
	[ 0xde, 0x94, 0xa0, 0x0c, 0xf4, 0x4c, 0x5a, 0xe2
	, 0xfb, 0xf7, 0x08, 0x3c, 0x9a, 0xdb, 0xda, 0x25 
	, 0xa0, 0x2a, 0x01, 0xfb, 0x68, 0x81, 0x94, 0x88
	, 0x8b, 0xc2, 0xbe, 0x1d, 0x6c, 0xae, 0x7b, 0x9c
	]
  , testCase "двордвор" $
      (hash twistree96 (BL.fromStrict $ fromString key96))
      @?= fromList
	[ 0xb1, 0x1b, 0x84, 0x9d, 0xf7, 0xb4, 0x15, 0xa3
	, 0xf9, 0xdc, 0x69, 0x47, 0xe1, 0xd2, 0xc0, 0x20 
	, 0x64, 0x0d, 0x8f, 0xf6, 0x79, 0xb6, 0x7b, 0x69
	, 0xbc, 0x70, 0xe1, 0x2e, 0x3d, 0xd7, 0xeb, 0x57
	]
  ]
