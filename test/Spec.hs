{-# LANGUAGE InstanceSigs #-}
import Data.Word
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Cryptography.WringTwistree.Permute
import Cryptography.WringTwistree.KeySchedule
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

wrapPermut8 :: [a] -> Word16 -> [a]
wrapPermut8 as n = toList $ permut8 (Seq.fromList as) n

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
