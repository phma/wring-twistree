{-# LANGUAGE InstanceSigs #-}
import Data.Word
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Cryptography.WringTwistree.Permute
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

wrapPermut8 :: [a] -> Word16 -> [a]
wrapPermut8 as n = toList $ permut8 (Seq.fromList as) n

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "repaints" $
      \x -> wrapPermut8 "repaints" x /= "pantries"
  ]

unitTests = testGroup "Unit tests"
  [ testCase "calipers" $
      (wrapPermut8 "calipers" 0x3c45) @?= "spiracle"
  , testCase "recounts" $
      (wrapPermut8 "recounts" 0x595b) @?= "construe"
  , testCase "thousand" $
      (wrapPermut8 "thousand" 0x30da) @?= "handouts"
  ]
