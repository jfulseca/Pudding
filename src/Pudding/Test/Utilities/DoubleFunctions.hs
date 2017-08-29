{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Utilities.DoubleFunctions
(htf_thisModulesTests) where

import Test.Framework
import Test.HUnit
import Pudding.Utilities.DoubleFunctions

prop_fmodCutoff :: Double -> (NonZero Double) -> Bool
prop_fmodCutoff a (NonZero b) = (abs (a `fmod` b)) < (abs b)

prop_fmodPeriod :: Double -> (NonZero Double) -> Bool
prop_fmodPeriod a (NonZero b) = (a `fmod` b) `doubleEq` ((a + b) `fmod` b)

prop_fmodSmallerPositive :: (Positive Double) -> Double -> Property
prop_fmodSmallerPositive (Positive a) b = (a < b) ==> (a `fmod` b) `doubleEq` a

prop_fmodSmallerNegative :: (Positive Double) -> Double -> Property
prop_fmodSmallerNegative (Positive a) b = (a < b) ==> ((-a) `fmod` b) `doubleEq` (b - a)

equalTest :: Double -> Assertion
equalTest d = assertEqual True $ d `doubleEq` d

differentTest :: Double -> Double -> Assertion
differentTest d1 d2 = assertEqual False $ d1 `doubleEq` d2

test_doubleEqOneOne = equalTest 1
test_doubleEqZeroZero = equalTest 0
test_doubleEqMinuszeroMinuszero = equalTest (-0)
test_doubleEqThirdThird = equalTest 0.3333
test_doubleEqSmallSmall = equalTest 1e-30

test_doubleEqOneTwo = differentTest 1 2
test_doubleEqZeroOne = differentTest 0 1
test_doubleEqOneZero = differentTest 1 0
test_doubleEqSmallOthersmall = differentTest 1.23e-50 1.234e-50

test_doubleEqZeroMinuszero =
  assertEqual True $ 0 `doubleEq` (-0)

test_doubleEqMinuszeroZero =
  assertEqual True $ (-0) `doubleEq` 0
