{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Utilities.ComplexFunctions
(htf_thisModulesTests) where

import Data.Complex
import Test.Framework
import Test.HUnit
import Pudding.Utilities.ComplexFunctions
import Pudding.Utilities.DoubleFunctions

prop_complexEqSelf :: (Complex Double) -> Bool
prop_complexEqSelf z = z `complexEq` z

prop_complexEqDifferent :: (Complex Double) -> (Complex Double) -> Property
prop_complexEqDifferent z1 z2 =
  (not (realPart z1 `doubleEq` realPart z2) ||
  not (imagPart z1 `doubleEq` imagPart z2))
    ==> not $ z1 `complexEq` z2

prop_scaleComplexScales :: Double -> (Complex Double) -> Bool
prop_scaleComplexScales d z =
  let z' = scaleComplex d z in
    realPart z' `doubleEq` (d * (realPart z)) &&
    imagPart z' `doubleEq` (d * (imagPart z))

