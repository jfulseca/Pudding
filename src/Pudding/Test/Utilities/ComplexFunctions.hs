{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Utilities.ComplexFunctions
(htf_thisModulesTests) where

import Data.Complex
import Test.Framework
import Pudding.Utilities.ComplexFunctions
import Pudding.Utilities.DoubleFunctions
import Pudding.Utilities.FloatEq

prop_complexEqSelf :: (Complex Double) -> Bool
prop_complexEqSelf z = z ~= z

prop_complexEqDifferent :: (Complex Double) -> (Complex Double) -> Property
prop_complexEqDifferent z1 z2 =
  (not (realPart z1 ~= realPart z2) ||
  not (imagPart z1 ~= imagPart z2))
    ==> not $ z1 ~= z2

prop_scaleComplexScales :: Double -> (Complex Double) -> Bool
prop_scaleComplexScales d z =
  let z' = scaleComplex d z in
    realPart z' ~= (d * (realPart z)) &&
    imagPart z' ~= (d * (imagPart z))

