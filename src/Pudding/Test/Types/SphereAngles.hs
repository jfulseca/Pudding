{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Types.SphereAngles
(htf_thisModulesTests) where

import Data.Complex
import Test.Framework
import Pudding.Test.Aux (assertTrue)
import Pudding.Types.Internal.SphereAngles
import Pudding.Utilities.DoubleFunctions
import Pudding.Utilities.ComplexFunctions

instance Arbitrary SphereAngles where
  arbitrary = do
    theta <- arbitrary
    phi <- arbitrary
    return $ SphereAngles theta phi

north :: SphereAngles
north = SphereAngles 0 0

prop_limitAngleLimit :: Angle -> Bool
prop_limitAngleLimit a = limitAngle a <= 2 * pi

prop_limitAnglePeriod :: Angle -> Bool
prop_limitAnglePeriod a =
  (limitAngle a) `doubleEq` (limitAngle $ a + 2 * pi)

test_northSpinor =
  assertTrue $ (uCoordinate northSpinor) `complexEq` (1 :+ 0 ) &&
  (vCoordinate northSpinor) `complexEq` (0 :+ 0)
    where northSpinor = toSpinor north

prop_spinorBounds :: SphereAngles -> Bool
prop_spinorBounds a =
  (abs . realPart $ uCoordinate s) <= 1 && 
  (abs . realPart $ vCoordinate s) <=1
    where s = toSpinor a