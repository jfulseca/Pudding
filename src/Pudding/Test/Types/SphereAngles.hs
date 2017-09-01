{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Types.SphereAngles
(htf_thisModulesTests) where

import Data.Complex hiding (polar)
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

south :: SphereAngles
south = SphereAngles pi 0

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

prop_chordLengthZero :: SphereAngles -> Bool
prop_chordLengthZero a =
  (a `chordLength` a) `doubleEq` 0

prop_chordLengthOppositePolar :: SphereAngles -> Bool
prop_chordLengthOppositePolar a =
  (a `chordLength` opposite) `doubleEq` 2 where
    opposite = a `rotate` south

prop_chordLengthHalfPolar :: SphereAngles -> Bool
prop_chordLengthHalfPolar a =
  (a `chordLength` half) `doubleEq` (sqrt 2) where
    half = a `rotate` (SphereAngles (pi / 2) 0)

prop_chordLengthOppositeAzimuthal :: SphereAngles -> Bool
prop_chordLengthOppositeAzimuthal a@(SphereAngles theta _) =
  (a `chordLength` opposite) `doubleEq` (2 * (abs $ sin theta)) where
    opposite = a `rotate` (SphereAngles 0 pi)  

prop_chordLengthHalfAzimuthal :: SphereAngles -> Bool
prop_chordLengthHalfAzimuthal a@(SphereAngles theta _) =
  (a `chordLength` half) `doubleEq` ((sqrt 2) * (abs $ sin theta)) where
    half = a `rotate` (SphereAngles 0 (pi / 2))  
