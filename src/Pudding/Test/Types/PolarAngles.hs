{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Types.PolarAngles
(htf_thisModulesTests) where

import Data.Complex hiding (polar)
import Test.Framework
import Pudding.Test.Aux (assertTrue)
import Pudding.Types.Internal.PolarAngles
import Pudding.Utilities.DoubleFunctions
import Pudding.Utilities.FloatEq

instance Arbitrary PolarAngles where
  arbitrary = do
    theta <- arbitrary
    phi <- arbitrary
    return $ PolarAngles theta phi

north :: PolarAngles
north = PolarAngles 0 0

south :: PolarAngles
south = PolarAngles pi 0

prop_limitAngleLimit :: Angle -> Bool
prop_limitAngleLimit a = limitAngle a <= 2 * pi

prop_limitAnglePeriod :: Angle -> Bool
prop_limitAnglePeriod a =
  (limitAngle a) ~= (limitAngle $ a + 2 * pi)

test_northSpinor =
  assertTrue $ (uCoordinate northSpinor) ~= (1 :+ 0 ) &&
  (vCoordinate northSpinor) ~= (0 :+ 0)
    where northSpinor = toSpinor north

prop_spinorBounds :: PolarAngles -> Bool
prop_spinorBounds a =
  (abs . realPart $ uCoordinate s) <= 1 && 
  (abs . realPart $ vCoordinate s) <=1
    where s = toSpinor a

prop_chordLengthZero :: PolarAngles -> Bool
prop_chordLengthZero a =
  (a `chordLength` a) ~= 0

prop_chordLengthOppositePolar :: PolarAngles -> Bool
prop_chordLengthOppositePolar a =
  (a `chordLength` opposite) ~= 2 where
    opposite = a `rotate` south

prop_chordLengthHalfPolar :: PolarAngles -> Bool
prop_chordLengthHalfPolar a =
  (a `chordLength` half) ~= (sqrt 2) where
    half = a `rotate` (PolarAngles (pi / 2) 0)

prop_chordLengthOppositeAzimuthal :: PolarAngles -> Bool
prop_chordLengthOppositeAzimuthal a@(PolarAngles theta _) =
  (a `chordLength` opposite) ~= (2 * (abs $ sin theta)) where
    opposite = a `rotate` (PolarAngles 0 pi)  

prop_chordLengthHalfAzimuthal :: PolarAngles -> Bool
prop_chordLengthHalfAzimuthal a@(PolarAngles theta _) =
  (a `chordLength` half) ~= ((sqrt 2) * (abs $ sin theta)) where
    half = a `rotate` (PolarAngles 0 (pi / 2))  

prop_sphereEqSelf :: PolarAngles -> Bool
prop_sphereEqSelf p = p ~= p

prop_sphereEqDifferent :: PolarAngles -> PolarAngles -> Property
prop_sphereEqDifferent p1@(PolarAngles theta1 _) p2@(PolarAngles theta2 _) =
  (theta1 /= 0 && theta2 /= 0) ==> p1 ~/ p2

prop_sphereEqNorth :: Angle -> Angle -> Bool
prop_sphereEqNorth phi1 phi2 = p1 ~= p2
  where p1 = placeOnSphere 0 phi1
        p2 = placeOnSphere 0 phi2

prop_sphereEqSouth :: Angle -> Angle -> Bool
prop_sphereEqSouth phi1 phi2 = p1 ~= p2
  where p1 = placeOnSphere pi phi1
        p2 = placeOnSphere pi phi2

prop_sphereEqNorthSouth :: Angle -> Angle -> Bool
prop_sphereEqNorthSouth phi1 phi2 = p1 ~/ p2
  where p1 = placeOnSphere 0 phi1
        p2 = placeOnSphere pi phi2

prop_sphereLimits :: Angle -> Angle -> Bool
prop_sphereLimits theta phi =
  let (PolarAngles theta' phi') = placeOnSphere theta phi in
    theta' >= 0 && theta' < pi && phi' >= 0 && phi' < 2 * pi

prop_spherePolarPeriodicForwardFull :: Angle -> Angle -> Bool
prop_spherePolarPeriodicForwardFull theta phi =
  p ~= p' where
    p = placeOnSphere theta phi
    p' = placeOnSphere (theta + 2 * pi) phi

prop_spherePolarPeriodicForwardHalf :: Angle -> Angle -> Bool
prop_spherePolarPeriodicForwardHalf theta phi =
  p ~= p' where
    p = placeOnSphere (theta + pi) phi
    p' = placeOnSphere (pi - theta) (phi + pi)

prop_spherePolarPeriodicBackwardFull :: Angle -> Angle -> Bool
prop_spherePolarPeriodicBackwardFull theta phi =
  p ~= p' where
    p = placeOnSphere theta phi
    p' = placeOnSphere (theta - 2 * pi) phi

prop_spherePolarPeriodicBackwardHalf :: Angle -> Angle -> Bool
prop_spherePolarPeriodicBackwardHalf theta phi =
  p ~= p' where
    p = placeOnSphere (theta - pi) phi
    p' = placeOnSphere (pi - theta) (phi + pi)

prop_sphereAzimuthalPeriodicForward :: Angle -> Angle -> Bool
prop_sphereAzimuthalPeriodicForward theta phi =
  p ~= p' where
    p = placeOnSphere theta phi
    p' = placeOnSphere theta (phi + 2 * pi)

prop_sphereAzimuthalPeriodicBackward :: Angle -> Angle -> Bool
prop_sphereAzimuthalPeriodicBackward theta phi =
  p ~= p' where
    p = placeOnSphere theta phi
    p' = placeOnSphere theta (phi - 2 * pi)

prop_rotateZero :: PolarAngles -> Bool
prop_rotateZero a = a ~= a' where
  a' = a `rotate` north

rotateTest :: (PolarAngles -> PolarAngles) -> PolarAngles -> PolarAngles -> Bool
rotateTest f a2 a1 = (f a1) ~= (a1 `rotate` a2)

prop_rotatePolarOne :: PolarAngles -> Bool
prop_rotatePolarOne = rotateTest rotPolOne (PolarAngles 1 0) where
  rotPolOne (PolarAngles theta phi) =
    placeOnSphere (theta + 1) phi

prop_rotatePolarBackOne :: PolarAngles -> Bool
prop_rotatePolarBackOne = rotateTest rotPolBackOne (PolarAngles (-1) 0) where
  rotPolBackOne (PolarAngles theta phi) =
    placeOnSphere (theta - 1) phi

prop_rotateAzimuthOne :: PolarAngles -> Bool
prop_rotateAzimuthOne = rotateTest rotAzmOne (PolarAngles 0 1) where
  rotAzmOne (PolarAngles theta phi) =
    placeOnSphere theta (phi + 1)

prop_rotateAzimuthBackOne :: PolarAngles -> Bool
prop_rotateAzimuthBackOne = rotateTest rotAzmBackOne (PolarAngles 0 (-1)) where
  rotAzmBackOne (PolarAngles theta phi) =
    placeOnSphere theta (phi - 1)

prop_rotatePolarTen :: PolarAngles -> Bool
prop_rotatePolarTen = rotateTest rotPolTen (PolarAngles 10 0) where
  rotPolTen (PolarAngles theta phi) =
    placeOnSphere (theta + 10) phi

prop_rotatePolarTwoPi :: PolarAngles -> Bool
prop_rotatePolarTwoPi =
  rotateTest id (PolarAngles (2 * pi) 0)

prop_rotatePolarBackTwoPi :: PolarAngles -> Bool
prop_rotatePolarBackTwoPi =
  rotateTest id (PolarAngles ((-2) * pi) 0)

prop_rotateAzimuthTwoPi :: PolarAngles -> Bool
prop_rotateAzimuthTwoPi =
  rotateTest id (PolarAngles 0 (2 * pi))

prop_rotatePolarFourPi :: PolarAngles -> Bool
prop_rotatePolarFourPi =
  rotateTest id (PolarAngles (4 * pi) 0)
