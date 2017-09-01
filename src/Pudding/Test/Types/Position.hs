{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Types.Position 
(htf_thisModulesTests) where

import Test.Framework
import Pudding.Test.Aux (assertTrue)
import Pudding.Test.Types.SphereAngles ()
import Pudding.Types.Internal.Position
import Pudding.Types.Internal.SphereAngles

instance Arbitrary Position where
  arbitrary =
    oneof [ SpherePosition <$> arbitrary ]

sphereNorth :: Position
sphereNorth = placeOnSphere 0 0

prop_sphereEqSelf :: Position -> Bool
prop_sphereEqSelf p = p == p

prop_sphereEqDifferent :: Position -> Position -> Property
prop_sphereEqDifferent p1@(SpherePosition (SphereAngles theta1 _)) p2@(SpherePosition (SphereAngles theta2 _)) =
  (theta1 /= 0 && theta2 /= 0) ==> p1 /= p2

prop_sphereEqNorth :: Angle -> Angle -> Bool
prop_sphereEqNorth phi1 phi2 = p1 == p2
  where p1 = placeOnSphere 0 phi1
        p2 = placeOnSphere 0 phi2

prop_sphereEqSouth :: Angle -> Angle -> Bool
prop_sphereEqSouth phi1 phi2 = p1 == p2
  where p1 = placeOnSphere pi phi1
        p2 = placeOnSphere pi phi2

prop_sphereEqNorthSouth :: Angle -> Angle -> Bool
prop_sphereEqNorthSouth phi1 phi2 = p1 /= p2
  where p1 = placeOnSphere 0 phi1
        p2 = placeOnSphere pi phi2


prop_sphereLimits :: Angle -> Angle -> Bool
prop_sphereLimits theta phi =
  let (SpherePosition (SphereAngles theta' phi')) = placeOnSphere theta phi in
    theta' >= 0 && theta' < pi && phi' >= 0 && phi' < 2 * pi

prop_spherePolarPeriodicForwardFull :: Angle -> Angle -> Bool
prop_spherePolarPeriodicForwardFull theta phi =
  p == p' where
    p = placeOnSphere theta phi
    p' = placeOnSphere (theta + 2 * pi) phi

prop_spherePolarPeriodicForwardHalf :: Angle -> Angle -> Bool
prop_spherePolarPeriodicForwardHalf theta phi =
  p == p' where
    p = placeOnSphere (theta + pi) phi
    p' = placeOnSphere (pi - theta) (phi + pi)

prop_spherePolarPeriodicBackwardFull :: Angle -> Angle -> Bool
prop_spherePolarPeriodicBackwardFull theta phi =
  p == p' where
    p = placeOnSphere theta phi
    p' = placeOnSphere (theta - 2 * pi) phi

prop_spherePolarPeriodicBackwardHalf :: Angle -> Angle -> Bool
prop_spherePolarPeriodicBackwardHalf theta phi =
  p == p' where
    p = placeOnSphere (theta - pi) phi
    p' = placeOnSphere (pi - theta) (phi + pi)

prop_sphereAzimuthalPeriodicForward :: Angle -> Angle -> Bool
prop_sphereAzimuthalPeriodicForward theta phi =
  p == p' where
    p = placeOnSphere theta phi
    p' = placeOnSphere theta (phi + 2 * pi)

prop_sphereAzimuthalPeriodicBackward :: Angle -> Angle -> Bool
prop_sphereAzimuthalPeriodicBackward theta phi =
  p == p' where
    p = placeOnSphere theta phi
    p' = placeOnSphere theta (phi - 2 * pi)

test_moveSphereNorthPolarOne =
  assertTrue $ (placeOnSphere 1 0) == (sphereNorth `move` (placeOnSphere 1 0))

test_moveSphereNorthAzimuthalOne =
  assertTrue $ (placeOnSphere 0 1) == (sphereNorth `move` (placeOnSphere 0 1))

moveTest :: Position -> (Position -> Position) -> Position -> Bool
moveTest p2 f p1 = (p1 `move` p2) == (f p1)

prop_moveSphereZero :: Position -> Bool
prop_moveSphereZero = moveTest sphereNorth moveZero where
  moveZero (SpherePosition angles) = SpherePosition . normalize $ angles

prop_moveSpherePolarOne :: Position -> Bool
prop_moveSpherePolarOne = moveTest p movePolarOne where
  p = placeOnSphere 1 0
  movePolarOne (SpherePosition (SphereAngles theta phi)) = SpherePosition . normalize $ SphereAngles (theta + 1) phi

prop_moveSphereAzimuthalOne :: Position -> Bool
prop_moveSphereAzimuthalOne = moveTest p moveAzimuthalOne where
  p = placeOnSphere 0 1
  moveAzimuthalOne (SpherePosition (SphereAngles theta phi)) = SpherePosition . normalize $ SphereAngles theta (phi + 1)
