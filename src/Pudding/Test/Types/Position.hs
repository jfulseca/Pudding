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

prop_sphereLimits :: Angle -> Angle -> Bool
prop_sphereLimits theta phi =
  let (SpherePosition (SphereAngles theta' phi')) = placeOnSphere theta phi in
    theta' >= 0 && theta' < pi && phi' >= 0 && phi' < 2 * pi

prop_spherePolarPeriodicForwardFull :: Angle -> Angle -> Bool
prop_spherePolarPeriodicForwardFull theta phi =
  p `positionEq` p' where
    p = placeOnSphere theta phi
    p' = placeOnSphere (theta + 2 * pi) phi

prop_spherePolarPeriodicForwardHalf :: Angle -> Angle -> Bool
prop_spherePolarPeriodicForwardHalf theta phi =
  p `positionEq` p' where
    p = placeOnSphere (theta + pi) phi
    p' = placeOnSphere (pi - theta) (phi + pi)

prop_spherePolarPeriodicBackwardFull :: Angle -> Angle -> Bool
prop_spherePolarPeriodicBackwardFull theta phi =
  p `positionEq` p' where
    p = placeOnSphere theta phi
    p' = placeOnSphere (theta - 2 * pi) phi

prop_spherePolarPeriodicBackwardHalf :: Angle -> Angle -> Bool
prop_spherePolarPeriodicBackwardHalf theta phi =
  p `positionEq` p' where
    p = placeOnSphere (theta - pi) phi
    p' = placeOnSphere (pi - theta) (phi + pi)

prop_sphereAzimuthalPeriodicForward :: Angle -> Angle -> Bool
prop_sphereAzimuthalPeriodicForward theta phi =
  p `positionEq` p' where
    p = placeOnSphere theta phi
    p' = placeOnSphere theta (phi + 2 * pi)

prop_sphereAzimuthalPeriodicBackward :: Angle -> Angle -> Bool
prop_sphereAzimuthalPeriodicBackward theta phi =
  p `positionEq` p' where
    p = placeOnSphere theta phi
    p' = placeOnSphere theta (phi - 2 * pi)

test_moveSphereNorthPolarOne =
  assertTrue $ (placeOnSphere 1 0) `positionEq` (sphereNorth `move` (placeOnSphere 1 0))

test_moveSphereNorthAzimuthalOne =
  assertTrue $ (placeOnSphere 0 1) `positionEq` (sphereNorth `move` (placeOnSphere 0 1))

moveTest :: Position -> (Position -> Position) -> Position -> Bool
moveTest p2 f p1 = (p1 `move` p2) `positionEq` (f p1)

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
