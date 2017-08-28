{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Types.Position 
(htf_thisModulesTests) where

import Test.Framework
import Pudding.Test.Aux (assertTrue)
import Pudding.Test.Types.SphereAngles
import Pudding.Types.Internal.Position
import Pudding.Types.Internal.SphereAngles

-- instance Arbitrary SphereAngles where
--   arbitrary = do
--     theta <- arbitrary
--     phi <- arbitrary
--     return $ SphereAngles theta phi

instance Arbitrary Position where
  arbitrary =
    oneof [ SpherePosition <$> arbitrary ]

north :: Position
north = createSpherical 0 0

test_moveNorthPolarOne =
  assertTrue $ (createSpherical 1 0) `positionEq` (north `move` (createSpherical 1 0))

test_moveNorthAzimuthalOne =
  assertTrue $ (createSpherical 0 1) `positionEq` (north `move` (createSpherical 0 1))

moveTest :: Position -> (Position -> Position) -> Position -> Bool
moveTest p2 f p1 = (p1 `move` p2) `positionEq` (f p1)

prop_moveZero :: Position -> Bool
prop_moveZero = moveTest north moveZero where
  moveZero (SpherePosition angles) = SpherePosition . normalize $ angles

prop_movePolarOne :: Position -> Bool
prop_movePolarOne = moveTest p movePolarOne where
  p = createSpherical 1 0
  movePolarOne (SpherePosition (SphereAngles theta phi)) = SpherePosition . normalize $ SphereAngles (theta + 1) phi

prop_moveAzimuthalOne :: Position -> Bool
prop_moveAzimuthalOne = moveTest p moveAzimuthalOne where
  p = createSpherical 0 1
  moveAzimuthalOne (SpherePosition (SphereAngles theta phi)) = SpherePosition . normalize $ SphereAngles theta (phi + 1)
