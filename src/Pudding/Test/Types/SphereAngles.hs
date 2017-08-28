{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Types.SphereAngles
(htf_thisModulesTests) where

import Test.Framework
import Pudding.Test.Aux (assertTrue)
import Pudding.Types.Internal.SphereAngles

instance Arbitrary SphereAngles where
  arbitrary = do
    theta <- arbitrary
    phi <- arbitrary
    return $ SphereAngles theta phi

north :: SphereAngles
north = SphereAngles 0 0

-- test_moveNorthPolarOne =
--   assertTrue $ (createSpherical 1 0) `positionEq` (north `move` (createSpherical 1 0))

-- prop_moveZero :: Position -> Bool
-- prop_moveZero = moveTest north moveZero where
--   moveZero (SpherePosition angles) = SpherePosition . normalize $ angles
