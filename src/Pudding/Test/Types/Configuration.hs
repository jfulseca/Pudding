{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Types.Configuration
(htf_thisModulesTests) where

import qualified Data.Vector as V
import Test.Framework
import Pudding.Test.Aux (assertTrue)
import Pudding.Types.Internal.Configuration
import Pudding.Types.PolarAngles
import System.Random (mkStdGen)

prop_generateSphereLength :: Int -> (Positive Int) -> Bool
prop_generateSphereLength genInt (Positive n) =
  length v == n where
    (Sphere v) = fst (generateSphereConfiguration (mkStdGen genInt) n)
