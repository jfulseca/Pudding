{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Types.Configuration
(htf_thisModulesTests) where

import Test.Framework
import Pudding.Types.Internal.Configuration
import System.Random (mkStdGen)

prop_generateSphereLength :: Int -> (Positive Int) -> Bool
prop_generateSphereLength genInt (Positive n) =
  length v == n where
    (Sphere v) = fst (generateSphereConfiguration (mkStdGen genInt) n)
