{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Types.Configuration
(htf_thisModulesTests) where

import qualified Data.Vector as V
import Test.Framework
import Pudding.Test.Aux (assertTrue)
import Pudding.Types.Internal.Configuration
import Pudding.Types.Position
import System.Random (mkStdGen)

test_createExampleSphereConfiguration =
  assertTrue $ V.length (V.fromList [placeOnSphere 0 0]) == 1

prop_generateSphereLength :: Int -> (Positive Int) -> Bool
prop_generateSphereLength genInt (Positive n) =
  length (fst (generateSphereConfiguration (mkStdGen genInt) n)) == n
