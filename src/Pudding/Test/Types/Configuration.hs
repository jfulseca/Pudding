{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Types.Configuration
(htf_thisModulesTests) where

import qualified Data.Vector as V
import Test.Framework
import Pudding.Test.Aux (assertTrue)
import Pudding.Types.Internal.Configuration
import Pudding.Types.Position

test_createExampleSphereConfiguration =
  assertTrue $ V.length (V.fromList [placeOnSphere 0 0]) == 1
