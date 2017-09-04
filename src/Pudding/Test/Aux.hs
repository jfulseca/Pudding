module Pudding.Test.Aux
(
  assertTrue
, getSphere
) where

import Pudding.Types.Configuration
import System.Random (mkStdGen)
import Test.Framework
import Test.HUnit

assertTrue :: Bool -> Assertion
assertTrue = assertEqual "Assert True" True

getSphere :: Int -> Int -> Configuration
getSphere n genInt =
  fst $ generateSphereConfiguration (mkStdGen genInt) n
