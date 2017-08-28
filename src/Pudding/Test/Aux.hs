module Pudding.Test.Aux
(
  assertTrue
) where

import Test.Framework
import Test.HUnit

assertTrue :: Bool -> Assertion
assertTrue = assertEqual "Assert True" True
