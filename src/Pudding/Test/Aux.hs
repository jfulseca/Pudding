module Pudding.Test.Aux
( assertTrue
, getSphere
, getSphereSamples
) where

import Pudding.Types.Configuration
import System.Random (mkStdGen, randoms)
import Test.HUnit

assertTrue :: Bool -> Assertion
assertTrue = assertEqual "Assert True" True

getSphere :: Int -> Int -> Configuration
getSphere n genInt =
  fst $ generateSphereConfiguration (mkStdGen genInt) n

getSphereSamples :: Int -> Int -> Int -> Samples
getSphereSamples n k genInt =
  map (getSphere k) genInts where
    genInts = take n $ randoms (mkStdGen genInt)
