{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Observables.Energy
(htf_thisModulesTests) where

import Data.Complex (imagPart, realPart)
import Pudding.Observables.Energy
import Pudding.Observables.SimpleObservable (evaluate)
import Pudding.Test.Aux (getSphere)
import Pudding.Types.Configuration
import Pudding.Utilities.ComplexFunctions (complexEq)
import Test.Framework

prop_energySphereReal :: (Positive Int) -> Int -> Property
prop_energySphereReal (Positive n) genInt =
  (n > 1) ==>
    imagPart (evaluate energy $ c) == 0 where
      c = getSphere n genInt

prop_energySpherePositive :: (Positive Int) -> Int -> Property
prop_energySpherePositive (Positive n) genInt =
  (n > 1) ==>
    realPart (evaluate energy $ c) > 0 where
      c = getSphere n genInt

prop_energySphereSymmetric :: (Positive Int) -> (Positive Int) -> (Positive Int) -> Int -> Property
prop_energySphereSymmetric (Positive n) (Positive p) (Positive q) genInt =
  (p < n && q < n) ==>
    (evaluate energy $ c) `complexEq` (evaluate energy $ c') where
      c = getSphere n genInt
      mc = swap p q c
      c' = case mc of
        Nothing -> emptySphere
        Just val -> val
