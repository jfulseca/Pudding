{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Observables.SimpleObservable
(htf_thisModulesTests) where

import Pudding.Observables.SimpleObservable
import Pudding.Test.Aux (assertTrue)
import Pudding.Types.Configuration (generateSphereConfiguration)
import Pudding.Utilities.ComplexFunctions (complexEq)
import Test.Framework
import System.Random (mkStdGen)

testObs :: SimpleObservable
testObs = SimpleObservable {
  evaluate = \_ -> 0
, name = "Test"
} 

test_createSimpleObservable = assertTrue $
  evaluate testObs c `complexEq` 0 &&
  name testObs == show testObs where
    c = fst $ generateSphereConfiguration (mkStdGen 5) 2
