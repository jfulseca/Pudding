{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Observables.SimpleObservable
(htf_thisModulesTests) where

import qualified Data.Vector.Unboxed as V
import Pudding.Observables.Observable
import Pudding.Observables.SimpleObservable
import Pudding.Test.Aux (getSphere, getSphereSamples)
import Pudding.Types.Configuration
import Pudding.Types.Internal.PolarAngles
import Pudding.Utilities.FloatEq
import Pudding.Utilities.Separate
import Test.Framework hiding (Result)

testObsOne :: (SimpleObservable Double)
testObsOne = \_ -> (1 :: Double)

test_obsOne = assertEqual (1 :: Double) $
  (testObsOne (getSphere 3 1))

prop_liftedObsOne :: (Positive Int) -> (Positive Int) -> Int -> Bool
prop_liftedObsOne (Positive n) (Positive k) genInt =
  mean ~= 1.0 where
    samples = getSphereSamples n k genInt  
    Result { mean } = liftSimple testObsOne $ samples

sumTheta :: Configuration -> Double
sumTheta (Sphere { angles }) =
  V.foldl (\acc (PolarAngles { polar }) -> acc + polar) 0.0 angles

simpleObsSumTheta :: SimpleObservable Double
simpleObsSumTheta = sumTheta

prop_simpleObsSumTheta :: (Positive Int) -> (Positive Int) -> Int -> Bool
prop_simpleObsSumTheta (Positive n) (Positive k) genInt =
  result == resultCheck where
  samples = getSphereSamples n k genInt
  result = liftSimple simpleObsSumTheta $ samples
  thetaSums = V.fromList $ map sumTheta samples
  resultCheck = estimateSimple thetaSums

