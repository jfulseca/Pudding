{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Observables.SimpleObservable
(htf_thisModulesTests) where

import Data.Complex (Complex(..))
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
    Result { mean } = liftSimple estimateSimple testObsOne $ samples

prop_liftedObsOneBlock :: (Positive Int) -> (Positive Int) -> Int -> Property
prop_liftedObsOneBlock (Positive n) (Positive k) genInt = (n > 10) ==>
  mean ~= 1.0 where
    samples = getSphereSamples n k genInt  
    Result { mean } =
      liftSimple (blockEstimateSimple (n `quot` 10)) testObsOne $ samples

checkSimpleObservable :: (Eq a, V.Unbox a, Separate a) => (Configuration -> a) -> (Positive Int) -> (Positive Int) -> Int -> Bool
checkSimpleObservable f (Positive n) (Positive k) genInt =
  result == resultCheck where
  samples = getSphereSamples n k genInt
  result = liftSimple estimateSimple f $ samples
  resultCheck = (estimateSimple . V.fromList) $
    map f samples

sumTheta :: Configuration -> Double
sumTheta (Sphere { angles }) =
  V.foldl (\acc (PolarAngles { polar }) -> acc + polar) 0.0 angles

prop_sumTheta :: (Positive Int) -> (Positive Int) -> Int -> Bool
prop_sumTheta = checkSimpleObservable sumTheta

mulV :: Configuration -> (Complex Double)
mulV (Sphere { spinors }) =
  V.foldl (\acc (SpinorCoordinates { vCoordinate }) -> acc * vCoordinate)
          (1.0 :+ 0.0)
          spinors

prop_mulV :: (Positive Int) -> (Positive Int) -> Int -> Bool
prop_mulV = checkSimpleObservable mulV
