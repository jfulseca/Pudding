{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Observables.SimpleObservable
(htf_thisModulesTests) where
import Debug.Trace
import Data.Complex (Complex(..))
import Data.Either (isLeft, isRight)
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
  let samples = getSphereSamples n k genInt  
      result = liftSimple estimateSimple testObsOne $ samples
  in either (\_ -> False)
            (\Result { mean } -> mean ~= 1.0)
            result

prop_liftedObsOneBlock :: (Positive Int) -> (Positive Int) -> Int -> Property
prop_liftedObsOneBlock (Positive n) (Positive k) genInt = (n > 10) ==>
  let samples = getSphereSamples n k genInt  
      result = liftSimple (blockEstimateSimple (n `quot` 10)) testObsOne $ samples
  in either (\_ -> False)
            (\Result { mean } -> mean ~= 1.0)
            result

prop_liftedObsOneBlockSmall :: (Positive Int) -> (Positive Int) -> Int -> Property
prop_liftedObsOneBlockSmall (Positive n) (Positive b) genInt = (b <= n) ==>
  let samples = getSphereSamples n 10 genInt  
      result = liftSimple (blockEstimateSimple b) testObsOne $ samples
  in isRight result

prop_liftedObsOneBlockLarge :: (Positive Int) -> (Positive Int) -> Int -> Property
prop_liftedObsOneBlockLarge (Positive n) (Positive b) genInt = (b > n) ==>
  let samples = getSphereSamples n 10 genInt  
      result = liftSimple (blockEstimateSimple b) testObsOne $ samples
  in isLeft result

checkSimpleObservable :: (Show a, FloatEq a, Eq a, V.Unbox a, Separate a) => (Configuration -> a) -> (Positive Int) -> (Positive Int) -> Int -> Bool
checkSimpleObservable f (Positive n) (Positive k) genInt =
  result == resultCheck
  where samples = getSphereSamples n k genInt
        result = liftSimple estimateSimple f $ samples
        resultCheck =
          (estimateSimple . V.fromList) $
            map f samples

sumTheta :: Configuration -> Double
sumTheta (Sphere { angles }) =
  V.foldl (\acc (PolarAngles { polar }) -> acc + polar) 0.0 angles

prop_sumTheta :: (Positive Int) -> (Positive Int) -> Int -> Bool
prop_sumTheta = trace "propsumtheta" $ checkSimpleObservable sumTheta

mulV :: Configuration -> (Complex Double)
mulV (Sphere { spinors }) =
  V.foldl (\acc (SpinorCoordinates { vCoordinate }) -> acc * vCoordinate)
          (1.0 :+ 0.0)
          spinors

prop_mulV :: (Positive Int) -> (Positive Int) -> Int -> Bool
prop_mulV = checkSimpleObservable mulV

divides :: Int -> Int -> Bool
divides n = (==0) . (flip mod $ n)

getMean :: Either String (Result a) -> Maybe a
getMean = either (\_ -> Nothing :: Maybe a)
                 (\Result { mean } -> Just mean)

checkSkew :: (Separate a) => Either String (Result a) -> Either String (Result a) -> Either String Bool
checkSkew eResult eBlockResult = do
  result <- eResult
  blockResult <- eBlockResult
  return $ withinBounds result blockResult

checkSimpleBlockMean :: (Show a, FloatEq a, Eq a, V.Unbox a, Separate a) => (Configuration -> a) -> (Positive Int) -> (Positive Int) -> Int -> Bool
checkSimpleBlockMean f (Positive n) (Positive b) genInt
  | b > n = isLeft blockResult
  | b `divides` n = blockMean ~= mean
  | otherwise =
      either (\_ -> False)
             id
             (checkSkew result blockResult)
  where samples = getSphereSamples n 10 genInt
        blockResult = liftSimple (blockEstimateSimple b) f $ samples
        blockMean = getMean blockResult
        result = liftSimple estimateSimple f $ samples
        mean = getMean result

prop_sumThetaBlockMean :: (Positive Int) -> (Positive Int) -> Int -> Bool
prop_sumThetaBlockMean =
  checkSimpleBlockMean sumTheta

prop_mulVBlockMean :: (Positive Int) -> (Positive Int) -> Int -> Bool
prop_mulVBlockMean =
  checkSimpleBlockMean mulV
