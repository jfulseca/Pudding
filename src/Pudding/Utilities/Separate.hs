{-# LANGUAGE NamedFieldPuns #-}

module Pudding.Utilities.Separate
( Estimator
, Separate(..)
) where

import Data.Complex (Complex(..), imagPart, realPart)
import qualified Data.Vector.Unboxed as U
import Pudding.Observables.Observable (Result(..))
import qualified Statistics.Sample as S

type Estimator a = U.Vector a -> Result a

class (U.Unbox a) => Separate a where
  estimateSimple :: Estimator a
  blockEstimateSimple :: Int -> Estimator a

toResult :: (Double, Double) -> (Result Double)
toResult (mean, variance) =
  Result {
    mean,
    stdDev = sqrt variance
  }

getBlocks :: Int -> (U.Vector Double) -> (U.Vector Double)
getBlocks n u =
  let nBlocks = (U.length u) `quot` n
      gen k = S.mean $ U.slice (k * n) n u
  in U.generate nBlocks gen

instance Separate Double where
  estimateSimple = toResult . S.meanVariance
  blockEstimateSimple n xs =
    estimateSimple $ getBlocks n xs

complexify :: (Separate a) => Estimator a -> Estimator (Complex a)
complexify f cs = let
  Result { mean = rMean, stdDev = rStdDev } =
    f $ U.map realPart cs
  Result { mean = iMean, stdDev = iStdDev } =
    f $ U.map imagPart cs
  in Result {
    mean = rMean :+ iMean,
    stdDev = rStdDev :+ iStdDev
  }

instance (Separate a) => Separate (Complex a) where
  estimateSimple =
    complexify (estimateSimple :: (Separate a) => Estimator a)
  blockEstimateSimple n =
    complexify (blockEstimateSimple n :: (Separate a) => Estimator a)
