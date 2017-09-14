{-# LANGUAGE NamedFieldPuns #-}

module Pudding.Utilities.Separate
( Separate(..)
) where

import Data.Complex (Complex(..), imagPart, realPart)
import qualified Data.Vector.Unboxed as U
import Pudding.Observables.Observable (Result(..))
import Statistics.Sample (meanVariance)

class (U.Unbox a) => Separate a where
  estimateSimple :: (U.Vector a) -> (Result a)

toResult :: (Double, Double) -> (Result Double)
toResult (mean, variance) =
  Result {
    mean,
    stdDev = sqrt variance
  }

instance Separate Double where
  estimateSimple = toResult . meanVariance

instance (Separate a) => Separate (Complex a) where
  estimateSimple cs = let
    Result { mean = rMean, stdDev = rStdDev } =
      estimateSimple $ U.map realPart cs
    Result { mean = iMean, stdDev = iStdDev } =
      estimateSimple $ U.map imagPart cs
    in Result {
      mean = rMean :+ iMean,
      stdDev = rStdDev :+ iStdDev
    }
