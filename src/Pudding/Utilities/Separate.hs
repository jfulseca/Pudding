{-# LANGUAGE NamedFieldPuns #-}

module Pudding.Utilities.Separate
( Estimator
, Separate(..)
) where

import Data.Complex (Complex(..), imagPart, realPart)
import qualified Data.Vector.Unboxed as U
import Pudding.Observables.Observable (Result(..))
import qualified Statistics.Sample as S

type Estimator a = U.Vector a -> Either String (Result a)

class (U.Unbox a) => Separate a where
  estimateSimple :: Estimator a
  blockEstimateSimple :: Int -> Estimator a

toResult :: (Double, Double) -> (Result Double)
toResult (mean, variance) =
  Result {
    mean,
    stdDev = sqrt variance
  }

getBlocks :: Int -> (U.Vector Double) -> Either String (U.Vector Double)
getBlocks n u = do
  let nBlocks = (U.length u) `quot` n
  let gen k = S.mean $ U.slice (k * n) n u
  if nBlocks < 1
    then Left $ (show n) ++ "is too many values per block for " ++ (show $ U.length u) ++ " values"
    else return $ U.generate nBlocks gen

instance Separate Double where
  estimateSimple = Right . toResult . S.meanVariance
  blockEstimateSimple n xs = do
    blocks <- getBlocks n xs
    estimateSimple blocks

complexify :: (Separate a) => Estimator a -> Estimator (Complex a)
complexify f cs = do
  Result { mean = rMean, stdDev = rStdDev } <-
    f $ U.map realPart cs
  Result { mean = iMean, stdDev = iStdDev } <-
    f $ U.map imagPart cs
  Right $ Result {
    mean = rMean :+ iMean,
    stdDev = rStdDev :+ iStdDev
  }

instance (Separate a) => Separate (Complex a) where
  estimateSimple =
    complexify (estimateSimple :: (Separate a) => Estimator a)
  blockEstimateSimple n =
    complexify (blockEstimateSimple n :: (Separate a) => Estimator a)
