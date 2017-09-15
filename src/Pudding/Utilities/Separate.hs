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
  neutral :: a
  estimateSimple :: Estimator a
  blockEstimateSimple :: Int -> Estimator a
  withinBounds :: Result a -> Result a -> Bool

toResult :: (Double, Double) -> (Result Double)
toResult (mean, variance) =
  Result {
    mean,
    stdDev = sqrt variance
  }

blockSize :: Int -> Int -> Int -> Int
blockSize nVals n k = min n (nVals - n * k)

getBlocks :: Int -> (U.Vector Double) -> Either String (U.Vector Double)
getBlocks n u = if n < 1 then Left "The number of values in each block must be positive"
  else do
    let nVals = U.length u
    let nBlocks = nVals `quot` n
    let gen k = S.mean $ U.slice (k * n) (blockSize nVals n k) u
    if nBlocks < 1
      then Left $ (show n) ++ "is too many values per block for " ++ (show $ nVals) ++ " values"
      else return $ U.generate nBlocks gen

instance Separate Double where
  neutral = 0
  estimateSimple = Right . toResult . S.meanVariance
  blockEstimateSimple n xs = do
    blocks <- getBlocks n xs
    estimateSimple blocks
  withinBounds (Result { mean = m1, stdDev = s1 })
               (Result { mean = m2, stdDev = s2 })
    = abs (m2 - m1) < (s1 + s2)

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

sepComplexResult :: (Separate a) => Result (Complex a) -> [Result a]
sepComplexResult (Result { mean = m, stdDev = s }) =
  [realResult, imagResult] where
    realResult = Result { mean = realPart m, stdDev = realPart s}
    imagResult = Result { mean = imagPart m, stdDev = imagPart s}

instance (Separate a) => Separate (Complex a) where
  neutral = neutral :+ neutral
  estimateSimple =
    complexify (estimateSimple :: (Separate a) => Estimator a)
  blockEstimateSimple n =
    complexify (blockEstimateSimple n :: (Separate a) => Estimator a)
  withinBounds z1 z2 =
    and $ zipWith withinBounds 
                  (sepComplexResult z1)
                  (sepComplexResult z2)
