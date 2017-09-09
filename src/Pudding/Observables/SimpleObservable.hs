{-# LANGUAGE NamedFieldPuns #-}

module Pudding.Observables.SimpleObservable
( SimpleObservable
, liftSimple
) where

import Pudding.Observables.Observable
import Pudding.Types.Configuration (Configuration)
import Pudding.Utilities.Separate

type SimpleObservable a = Configuration -> a

liftSimple :: (Separate a) => (SimpleObservable a) -> (Observable a)
liftSimple obs = \samples -> let
  values = map obs samples
  (squareSummed, summed, number) =
    foldl (\(sqAcc, acc, n) x -> (sqAcc `sadd` x `smul` x, acc `sadd` x, n + 1.0))
          (neutral, neutral, 0 :: Double)
          values
  fraction = 1 / number :: Double
  mean = fraction `sscale` summed
  stdDev = ssqrt $
    (fraction `sscale` squareSummed) `ssub`
   ((fraction * fraction) `sscale` (summed `smul` summed))
  in Result { mean, stdDev }

