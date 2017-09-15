{-# LANGUAGE NamedFieldPuns #-}

module Pudding.Observables.SimpleObservable
( SimpleObservable
, liftSimple
) where

import qualified Data.Vector.Unboxed as U
import Pudding.Observables.Observable
import Pudding.Observables.Separate
import Pudding.Types.Configuration (Configuration)

type SimpleObservable a = Configuration -> a

liftSimple :: (Separate a, U.Unbox a) => Estimator a -> SimpleObservable a -> Observable a
liftSimple est obs = \samples ->
  est . U.fromList $ map obs samples

