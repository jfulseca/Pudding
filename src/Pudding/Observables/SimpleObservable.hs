{-# LANGUAGE NamedFieldPuns #-}

module Pudding.Observables.SimpleObservable
( SimpleObservable
, liftSimple
) where

import qualified Data.Vector.Unboxed as U
import Pudding.Observables.Observable
import Pudding.Types.Configuration (Configuration)
import Pudding.Utilities.Separate

type SimpleObservable a = Configuration -> a

liftSimple :: (Separate a, U.Unbox a) => (SimpleObservable a) -> (Observable a)
liftSimple obs = \samples ->
  estimateSimple . U.fromList $ map obs samples

