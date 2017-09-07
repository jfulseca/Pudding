module Pudding.Observables.SimpleObservable
( SimpleObservable
, liftSimple
) where

import Pudding.Observables.Observable
import Pudding.Types.Configuration (Configuration)

type SimpleObservable a = Configuration -> a

liftSimple :: (SimpleObservable a) -> (Observable a)
liftSimple = undefined
