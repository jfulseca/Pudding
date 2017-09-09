module Pudding.Observables.Observable
( Observable
, Result(..)
) where

import Pudding.Types.Configuration (Samples)

data Result a = Result {
  mean :: a
, stdDev :: a
}

type Observable a = Samples -> (Result a)
