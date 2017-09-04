module Pudding.Observables.SimpleObservable
( SimpleObservable (..)
) where

import Data.Complex
import Pudding.Types.Configuration

data SimpleObservable = SimpleObservable {
  evaluate :: Configuration -> (Complex Double)
, name :: String
}

instance Show SimpleObservable where
  show = name
