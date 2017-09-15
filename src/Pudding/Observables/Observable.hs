{-# LANGUAGE FlexibleInstances #-}

module Pudding.Observables.Observable
( Estimator
, Observable
, Result(..)
, SafeResult
) where

import qualified Data.Vector.Unboxed as U
import Pudding.Types.Configuration (Samples)
import Pudding.Utilities.FloatEq

data Result a = Result {
  mean :: a
, stdDev :: a
} deriving (Eq, Show)

instance (FloatEq a) => FloatEq (Result a) where
  (~=) (Result { mean = m1, stdDev = s1 }) 
       (Result { mean = m2, stdDev = s2 }) 
    = m1 ~= m2 && s1 ~= s2

type SafeResult a = Either String (Result a)

instance (FloatEq a) => FloatEq (SafeResult a) where
  Left _ ~= Left _ = False
  Right x ~= Right y = x ~= y
  _ ~= _ = False

type Estimator a = U.Vector a -> SafeResult a

type Observable a = Samples -> SafeResult a
