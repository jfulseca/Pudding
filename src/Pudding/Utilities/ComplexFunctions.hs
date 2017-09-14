{-# LANGUAGE FlexibleInstances #-}

module Pudding.Utilities.ComplexFunctions
( cabs
, scaleComplex
) where

import Data.Complex
import Pudding.Utilities.FloatEq
import Pudding.Utilities.DoubleFunctions

complexEq :: Complex Double -> Complex Double -> Bool
complexEq a b = (realPart a) ~= (realPart b) &&
    (imagPart a) ~= (imagPart b) 

scaleComplex :: Double -> Complex Double -> Complex Double
scaleComplex a b = (a :+ 0) * b

cabs :: (Complex Double) -> Double
cabs = realPart . abs

instance FloatEq (Complex Double) where
  (~=) = complexEq
