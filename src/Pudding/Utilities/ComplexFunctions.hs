module Pudding.Utilities.ComplexFunctions
(
  cabs
, complexEq
, scaleComplex
) where

import Data.Complex
import Pudding.Utilities.DoubleFunctions

complexEq :: Complex Double -> Complex Double -> Bool
complexEq a b = (realPart a) `doubleEq` (realPart b) &&
    (imagPart a) `doubleEq` (imagPart b) 

scaleComplex :: Double -> Complex Double -> Complex Double
scaleComplex a b = (a :+ 0) * b

cabs :: (Complex Double) -> Double
cabs = realPart . abs
