module Pudding.Utilities.ComplexFunctions
(
  scaleComplex
) where

import Data.Complex

scaleComplex :: Double -> Complex Double -> Complex Double
scaleComplex a b = (a :+ 0) * b
