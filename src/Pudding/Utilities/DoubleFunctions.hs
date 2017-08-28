module Pudding.Utilities.DoubleFunctions
(
  doubleEq
) where

compareDouble :: Double -> Double -> Double -> Bool
compareDouble precision d1 d2 = 
  abs ((d2 - d1) / d1) < precision

doubleEq :: Double -> Double -> Bool
doubleEq 0 d = d == 0
doubleEq d 0 = d == 0
doubleEq d1 d2 = compareDouble 1e-13 d1 d2
