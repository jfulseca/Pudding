module Pudding.Utilities.DoubleFunctions
(
  doubleEq
, fmod
) where

roundDown :: Double -> Double
roundDown = fromIntegral . floor 

fmod :: Double -> Double -> Double
fmod _ 0 = error "fmod with second argument 0 is undefined"
fmod x y = x - n * y
  where n = roundDown $ x / y

compareDouble :: Double -> Double -> Double -> Bool
compareDouble precision d1 d2 = 
  abs ((d2 - d1) / d1) < precision

doubleEq :: Double -> Double -> Bool
doubleEq 0 d = abs d < 5e-11
doubleEq d1 d2 = compareDouble 5e-11 d1 d2
