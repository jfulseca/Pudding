{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Pudding.Types.Internal.PolarAngles

( Angle
, PolarAngles(..)
, SpinorCoordinates(..)
, arcLength
, limitAngle
, chordLength
, normalize
, placeOnSphere
, rotate
, spinorProduct
, toSpinor
) where

import Data.Complex (Complex((:+)), realPart)
import Data.Vector.Unboxed.Deriving
import Pudding.Utilities.ComplexFunctions
import Pudding.Utilities.FloatEq
import Pudding.Utilities.DoubleFunctions

type Angle = Double

limitAngle :: Angle -> Angle
limitAngle = flip fmod $ 2 * pi

data PolarAngles = PolarAngles {
  polar :: Angle
, azimuthal :: Angle
} deriving (Show)

data SpinorCoordinates = SpinorCoordinates {
  uCoordinate :: Complex Double
, vCoordinate :: Complex Double
} deriving (Show)

toSpinor :: PolarAngles -> SpinorCoordinates
toSpinor (PolarAngles theta phi) =
  SpinorCoordinates {
    uCoordinate = cos (theta / 2) `scaleComplex` (cosPhiHalf :+ sinPhiHalf)
  , vCoordinate = sin (theta / 2) `scaleComplex` (cosPhiHalf :+ (-sinPhiHalf))
  } where
    cosPhiHalf = cos $ phiHalf
    sinPhiHalf = sin $ phiHalf
    phiHalf = phi / 2

spinorProduct :: SpinorCoordinates -> SpinorCoordinates -> (Complex Double)
spinorProduct (SpinorCoordinates u1 v1) (SpinorCoordinates u2 v2) =
  u1 * v2 - u2 * v1

chordLength :: PolarAngles -> PolarAngles -> Double
chordLength a1 a2 =
  2.0 * (realPart . abs $ spinorProduct s1 s2) where
    s1 = toSpinor a1
    s2 = toSpinor a2

arcLength :: PolarAngles -> PolarAngles -> Double
arcLength = undefined

normalize :: PolarAngles -> PolarAngles
normalize (PolarAngles theta phi) =
  PolarAngles theta'' (limitAngle phi') where
    theta' = limitAngle theta
    (theta'', phi')
      | theta' < 0 = (-theta', phi + pi)
      | theta' > pi = (2 * pi - theta', phi + pi)
      | otherwise = (theta', phi)

polarAnglesEq :: PolarAngles -> PolarAngles -> Bool
polarAnglesEq a1 a2
  | theta1' ~= 0 = theta2' ~= 0
  | theta2' ~= 0 = theta1' ~= 0
  | theta1' ~= pi = theta2' ~= pi
  | theta2' ~= 0 = theta1' ~= pi
  | otherwise = theta1' ~= theta2' && phi1' ~= phi2'
  where PolarAngles theta1' phi1' = normalize a1
        PolarAngles theta2' phi2' = normalize a2

spinorCoordinatesEq :: SpinorCoordinates -> SpinorCoordinates -> Bool
spinorCoordinatesEq (SpinorCoordinates u1 v1) (SpinorCoordinates u2 v2) =
  u1 ~= u2 && v1 ~= v2

instance FloatEq PolarAngles where
  (~=) = polarAnglesEq

instance FloatEq SpinorCoordinates where
  (~=) = spinorCoordinatesEq

rotate :: PolarAngles -> PolarAngles -> PolarAngles
rotate (PolarAngles theta1 phi1) (PolarAngles theta2 phi2) =
  normalize $ PolarAngles (theta1 + theta2) (phi1 + phi2)

placeOnSphere :: Angle -> Angle -> PolarAngles
placeOnSphere theta phi =
  normalize (PolarAngles theta phi)

derivingUnbox "PolarAngles"
  [t| PolarAngles -> (Angle, Angle) |]
  [| \(PolarAngles { polar, azimuthal }) -> (polar, azimuthal) |]
  [| \(polar, azimuthal) -> PolarAngles { polar, azimuthal } |]

derivingUnbox "SpinorCoordinates"
  [t| SpinorCoordinates -> (Complex Double, Complex Double) |]
  [| \(SpinorCoordinates { uCoordinate, vCoordinate }) -> (uCoordinate, vCoordinate) |]
  [| \(uCoordinate, vCoordinate) -> SpinorCoordinates { uCoordinate, vCoordinate } |]

