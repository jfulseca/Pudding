module Pudding.Types.Internal.SphereAngles

( Angle
, SphereAngles(..)
, SpinorCoordinates(..)
, arcLength
, chordLength
, limitAngle
, normalize
, rotate
, sphereAnglesEq
, spinorProduct
, toSpinor
) where

import Data.Complex
import Pudding.Utilities.ComplexFunctions
import Pudding.Utilities.DoubleFunctions

type Angle = Double

limitAngle :: Angle -> Angle
limitAngle = flip fmod $ 2 * pi

data SphereAngles = SphereAngles {
  polar :: Angle
, azimuthal :: Angle
} deriving (Show)

data SpinorCoordinates = SpinorCoordinates {
  uCoordinate :: Complex Double
, vCoordinate :: Complex Double
} deriving (Show)

toSpinor :: SphereAngles -> SpinorCoordinates
toSpinor (SphereAngles theta phi) =
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

chordLength :: SphereAngles -> SphereAngles -> Double
chordLength a1 a2 =
  2.0 * (realPart . abs $ spinorProduct s1 s2) where
    s1 = toSpinor a1
    s2 = toSpinor a2

arcLength :: SphereAngles -> SphereAngles -> Double
arcLength = undefined

normalize :: SphereAngles -> SphereAngles
normalize (SphereAngles theta phi) =
  SphereAngles (limitAngle theta') (limitAngle phi') where
    (theta', phi')
      | theta < 0 = (-theta, phi + pi)
      | theta > pi = (2 * pi - theta, phi + pi)
      | otherwise = (theta, phi)

sphereAnglesEq :: SphereAngles -> SphereAngles -> Bool
sphereAnglesEq a1 a2 =
  theta1' `doubleEq` theta2' && phi1' `doubleEq` phi2' where
    SphereAngles theta1' phi1' = normalize a1
    SphereAngles theta2' phi2' = normalize a2

rotate :: SphereAngles -> SphereAngles -> SphereAngles
rotate (SphereAngles theta1 phi1) (SphereAngles theta2 phi2) =
  normalize $ SphereAngles (theta1 + theta2) (phi1 + phi2)
