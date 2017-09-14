{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Pudding.Types.Internal.Configuration
( Configuration(..)
, Samples
, emptySphere
, generateSphereConfiguration
, getAngles
, getSpinors
, swap
) where

import qualified Data.Vector.Unboxed as U
import Pudding.Utilities.FloatEq
import Pudding.Utilities.RandomFunctions (randomList)
import Pudding.Types.PolarAngles
import System.Random (StdGen)

data Configuration =
  Sphere {
    angles :: U.Vector PolarAngles
  , spinors :: U.Vector SpinorCoordinates
  }
  deriving (Show)

getAngles :: Configuration -> (U.Vector PolarAngles)
getAngles (Sphere { angles }) = angles

getSpinors :: Configuration -> (U.Vector SpinorCoordinates)
getSpinors (Sphere { spinors }) = spinors

instance FloatEq (U.Vector PolarAngles) where
  v1 ~= v2 = U.and $ U.zipWith (~=) v1 v2

instance FloatEq (U.Vector SpinorCoordinates) where
  v1 ~= v2 = U.and $ U.zipWith (~=) v1 v2

instance FloatEq Configuration where
  (Sphere { angles = a1, spinors = s1 }) ~= (Sphere { angles = a2, spinors = s2 }) =
    a1 ~= a2 && s1 ~= s2

type Samples = [Configuration]

emptySphere :: Configuration
emptySphere = Sphere {
  angles = U.empty
, spinors = U.empty
}

swap :: Int -> Int -> Configuration -> (Maybe Configuration)
swap p q (Sphere { angles = oldAngles, spinors = oldSpinors })
  | p < 0 || q < 0 = Nothing
  | p >= len || q >= len = Nothing
  | otherwise = Just $ Sphere {
    angles = oldAngles U.// [(p, oldAngles U.! q), (q, oldAngles U.! p)]
  , spinors = oldSpinors U.// [(p, oldSpinors U.! q), (q, oldSpinors U.! p)]
  }
  where len = U.length oldAngles

generateSphereConfiguration :: StdGen -> Int -> (Configuration, StdGen)
generateSphereConfiguration gen n =
  (Sphere { angles, spinors }, gen'') where
    (thetaList, gen') = randomList (0, pi) n gen
    (phiList, gen'') = randomList (0, 2 * pi) n gen'
    positionList = zipWith placeOnSphere thetaList phiList
    angles = U.fromList positionList
    spinors = U.map toSpinor angles
