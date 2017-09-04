{-# LANGUAGE NamedFieldPuns #-}

module Pudding.Types.Internal.Configuration
( Configuration(..)
, emptySphere
, generateSphereConfiguration
, swap
) where

import Data.Vector ((!), (//), Vector, empty, 
                    fromList, map)
import Prelude hiding (map)
import Pudding.Utilities.RandomFunctions (randomList)
import Pudding.Types.PolarAngles
import System.Random (StdGen)

data Configuration =
  Sphere {
    angles :: Vector PolarAngles
  , spinors :: Vector SpinorCoordinates
  }
  deriving (Eq, Show)

emptySphere :: Configuration
emptySphere = Sphere {
  angles = empty
, spinors = empty
}

swap :: Int -> Int -> Configuration -> (Maybe Configuration)
swap p q (Sphere { angles = oldAngles, spinors = oldSpinors })
  | p < 0 || q < 0 = Nothing
  | p >= len || q >= len = Nothing
  | otherwise = Just $ Sphere {
    angles = oldAngles // [(p, oldAngles ! q), (q, oldAngles ! p)]
  , spinors = oldSpinors // [(p, oldSpinors ! q), (q, oldSpinors ! p)]
  }
  where len = length oldAngles

generateSphereConfiguration :: StdGen -> Int -> (Configuration, StdGen)
generateSphereConfiguration gen n =
  (Sphere { angles, spinors }, gen'') where
    (thetaList, gen') = randomList (0, pi) n gen
    (phiList, gen'') = randomList (0, 2 * pi) n gen'
    positionList = zipWith placeOnSphere thetaList phiList
    angles = fromList positionList
    spinors = map toSpinor angles
