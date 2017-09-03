module Pudding.Types.Internal.Configuration
( Configuration(..)
, generateSphereConfiguration
) where

import qualified Data.Vector as V
import Pudding.Utilities.RandomFunctions (randomList)
import Pudding.Types.PolarAngles
import System.Random (StdGen)

data Configuration = Sphere (V.Vector PolarAngles)
  deriving (Eq, Show)

generateSphereConfiguration :: StdGen -> Int -> (Configuration, StdGen)
generateSphereConfiguration gen n =
  (Sphere $ V.fromList positionList, gen'') where
    (thetaList, gen') = randomList (0, pi) n gen
    (phiList, gen'') = randomList (0, 2 * pi) n gen'
    positionList = zipWith placeOnSphere thetaList phiList
