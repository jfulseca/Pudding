module Pudding.Types.Internal.Configuration
( Configuration
, generateSphereConfiguration
) where

import qualified Data.Vector as V
import Pudding.Types.Position
import Pudding.Utilities.RandomFunctions (randomList)
import System.Random (StdGen)

type Configuration = V.Vector Position

generateSphereConfiguration :: StdGen -> Int -> (Configuration, StdGen)
generateSphereConfiguration gen n =
  (V.fromList positionList, gen'') where
    (thetaList, gen') = randomList (0, pi) n gen
    (phiList, gen'') = randomList (0, 2 * pi) n gen'
    positionList = zipWith placeOnSphere thetaList phiList
