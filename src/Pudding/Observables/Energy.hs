module Pudding.Observables.Energy
( energy
) where

import Data.Complex (Complex)
import Pudding.Observables.SimpleObservable
import Pudding.Types.Configuration (spinors)
import Pudding.Types.PolarAngles (SpinorCoordinates, spinorProduct)
import Pudding.Utilities.ComplexFunctions (cabs)
import Pudding.Utilities.VectorFunctions (pairApply)

pairEnergy :: SpinorCoordinates -> SpinorCoordinates -> (Complex Double)
pairEnergy s1 s2 = abs $ spinorProduct s1 s2

energy :: Double -> (SimpleObservable Double)
energy radius = \c -> 2 * radius * (cabs $
  pairApply (+) 0 pairEnergy (spinors c))
