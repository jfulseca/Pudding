module Pudding.Observables.Energy
( energy
) where

import Data.Complex (Complex)
import Pudding.Observables.SimpleObservable
import Pudding.Types.Configuration
import Pudding.Types.PolarAngles (SpinorCoordinates, spinorProduct)
import Pudding.Utilities.VectorFunctions (pairApply)

pairEnergy :: SpinorCoordinates -> SpinorCoordinates -> (Complex Double)
pairEnergy s1 s2 = abs $ spinorProduct s1 s2

energy :: SimpleObservable
energy = SimpleObservable {
  evaluate = \c -> pairApply (+) 0 pairEnergy (spinors c)
, name = "Energy"
}
