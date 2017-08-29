module Pudding.Types.Internal.Position

( Position(..)
, createSpherical
, distance
, move
, positionEq
) where

import Pudding.Types.Internal.SphereAngles
import Pudding.Utilities.DoubleFunctions

data Position = SpherePosition SphereAngles | DiskPosition Int
  deriving (Show)

positionEq :: Position -> Position -> Bool
positionEq (SpherePosition p1) (SpherePosition p2) =
  p1 `sphereAnglesEq` p2
positionEq _ _ = undefined

createSpherical :: Angle -> Angle -> Position
createSpherical theta phi =
  SpherePosition $ angles
    where angles = normalize $ SphereAngles theta phi

distance :: Position -> Position -> Double
distance (SpherePosition p1) (SpherePosition p2) =
  p1 `chordLength` p2
distance _ _ = undefined

move :: Position -> Position -> Position
move (SpherePosition p1) (SpherePosition p2) =
  SpherePosition $ p1 `rotate` p2
move _ _ = undefined
