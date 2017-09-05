module Pudding.Storage.ConfigurationSamples
( ConfigurationSamples(..)
) where

import Pudding.Storage.Header

data Geometry = Spherical | Planar

data ConfigurationSamples = ConfigurationSamples {
  geometry :: Geometry
, particles :: Int
, samples :: Int
}
