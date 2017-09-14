module Pudding.Utilities.VectorFunctions
( pairApply
) where

import qualified Data.Vector.Unboxed as U

pairApply :: (U.Unbox a, U.Unbox b)
          => (b -> b -> b)
          -> b
          -> (a -> a -> b)
          -> (U.Vector a)
          -> b
pairApply combine neutral f v = U.ifoldl'
  (\acc i p -> U.foldl' (\acc' q -> combine acc' $ f p q)
               acc
               (U.drop (i + 1) v))
  neutral
  v
