module Pudding.Utilities.VectorFunctions
( pairUp
) where

import qualified Data.Vector as V

pairElement :: (V.Vector a)
            -> (a -> a -> b)
            -> Int
            -> a
            -> (V.Vector b)
pairElement v f idx el =
  V.map (f el) $ V.drop (idx + 1) v

pairUp :: (a -> a -> b) -> (V.Vector a) -> (V.Vector (V.Vector b))
pairUp f v = V.imap (pairElement v f) v
