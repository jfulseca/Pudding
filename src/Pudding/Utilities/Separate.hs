module Pudding.Utilities.Separate
( Separate
, neutral
, sadd
, sdiv
, smul
, sscale
, ssqrt
, ssub
) where

import Data.Complex (Complex(..))

class Separate a where
  neutral :: a
  sadd :: a -> a -> a
  sdiv :: a -> a -> a
  smul :: a -> a -> a
  sscale :: Double -> a -> a
  ssqrt :: a -> a
  ssub :: a -> a -> a

instance Separate Double where
  neutral = 0
  sadd = (+)
  sdiv = (/)
  smul = (*)
  sscale = (*)
  ssqrt = sqrt
  ssub = (-)

instance (RealFloat a, Separate a) => Separate (Complex a) where
  neutral = 0 :+ 0
  sadd = (+)
  (x1 :+ y1) `sdiv` (x2 :+ y2) =
    (x1 / x2) :+ (y1 / y2)
  (x1 :+ y1) `smul` (x2 :+ y2) =
    (x1 * x2) :+ (y1 * y2)
  d `sscale` z = (sscale d) <$> z
  ssqrt z = sqrt <$> z
  ssub = (-)
