module Pudding.Utilities.FloatEq
( FloatEq(..)
) where

class FloatEq a where
  (~=) :: a -> a -> Bool
  (~/) :: a -> a -> Bool
  x ~= y = not (x ~/ y)
  x ~/ y = not (x ~= y)

instance FloatEq Int where
  (~=) = (==)
