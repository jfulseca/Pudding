{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Pudding.Test.Types.Configuration
(htf_thisModulesTests) where

import qualified Data.Vector as V
import Test.Framework
import Pudding.Test.Aux (getSphere)
import Pudding.Types.Internal.Configuration
import Pudding.Types.PolarAngles (toSpinor)

prop_generateSphereLength :: Int -> (Positive Int) -> Bool
prop_generateSphereLength genInt (Positive n) =
  length a == n && length s == n where
    (Sphere a s) = getSphere n genInt

prop_generateSphereDifferent :: Int -> (Positive Int) -> Bool
prop_generateSphereDifferent genInt (Positive n) =
  V.and $ V.map V.and (V.imap (\idx el -> V.map (/= el) (V.drop (idx + 1) a)) a)
    where (Sphere a _) = getSphere n genInt

prop_generateSphereSpinors :: Int -> (Positive Int) -> Bool
prop_generateSphereSpinors genInt (Positive n) =
  s == V.map toSpinor a where
    (Sphere a s) = getSphere n genInt

test_emptySphereAngles = assertEqual 0 $
  length (angles emptySphere)

test_emptySphereSpinors = assertEqual 0 $
  length (spinors emptySphere)

test_sphereSwapPNeg = assertEqual Nothing $
  swap (-1) 1 c where
    c = getSphere 3 0

test_sphereSwapQNeg = assertEqual Nothing $
  swap 1 (-1) c where
    c = getSphere 3 0

test_sphereSwapPLarge = assertEqual Nothing $
  swap 6 1 c where
    c = getSphere 3 0

test_sphereSwapQLarge = assertEqual Nothing $
  swap 1 6 c where
    c = getSphere 3 0

prop_sphereSwapLength :: (Positive Int)
                      -> Int
                      -> (NonNegative Int)
                      -> (NonNegative Int)
                      -> Property
prop_sphereSwapLength (Positive n) genInt (NonNegative p) (NonNegative q) =
  (p < n && q <n) ==>
    length (angles c) == length (angles c') &&
    length (spinors c) == length (spinors c') &&
    length (angles c') == length (spinors c')
      where c = getSphere n genInt
            mc = swap p q c
            c' = case mc of
              Nothing -> emptySphere
              Just conf -> conf

prop_sphereSwapValues :: (Positive Int)
                      -> Int
                      -> (NonNegative Int)
                      -> (NonNegative Int)
                      -> Property
prop_sphereSwapValues (Positive n) genInt (NonNegative p) (NonNegative q) =
  (p < n && q <n) ==>
    a V.! p == a' V.! q && a V.! q == a' V.! p &&
    s V.! p == s' V.! q && s V.! q == s' V.! p
      where c@(Sphere a s) = getSphere n genInt
            mc = swap p q c
            (Sphere a' s') = case mc of
              Nothing -> emptySphere
              Just conf -> conf

prop_sphereSwapTwice :: (Positive Int)
                     -> Int
                     -> (NonNegative Int)
                     -> (NonNegative Int)
                     -> Property
prop_sphereSwapTwice (Positive n) genInt (NonNegative p) (NonNegative q) =
  (p < n && q <n) ==>
    c == c''
      where c = getSphere n genInt
            mc' = swap p q c
            c' = case mc' of
              Nothing -> emptySphere
              Just conf -> conf
            mc'' = swap p q c'
            c'' = case mc'' of
              Nothing -> emptySphere
              Just conf -> conf
