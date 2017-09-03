{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Types.Configuration
(htf_thisModulesTests) where

import qualified Data.Vector as V
import Test.Framework
import Pudding.Types.Internal.Configuration
import Pudding.Types.PolarAngles (toSpinor)
import System.Random (mkStdGen)

getConf :: Int -> (Positive Int) -> Configuration
getConf genInt (Positive n) =
  fst (generateSphereConfiguration (mkStdGen genInt) n)

prop_generateSphereLength :: Int -> (Positive Int) -> Bool
prop_generateSphereLength genInt p@(Positive n) =
  length a == n && length s == n where
    (Sphere a s) = getConf genInt p

prop_generateSphereDifferent :: Int -> (Positive Int) -> Bool
prop_generateSphereDifferent genInt p =
  V.and $ V.map V.and (V.imap (\idx el -> V.map (/= el) (V.drop (idx + 1) a)) a)
    where (Sphere a _) = getConf genInt p

prop_generateSphereSpinors :: Int -> (Positive Int) -> Bool
prop_generateSphereSpinors genInt p =
  s == V.map toSpinor a where
    (Sphere a s) = getConf genInt p
