{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Utilities.VectorFunctions
(htf_thisModulesTests) where

import qualified Data.Vector as V
import Test.Framework
import Pudding.Test.Types.PolarAngles ()
import Pudding.Types.PolarAngles
import Pudding.Utilities.ComplexFunctions (cabs)
import Pudding.Utilities.VectorFunctions

listPairApply :: (b -> b -> b)
              -> b
              -> (a -> a -> b)
              -> [a]
              -> b
listPairApply combine neutral f l =
  folder $ map folder paired where
    folder = foldl combine neutral
    paired = listPairUp f l
    listPairUp _ [] = []
    listPairUp g m@(_:xs) =
      (listPairElement g m):(listPairUp g xs)
    listPairElement _ [] = []
    listPairElement _ [_] = []
    listPairElement h (y:ys) = map (h y) ys

checkPairApplied :: (Eq b)
                 => (b -> b -> b)
                 -> b
                 -> (a -> a -> b)
                 -> [a]
                 -> Bool
checkPairApplied combine neutral f l =
  pairApplied == listApplied where
    pairApplied = pairApply combine neutral f $ V.fromList l
    listApplied = listPairApply combine neutral f l

prop_pairMultAddInt :: [Int] -> Bool
prop_pairMultAddInt =
  checkPairApplied (+) 0 (*)

prop_pairAddMultDouble :: [Double] -> Bool
prop_pairAddMultDouble =
  checkPairApplied (*) 1 (+)

prop_pairMultAddSpinor :: [PolarAngles] -> Bool
prop_pairMultAddSpinor angleList =
  checkPairApplied (+) 0.0 energy spinors where
    energy s1 s2 = cabs $ spinorProduct s1 s2
    spinors = map toSpinor angleList
