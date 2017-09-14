{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Pudding.Test.Utilities.VectorFunctions
(listPairApply, htf_thisModulesTests) where

import Data.List (foldl')
import qualified Data.Vector.Unboxed as V
import Test.Framework
import Pudding.Test.Types.PolarAngles ()
import Pudding.Types.PolarAngles
import Pudding.Utilities.FloatEq
import Pudding.Utilities.ComplexFunctions (cabs)
import Pudding.Utilities.VectorFunctions

listPairApply :: (b -> b -> b)
              -> b
              -> (a -> a -> b)
              -> [a]
              -> b
listPairApply combine neutral f l =
  foldl' combine neutral $
    let n = length l in
    [f (l!!i) (l!!j) | i <- [0..(n-1)], j <- [(i+1)..(n-1)] ]

checkPairApplied :: (Show b, FloatEq b, V.Unbox a, V.Unbox b)
                 => (b -> b -> b)
                 -> b
                 -> (a -> a -> b)
                 -> [a]
                 -> Bool
checkPairApplied combine neutral f l =
  let pairApplied = pairApply combine neutral f $ V.fromList l
      listApplied = listPairApply combine neutral f l
  in pairApplied ~= listApplied

prop_pairMultAddInt :: [Int] -> Bool
prop_pairMultAddInt =
  checkPairApplied (+) 0 (*)

prop_pairAddSubDouble :: [Double] -> Bool
prop_pairAddSubDouble =
  checkPairApplied (-) 1 (+)

prop_pairMultAddSpinor :: [PolarAngles] -> Bool
prop_pairMultAddSpinor angleList =
  checkPairApplied (+) 0.0 energy spinors where
    energy s1 s2 = cabs $ spinorProduct s1 s2
    spinors = map toSpinor angleList
