module Pudding.Utilities.RandomFunctions
( randomList
) where

import System.Random (Random, StdGen, random, randomRs)

randomList :: (Random a) => (a, a) -> Int -> StdGen -> ([a], StdGen)
randomList bounds n gen = (take n list, gen')
  where list = randomRs bounds gen
        gen' = snd (random gen :: (Bool, StdGen))
