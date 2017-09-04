{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Pudding.Test.Observables.Energy
import {-@ HTF_TESTS @-} Pudding.Test.Observables.SimpleObservable
import {-@ HTF_TESTS @-} Pudding.Test.Types.Configuration
import {-@ HTF_TESTS @-} Pudding.Test.Types.PolarAngles
import {-@ HTF_TESTS @-} Pudding.Test.Utilities.ComplexFunctions
import {-@ HTF_TESTS @-} Pudding.Test.Utilities.DoubleFunctions
import {-@ HTF_TESTS @-} Pudding.Test.Utilities.VectorFunctions

main :: IO ()
main = htfMain htf_importedTests
