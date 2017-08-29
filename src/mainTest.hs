{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Pudding.Test.Types.SphereAngles
import {-@ HTF_TESTS @-} Pudding.Test.Types.Position
import {-@ HTF_TESTS @-} Pudding.Test.Utilities.ComplexFunctions
import {-@ HTF_TESTS @-} Pudding.Test.Utilities.DoubleFunctions

main = htfMain htf_importedTests
