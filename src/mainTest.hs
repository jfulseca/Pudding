{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Pudding.Test.Utilities.DoubleFunctions
import {-@ HTF_TESTS @-} Pudding.Test.Types.Position

main = htfMain htf_importedTests
