module Pudding.Types.Internal.Configuration
( Configuration(..)
, display
) where

import qualified Data.Vector as V
import Pudding.Types.Position
import System.Random

type Configuration = V.Vector Position

display :: Configuration -> IO ()
display c = putStrLn (show c)
