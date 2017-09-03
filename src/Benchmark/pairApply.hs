import Criterion.Main (defaultMain)
import qualified Data.Vector as V
import Pudding.Types.Configuration
import Pudding.Utilities.VectorFunctions

-- Ex: pairApplyL (*) vec (+) (+) 0 0 V.foldl V.foldl
pairApplyL :: (a -> a -> b)
           -> (V.Vector a)
           -> (c -> b -> c)
           -> (d -> c -> d)
           -> c
           -> d
           -> ((c -> b -> c) -> c -> (V.Vector b) -> c)
           -> ((d -> c -> d) -> d -> (V.Vector c) -> d)
           -> d
pairApplyL f v combine1 combine2 init1 init2 fold1 fold2 =
  fold2 combine2 init2 $ V.map (fold1 combine1 init1) (pairUp f v)

-- Ex: pairApplyR (*) vec (+) (+) 0 0 V.foldr V.foldr
pairApplyR :: (a -> a -> b)
           -> (V.Vector a)
           -> (b -> c -> c)
           -> (c -> d -> d)
           -> c
           -> d
           -> ((b -> c -> c) -> c -> (V.Vector b) -> c)
           -> ((c -> d -> d) -> d -> (V.Vector c) -> d)
           -> d
pairApplyR f v combine1 combine2 init1 init2 fold1 fold2 =
  fold2 combine2 init2 $ V.map (fold1 combine1 init1) (pairUp f v)

main = defaultMain [bench "test" $ \n -> 1 + n - n ]
