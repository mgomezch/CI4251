module Main (main) where

import Vectorised
import Data.Array.Parallel
import Data.Array.Parallel.PArray

main = print . toList . doStuff . fromList $ [1..100]
