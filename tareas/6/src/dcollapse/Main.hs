module Main (main) where

import Data.Array.Parallel.PArray (fromList, toList)
import System.Environment (getArgs)

import Vectorised (collapse)

main = print . collapse . fromList . enumFromTo 1 . read . head =<< getArgs
