module Main (main) where

import Data.Array.Parallel.PArray (fromList, toList)
import System.Environment (getArgs)

import Vectorised (collapse)

import qualified Collapse as C

main = C.main $ collapse . fromList
