module Main (main) where

import Control.Parallel.Strategies (parMap, rpar)

import qualified Collapse as C

parSumMap :: (a -> Int) -> [a] -> Int
parSumMap f = sum . parMap rpar f

pcollapse :: [Int] -> Int
pcollapse = C.collapse . parSumMap C.collapse

main = print $ pcollapse [0..1000000]
