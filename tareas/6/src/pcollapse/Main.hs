module Main (main) where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Char                   (digitToInt  )

parSumMap :: (a -> Int) -> [a] -> Int
parSumMap f = sum . parMap rpar f

betterCollapse :: Int -> Int
betterCollapse = until (<10) $ parSumMap digitToInt . show

pcollapse :: [Int] -> Int
pcollapse = betterCollapse . parSumMap betterCollapse

main = print $ pcollapse [0..1000000]
