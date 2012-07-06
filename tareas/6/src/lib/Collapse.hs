module Collapse (collapse, main) where

import System.Environment (getArgs)

collapse :: Int -> Int
collapse n = 1 + (n - 1) `mod` 9

main :: ([Int] -> Int) -> IO ()
main c = print . c . enumFromTo 0 . read . head =<< getArgs
