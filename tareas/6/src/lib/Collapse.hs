module Collapse (collapse) where

collapse :: Int -> Int
collapse n = 1 + (n - 1) `mod` 9
