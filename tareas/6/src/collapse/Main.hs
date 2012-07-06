{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.Foldable (foldl')

sumMap :: (a -> Int) -> [a] -> Int
sumMap f = foldl' (\ !acc !n -> acc + f n) 0

collapse' :: Int -> Int
collapse' n = 1 + (n - 1) `mod` 9

collapse :: [Int] -> Int
collapse = collapse' . sumMap collapse'

main = print $ collapse [0..1000000]
