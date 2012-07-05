{-# LANGUAGE
  BangPatterns
  #-}

module Main (main) where

import Data.Char (digitToInt)
import Data.Foldable (foldl')

sumMap :: (a -> Int) -> [a] -> Int
sumMap f = foldl' (\ !acc !n -> acc + f n) 0

naïveCollapse :: Int -> Int
naïveCollapse = until (<10) $ sumMap digitToInt . show

collapse :: [Int] -> Int
collapse = naïveCollapse . sumMap naïveCollapse

main = print $ collapse [0..1000000]
