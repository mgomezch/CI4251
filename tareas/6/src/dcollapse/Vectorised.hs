{-# OPTIONS -fvectorise #-}
{-# LANGUAGE ParallelArrays, ParallelListComp #-}

module Vectorised (collapse) where

import Data.Array.Parallel        (toPArrayP, fromPArrayP)
import Data.Array.Parallel.PArray (PArray)

import qualified Data.Array.Parallel.Prelude.Int as I

{-# NOINLINE collapse #-}
collapse :: PArray Int -> Int
collapse xs = collapse' (fromPArrayP xs)

collapse' :: [:Int:] -> Int
collapse' ns = collapse1 (I.sumP [: collapse1 n | n <- ns :])

collapse1 :: Int -> Int
collapse1 n = 1 I.+ (n I.- 1) `I.mod` 9
