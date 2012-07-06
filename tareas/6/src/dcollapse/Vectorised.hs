{-# OPTIONS -fvectorise #-}
{-# LANGUAGE ParallelArrays, ParallelListComp #-}

module Vectorised (collapse) where

import Data.Array.Parallel        (toPArrayP, fromPArrayP)
import Data.Array.Parallel.PArray (PArray)

import qualified Data.Array.Parallel.Prelude.Int as I

{-# NOINLINE collapse #-}
collapse :: PArray Int -> PArray Int
collapse xs = toPArrayP (collapse' (fromPArrayP xs))

collapse' :: [:Int:] -> [:Int:]
collapse' ns = [: 1 I.+ (n I.- 1) `I.mod` 9 | n <- ns :]
