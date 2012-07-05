{-# OPTIONS
    -fvectorise
  #-}

{-# LANGUAGE
    ParallelArrays
  #-}

module Vectorised (doStuff) where

import Data.Array.Parallel
import Data.Array.Parallel.PArray
import qualified Data.Array.Parallel.Prelude.Int as I

{-# NOINLINE doStuff #-}
doStuff :: PArray Int -> PArray Int
doStuff xs = toPArrayP (reallyDoStuff (fromPArrayP xs))

reallyDoStuff :: [:Int:] -> [:Int:]
reallyDoStuff is = filterP (\ x -> x `I.mod` 2 I.== 0 ) is
