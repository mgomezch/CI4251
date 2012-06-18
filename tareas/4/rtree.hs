{-# LANGUAGE
  ScopedTypeVariables,
  UnicodeSyntax
  #-}

module Main {-(main)-} where

import Control.Monad (foldM)
import Data.Hilbert   as H
import Data.RTree     as RT
import Data.Rectangle as R
import Data.Word (Word16)

type Rectangle16 = Rectangle Word16
type RTree16     = RTree     Word16

main ∷ IO ()
main = do
  print tr1
  print tr2
  print tr3
  print tr4
  print tt1
  print $ maybe  undefined id $ search tt1 $ Rectangle 1 3 1 3
  print $ either undefined id $ foldM RT.insert RT.empty [tr1, tr2, tr3, tr4]



---



tr1, tr2, tr3, tr4 ∷ Rectangle16
tr1 = Rectangle  0    1    0     1
tr2 = Rectangle  2    3    2     3
tr3 = Rectangle  6    9    2     3
tr4 = Rectangle 15 1024 1234 10000



tt1, tt2 ∷ RTree16

tt1 = Leaf [tr1, tr2, tr3]

tt2 = Node
  { entries =
    [ Entry
      { mbr = tr1
      , lhv = toHilbert $ center tr1
      , child = Leaf { rectangles = [tr1] }
      }
    , Entry
      { mbr = minBoundR [tr2, tr3]
      , lhv = maximum $ map (toHilbert . center) [tr2, tr3]
      , child = Leaf { rectangles = [tr2, tr3] }
      }
    ]
  }
