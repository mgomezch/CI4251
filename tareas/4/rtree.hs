module Main (main) where

import Data.Rectangle
import Data.RTree ()
import Data.Word (Word16)

type Rect16 = Rectangle Word16

tr1, tr2, tr3, tr4 :: Rect16
tr1 = Rectangle  0    1    0     1
tr2 = Rectangle  2    3    2     3
tr3 = Rectangle  6    9    2     3
tr4 = Rectangle 15 1024 1234 10000

--t1 = Node ([ Entry ()
--            
--           ])

main :: IO ()
main = do
  print tr1
  print tr2
  print tr3
  print tr4
