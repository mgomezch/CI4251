module Main (main) where
  import Data.Foldable (foldl')
  import qualified Collapse as C

  collapse :: [Int] -> Int
  collapse = C.collapse . foldl' (\ acc n -> acc + C.collapse n) 0

  main = C.main collapse
