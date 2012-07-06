module Main (main) where
  import Control.Parallel.Strategies (parMap, rpar)
  import qualified Collapse as C

  pcollapse :: [Int] -> Int
  pcollapse = C.collapse . sum . parMap rpar C.collapse

  main = C.main pcollapse
