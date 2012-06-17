module Data.RTree where


import Control.Arrow           ((<<^)               )
import Control.Monad           (guard, join         )
import Control.Monad.Instances (                    )
import Data.Hilbert            (Twice               )
import Data.Maybe.Util         (toMaybe             )
import Data.Rectangle          (Rectangle, intersect)


cl :: Integer
cl = 3 -- Capacidad de las hojas.

cn :: Integer
cn = 3 -- Capacidad de los nodos.


data RTree coord = Leaf { rectangles :: [Rectangle coord] }
                 | Node { entries    :: [Entry     coord] }

data Entry coord = Entry { mbr :: Rectangle coord, lhv :: Twice coord, child :: RTree coord }



search :: Ord coord => RTree coord -> Rectangle coord -> Maybe [Rectangle coord]
search = (join (toMaybe <<^ not . null) .) . search'
  where
    search' (Leaf rs) r = intersect r `filter` rs
    search' (Node es) r = do
      e <- es
      guard $ r `intersect` mbr e
      child e `search'` r
