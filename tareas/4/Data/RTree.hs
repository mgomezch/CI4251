{-# LANGUAGE
  UnicodeSyntax,
  StandaloneDeriving,
  FlexibleContexts,
  UndecidableInstances
  #-}

module Data.RTree
  ( RTree(Leaf, Node)
  , rectangles
  , entries
  , Entry(Entry), mbr, lhv, child
  , leafCapacity, nodeCapacity
  , search
  ) where

import Control.Monad           (guard, join         )
import Control.Monad.Instances (                    )
import Data.Hilbert            (Twice               )
import Data.Maybe.Util         (toMaybe             )
import Data.Rectangle          (Rectangle, intersect)

import {-# SOURCE #-} qualified Data.RTree.Zipper as Z


leafCapacity, nodeCapacity ∷ Integer
leafCapacity = 3
nodeCapacity = 3


data RTree coord
  = Leaf { rectangles ∷ [Rectangle coord] }
  | Node { entries    ∷ [Entry     coord] }

deriving instance (Eq   coord, Eq   (Twice coord)) ⇒ Eq   (RTree coord)
deriving instance (Read coord, Read (Twice coord)) ⇒ Read (RTree coord)
deriving instance (Show coord, Show (Twice coord)) ⇒ Show (RTree coord)


data Entry coord
  = Entry
    { mbr   ∷ Rectangle coord
    , lhv   ∷ Twice     coord
    , child ∷ RTree     coord
    }

deriving instance (Eq   coord, Eq   (Twice coord)) ⇒ Eq   (Entry coord)
deriving instance (Read coord, Read (Twice coord)) ⇒ Read (Entry coord)
deriving instance (Show coord, Show (Twice coord)) ⇒ Show (Entry coord)



search ∷ Ord coord ⇒ RTree coord → Rectangle coord → Maybe [Rectangle coord]
search = (join (toMaybe . not . null) .) . search'
  where
    search' (Leaf rs) r = intersect r `filter` rs
    search' (Node es) r = do
      e ← es
      guard $ r `intersect` mbr e
      child e `search'` r



insert ∷ Ord coord ⇒ RTree coord → Rectangle coord → Either e (RTree coord)
insert = undefined



delete ∷ Ord coord ⇒ RTree coord → Rectangle coord → Either e (RTree coord)
delete = undefined
