{-# LANGUAGE
  UnicodeSyntax,
  FlexibleContexts,
  UndecidableInstances
  #-}

module Data.RTree
  ( RTree(Leaf, Node)
  , rectangles
  , entries
  , Entry(Entry), mbr, lhv
  , leafCapacity, nodeCapacity
  , search
  ) where

import Data.Hilbert   (Twice    )
import Data.Rectangle (Rectangle)

leafCapacity, nodeCapacity ∷ Integer

data RTree coord
  = Leaf { rectangles ∷ [Rectangle coord] }
  | Node { entries    ∷ [Entry     coord] }

instance (Eq   coord, Eq   (Twice coord)) ⇒ Eq   (RTree coord)
instance (Read coord, Read (Twice coord)) ⇒ Read (RTree coord)
instance (Show coord, Show (Twice coord)) ⇒ Show (RTree coord)

data Entry coord
  = Entry
    { mbr   ∷ Rectangle coord
    , lhv   ∷ Twice     coord
    , child ∷ RTree     coord
    }

instance (Eq   coord, Eq   (Twice coord)) ⇒ Eq   (Entry coord)
instance (Read coord, Read (Twice coord)) ⇒ Read (Entry coord)
instance (Show coord, Show (Twice coord)) ⇒ Show (Entry coord)

search ∷ Ord coord ⇒ RTree coord → Rectangle coord → Maybe [Rectangle coord]
