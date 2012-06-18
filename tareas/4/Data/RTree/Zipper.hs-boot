{-# LANGUAGE
  FlexibleContexts,
  UndecidableInstances,
  UnicodeSyntax
  #-}

module Data.RTree.Zipper
  ( Zipper(Leaf, Node, Entry)
  , parent
  , rectangles
  , left_entries, right_entries
  , mbr, lhv
  , fromRTree, fromEntry
  , up, down, left, right
  , top, start, end
  , toRTreeAt, toRTree
  , toEntryAt
  ) where

import {-# SOURCE #-} qualified Data.RTree {-(RTree(), Entry())-} as RT

import Data.Hilbert   (Twice    )
import Data.Rectangle (Rectangle)

data Zipper coord
  = Leaf
    { parent     ∷ Maybe (Zipper coord)
    , rectangles ∷ [Rectangle coord]
    }
  | Node
    { parent        ∷ Maybe (Zipper coord)
    , left_entries  ∷ [RT.Entry coord]
    , right_entries ∷ [RT.Entry coord]
    }
  | Entry
    { parent ∷ Maybe (Zipper coord)
    , mbr    ∷ Rectangle coord
    , lhv    ∷ Twice     coord
    , child  ∷ RT.RTree  coord
    }

instance (Eq   coord, Eq   (Twice coord)) ⇒ Eq   (Zipper coord)
instance (Read coord, Read (Twice coord)) ⇒ Read (Zipper coord)
instance (Show coord, Show (Twice coord)) ⇒ Show (Zipper coord)

fromRTree             ∷ RT.RTree coord → Zipper coord
fromEntry             ∷ RT.Entry coord → Zipper coord
up, down, left, right ∷ Zipper   coord → Maybe (Zipper coord)
top, start, end       ∷ Zipper   coord → Zipper coord
toRTreeAt, toRTree    ∷ Zipper   coord → RT.RTree coord
toEntryAt             ∷ Zipper   coord → RT.Entry coord
