{-# LANGUAGE
  FlexibleContexts,
  NamedFieldPuns,
  RecordWildCards,
  ScopedTypeVariables,
  StandaloneDeriving,
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

import {-# SOURCE #-} qualified Data.RTree {-(RTree(Leaf, Node), Entry(Entry))-} as RT

import Control.Monad  (mzero    )
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

deriving instance (Eq   coord, Eq   (Twice coord)) ⇒ Eq   (Zipper coord)
deriving instance (Read coord, Read (Twice coord)) ⇒ Read (Zipper coord)
deriving instance (Show coord, Show (Twice coord)) ⇒ Show (Zipper coord)



fromRTree ∷ RT.RTree coord → Zipper coord
fromRTree t = case t of
  RT.Leaf {..} → Leaf { parent = mzero, .. }

  RT.Node {..} →
    Node
      { parent        = mzero
      , left_entries  = []
      , right_entries = entries
      }



fromEntry ∷ RT.Entry coord → Zipper coord
fromEntry (RT.Entry {..}) = Entry { parent = mzero, .. }



up, down, left, right ∷ ∀ coord. Zipper coord → Maybe (Zipper coord)

up z = do
  p ← parent z
  return $ case p of
    Node  { left_entries = _:ls } → p { left_entries = (toEntryAt z): ls }
    Entry {} → p { child = toRTreeAt z }



down z = case z of
  Leaf {} → mzero

  Entry { child } →
    return $ (fromRTree child) { parent = return z }

  Node {} → case left_entries z of
    []  → mzero
    l:_ → return $ (fromEntry l) { parent = return z }



left z = case z of
  Entry {} → mzero
  Leaf  {} → mzero

  Node  { left_entries = [] } → mzero

  Node { left_entries = l:ls, right_entries = rs } →
    return $ z { left_entries = ls, right_entries = l:rs }



right z = case z of
  Entry {} → mzero
  Leaf  {} → mzero

  Node { right_entries = [] } → mzero

  Node { right_entries = r:rs, left_entries = ls } →
    return $ z { left_entries = r:ls, right_entries = rs }



top, start, end ∷ Zipper coord → Zipper coord
top   z = maybe z top   $ up    z
start z = maybe z start $ left  z
end   z = maybe z end   $ right z



toRTreeAt, toRTree ∷ Zipper coord → RT.RTree coord

toRTreeAt z = case z of
  Leaf {..} → RT.Leaf {..}
  Node {..} → RT.Node { entries = reverse left_entries ++ right_entries }

toRTree = toRTreeAt . top



toEntryAt ∷ Zipper coord → RT.Entry coord
toEntryAt (Entry {..}) = RT.Entry {..}
