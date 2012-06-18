{-# LANGUAGE
  FlexibleContexts,
  NamedFieldPuns,
  RecordWildCards,
  ScopedTypeVariables,
  StandaloneDeriving,
  TypeFamilies,
  UndecidableInstances,
  UnicodeSyntax
  #-}

module Data.RTree
  ( RTree(Leaf, Node)
  , rectangles
  , entries
  , Entry(Entry), mbr, lhv, child
  , leafCapacity, nodeCapacity
  , empty
  , search, insert, delete
  , chooseLeaf
  ) where

import Control.Monad           (guard, join                           )
import Control.Monad.Error     (Error, throwError, noMsg              )
import Control.Monad.Instances (                                      )
import Data.Bits               (Bits                                  )
import Data.Hilbert            (Twice, toHilbert                      )
import Data.List               (sort                                  )
import Data.Maybe              (fromJust                              )
import Data.Maybe.Util         (toMaybe                               )
import Data.Rectangle          (Rectangle, intersect, center, noBoundR)

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



data RError coord = Duplicate (Rectangle coord)
                  | Unimplemented

deriving instance (Eq   coord, Eq   (Twice coord)) ⇒ Eq   (RError coord)
deriving instance (Read coord, Read (Twice coord)) ⇒ Read (RError coord)
deriving instance (Show coord, Show (Twice coord)) ⇒ Show (RError coord)

instance Error (RError coord) where
  noMsg = undefined



empty ∷ ∀ coord. RTree coord
empty = Leaf { rectangles = [] }



search ∷ ∀ coord. Ord coord ⇒ RTree coord → Rectangle coord → Maybe [Rectangle coord]
search = (join (toMaybe . not . null) .) . search'
  where
    search' (Leaf rs) r = intersect r `filter` rs
    search' (Node es) r = do
      e ← es
      guard $ r `intersect` mbr e
      child e `search'` r



chooseLeaf
  ∷ ∀ coord hilbertValue.
    ( Bits     coord
    , Bounded  coord
    , Integral coord
    , Ord      coord
    , Bits     hilbertValue
    , Bounded  hilbertValue
    , Ord      hilbertValue
    , hilbertValue ~ Twice coord
    )
  ⇒ RTree     coord
  → Rectangle coord
  → Z.Zipper  coord

chooseLeaf t r = go $ Z.fromRTree t
  where
    hv ∷ Twice coord
    hv = toHilbert $ center r

    go ∷ Z.Zipper coord → Z.Zipper coord
    go z = case z of
      Z.Leaf {} → z
      Z.Entry {} → go $ fromJust $ Z.down z

      Z.Node {..} → case (left_entries, right_entries) of
        ([], _:_) → go $ fromJust $ Z.right z

        -- TODO: for this to work nicely, the entries should be kept sorted by lhv
        (Entry {..} : _, l)
          | lhv < hv →
            if   null l
            then newLeaf z
            else go $ fromJust $ Z.right z

          | otherwise →
            go $ fromJust $ Z.down z

        ([], []) → newLeaf z

    newLeaf ∷ Z.Zipper coord → Z.Zipper coord
    newLeaf z = go $ fromJust $ (Z.down $ z { Z.left_entries = newEntry : Z.left_entries z })

    newEntry ∷ Entry coord
    newEntry = Entry
      { mbr   = noBoundR
      , lhv   = minBound
      , child = Leaf { rectangles = [] }
      }



insert
  ∷ ∀ hilbertValue coord.
    ( Bits     coord
    , Bounded  coord
    , Integral coord
    , Bits     hilbertValue
    , Bounded  hilbertValue
    , Ord      hilbertValue
    , hilbertValue ~ Twice coord
    )
  ⇒ RTree coord
  → Rectangle coord
  → Either (RError coord) (RTree coord)
-- No overflow handling (or checking), no fixing the tree, no nothing. :-(
insert t r = maybe miss hit $ search t r
  where
    hit hs =
      if   r `elem` hs
      then throwError $ Duplicate r
      else miss

    miss = return $ Z.toRTree $ l { Z.rectangles = sort $ r : Z.rectangles l }

    l ∷ Z.Zipper coord
    l = chooseLeaf t r



delete ∷ ∀ coord e. Ord coord ⇒ RTree coord → Rectangle coord → Either e (RTree coord)
delete = undefined -- :-(
