{-# LANGUAGE
  FlexibleContexts,
  ScopedTypeVariables,
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
  ) where

import Control.Monad.Error (Error    )
import Data.Bits           (Bits     )
import Data.Hilbert        (Twice    )
import Data.Rectangle      (Rectangle)



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



data RError coord

instance (Eq   coord, Eq   (Twice coord)) ⇒ Eq    (RError coord)
instance (Read coord, Read (Twice coord)) ⇒ Read  (RError coord)
instance (Show coord, Show (Twice coord)) ⇒ Show  (RError coord)
instance                                    Error (RError coord)



empty ∷ ∀ coord. RTree coord



search ∷ ∀ coord  . Ord coord ⇒ RTree coord → Rectangle coord → Maybe [Rectangle coord]
delete ∷ ∀ coord e. Ord coord ⇒ RTree coord → Rectangle coord → Either e (RTree coord)

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
