{-# LANGUAGE ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}

module Data.Hilbert
  ( Twice
  , Half
  , fromHilbert
  , toHilbert
  ) where

import Control.Arrow           ((***))
import Control.Monad           (join)
import Control.Monad.Instances ()
import Data.Array              ((!), Array, array)
import Data.Bits               (Bits, bitSize, shift, testBit)
import Data.Function           (on)
import Data.Int                (Int8, Int16, Int32, Int64)
import Data.Rectangle          (Rectangle, center)
import Data.Word               (Word8, Word16, Word32, Word64)



type family Twice t :: *
type instance Twice Word32 = Word64
type instance Twice Word16 = Word32
type instance Twice Word8  = Word16
type instance Twice Int32  = Int64
type instance Twice Int16  = Int32
type instance Twice Int8   = Int16

type family Half t :: *
type instance Half Word64 = Word32
type instance Half Word32 = Word16
type instance Half Word16 = Word8
type instance Half Int64  = Int32
type instance Half Int32  = Int16
type instance Half Int16  = Int8



type TableIndex = Int
type BitLength  = Int
type BitValue   = Int



fromHilbert
  :: forall hilbertValue coord.
     ( Bits    coord
     , Bounded coord
     , Bits    hilbertValue
     , coord ~ Half hilbertValue
     )
  => hilbertValue
  -> (coord, coord)

fromHilbert h = go 1 h cBits (minBound, minBound) where

  go
    :: TableIndex
    -> hilbertValue
    -> BitLength
    -> (coord, coord)
    -> (coord, coord)

  go _ _ 0 p      = p

  go t n k (x, y) =
    let (bx, by, nt) = table ! (t, b0 n, b1 n)
    in go nt (n `shift` 2) (pred k) (x `shift` 1 + bx, y `shift` 1 + by)


  b0, b1 :: hilbertValue -> BitValue
  b0 = fromEnum . flip testBit (hBits - 1)
  b1 = fromEnum . flip testBit (hBits - 2)


  cBits, hBits :: BitLength
  cBits = bitSize (undefined :: coord)
  hBits = bitSize (undefined :: hilbertValue     )


  table :: Array (TableIndex, BitValue, BitValue) (coord, coord, TableIndex)
  table = array ((1, 0, 0), (4, 1, 1)) [
        ((1, 0, 0), (0, 0, 2)),   ((2, 0, 0), (0, 0, 1)),   ((3, 0, 0), (1, 1, 4)),   ((4, 0, 0), (1, 1, 3)),
        ((1, 0, 1), (1, 0, 1)),   ((2, 0, 1), (0, 1, 2)),   ((3, 0, 1), (0, 1, 3)),   ((4, 0, 1), (1, 0, 4)),
        ((1, 1, 0), (1, 1, 1)),   ((2, 1, 0), (1, 1, 2)),   ((3, 1, 0), (0, 0, 3)),   ((4, 1, 0), (0, 0, 4)),
        ((1, 1, 1), (0, 1, 4)),   ((2, 1, 1), (1, 0, 3)),   ((3, 1, 1), (1, 0, 2)),   ((4, 1, 1), (0, 1, 1))]



toHilbert
  :: forall hilbertValue coord.
     ( Bits    coord
     , Bits    hilbertValue
     , Bounded hilbertValue
     , Ord     hilbertValue
     , hilbertValue ~ Twice coord
     )
  => (coord, coord)
  -> hilbertValue


toHilbert p = go 1 p cBits minBound where

  go
    :: TableIndex
    -> (coord, coord)
    -> Int
    -> hilbertValue
    -> hilbertValue

  go _ _      0 n = n

  go t (x, y) k n =
    let (b0, b1, nt) = table ! (t, b x, b y)
    in go nt (join (***) (`shift` 1) (x, y)) (pred k) $ n `shift` 2 + b0 `shift` 1 + b1


  b :: coord -> BitValue
  b = fromEnum . flip testBit (cBits - 1)

  cBits :: BitLength
  cBits = bitSize (undefined :: coord)

  table :: Array (TableIndex, BitValue, BitValue) (hilbertValue, hilbertValue, TableIndex)
  table = array ((1, 0, 0), (4, 1, 1)) [
        ((1, 0, 0), (0, 0, 2)),   ((2, 0, 0), (0, 0, 1)),   ((3, 0, 0), (1, 0, 3)),   ((4, 0, 0), (1, 0, 4)),
        ((1, 0, 1), (1, 1, 4)),   ((2, 0, 1), (0, 1, 2)),   ((3, 0, 1), (0, 1, 3)),   ((4, 0, 1), (1, 1, 1)),
        ((1, 1, 0), (0, 1, 1)),   ((2, 1, 0), (1, 1, 3)),   ((3, 1, 0), (1, 1, 2)),   ((4, 1, 0), (0, 1, 4)),
        ((1, 1, 1), (1, 0, 1)),   ((2, 1, 1), (1, 0, 2)),   ((3, 1, 1), (0, 0, 4)),   ((4, 1, 1), (0, 0, 3))]



-- FIXME: compare r r' == EQ does NOT imply r == r'
-- FIXME: Is there a way to make this not be an orphan instance?  (preferably something that doesn’t involve mutually recursive modules…)
instance
  ( Bits     coord
  , Integral coord
  , Bits     hilbertValue
  , Bounded  hilbertValue
  , Ord      hilbertValue
  , hilbertValue ~ Twice coord
  ) => Ord (Rectangle coord) where
  compare = compare `on` (toHilbert :: (coord, coord) -> hilbertValue) . center
