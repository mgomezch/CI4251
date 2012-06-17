module Data.Function.Util
  ( flip'
  , rot
  , unrot
  ) where

flip' :: (a -> c -> b -> d) -> a -> b -> c -> d
flip' = (flip .)

rot :: (c -> a -> b -> d) -> a -> b -> c -> d
rot = flip' . flip

unrot :: (b -> c -> a -> d) -> a -> b -> c -> d
unrot = flip . flip'

infix 1 ?
(?) :: Bool -> a -> a -> a
(?) c t f = if c then t else f
