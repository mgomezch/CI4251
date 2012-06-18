{-# LANGUAGE
  ScopedTypeVariables,
  UnicodeSyntax
  #-}

module Data.Function.Util
  ( flip'
  , rot
  , unrot
  ) where

flip' ∷ ∀ a b c d. (a → c → b → d) → a → b → c → d
flip' = (flip .)

rot ∷ ∀ a b c d. (c → a → b → d) → a → b → c → d
rot = flip' . flip

unrot ∷ ∀ a b c d. (b → c → a → d) → a → b → c → d
unrot = flip . flip'

infix 1 ?
(?) ∷ ∀ a. Bool → a → a → a
(?) c t f = if c then t else f
