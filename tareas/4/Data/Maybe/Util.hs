{-# LANGUAGE
  ScopedTypeVariables,
  UnicodeSyntax
  #-}

module Data.Maybe.Util
  ( toMaybe
  ) where

import Control.Monad (guard)

toMaybe ∷ ∀ a. Bool → a → Maybe a
toMaybe b a = guard b >> return a
