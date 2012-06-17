module Data.Maybe.Util
  ( toMaybe
  ) where

import Data.Maybe (Maybe)
import Control.Monad (guard)

toMaybe :: Bool -> a -> Maybe a
toMaybe b a = guard b >> return a
