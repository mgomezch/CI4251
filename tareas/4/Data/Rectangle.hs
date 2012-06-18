{-# LANGUAGE
  ScopedTypeVariables,
  UnicodeSyntax
  #-}

module Data.Rectangle
  ( Rectangle(Rectangle)
  , xl, xh, yl, yh
  , inside
  , corners
  , center
  , intersect
  , minBoundR, noBoundR
  ) where


import Control.Arrow ((&&&)             )
import Control.Monad (ap, liftM2, liftM4)


data Rectangle coord = Rectangle { xl, xh, yl, yh ∷ coord }
                     deriving (Eq, Read, Show)


inside ∷ ∀ coord. Ord coord ⇒ (coord, coord) → Rectangle coord → Bool
inside (x, y) = liftM2 (&&) (f x xl xh) (f y yl yh)
  where
    f
      ∷ Ord coord
      ⇒ coord
      → (Rectangle coord → coord)
      → (Rectangle coord → coord)
      → Rectangle coord → Bool
    f t l h = inRange t . (l &&& h)

    inRange ∷ Ord coord ⇒ coord → (coord, coord) → Bool
    inRange t (l, h) = l <= t && t <= h


corners ∷ ∀ coord. Rectangle coord → [(coord, coord)]
corners = ap [x &&& y | x ← [xl, xh], y ← [yl, yh]] . return


center ∷ ∀ coord. Integral coord ⇒ Rectangle coord → (coord, coord)
center r = (xl .+. xh, yl .+. yh)
  where
    l .+. h = (`div` 2) $ liftM2 (+) l h r


intersect ∷ ∀ coord. Ord coord ⇒ Rectangle coord → Rectangle coord → Bool
intersect r r' = (`inside` r) `any` corners r'


minBoundR ∷ ∀ coord. Ord coord ⇒ [Rectangle coord] → Rectangle coord
minBoundR = liftM4 Rectangle (minimum . map xl)
                             (maximum . map xh)
                             (minimum . map yl)
                             (maximum . map yh)

noBoundR ∷ ∀ coord. Bounded coord ⇒ Rectangle coord
noBoundR = Rectangle { xl = maxBound
                     , xh = minBound
                     , yl = maxBound
                     , yh = minBound
                     }
