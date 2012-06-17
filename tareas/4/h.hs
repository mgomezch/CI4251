{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Word (Word16, Word32)
import Data.Array (array, (!))
import Data.Bits (shift, testBit)
import Data.Either ()
import Data.Tree (Tree(Node), rootLabel)
import Data.Tree.Zipper (fromTree, setLabel, tree)

import qualified Data.Monoid ()
import qualified Data.Sequence as DS

import Control.Monad (join)
import Control.Monad.Instances ()
import Control.Monad.Error (Error, throwError, noMsg)
import Control.Monad.State (MonadState, State, state, get, runState)
import Control.Monad.Writer (WriterT, tell, runWriterT)

import Control.Arrow (first, second, (***))




ln = 3
bf = 2

type Walker = WriterT (DS.Seq (Word16, Word16)) (State (Word16, Word16)) ()

update :: MonadState s m => (s -> s) -> m ()
update = state . (((),) .)

infix 2 +=
(+=) :: (MonadState (n, n) m, Num n) => ((n -> n) -> (n, n) -> (n, n)) -> n -> m ()
v += n = update $ v (n + )

infix 2 -=
(-=) :: (MonadState (n, n) m, Num n) => ((n -> n) -> (n, n) -> (n, n)) -> n -> m ()
v -= n = update $ v $ flip (-) n

x :: (s -> s) -> (s, s) -> (s, s)
x = first

y :: (s -> s) -> (s, s) -> (s, s)
y = second

move :: Walker
move = tell . return =<< get



a :: Integer -> Walker
a i = if i == 0 then return () else do
  d(i-1); x -= 1; move
  a(i-1); y -= 1; move
  a(i-1); x += 1; move
  b(i-1)

b :: Integer -> Walker
b i = if i == 0 then return () else do
  c(i-1); y += 1; move
  b(i-1); x += 1; move
  b(i-1); y -= 1; move
  a(i-1)

c :: Integer -> Walker
c i = if i == 0 then return () else do
  b(i-1); x += 1; move
  c(i-1); y += 1; move
  c(i-1); x -= 1; move
  d(i-1)

d :: Integer -> Walker
d i = if i == 0 then return () else do
  a(i-1); y -= 1; move
  d(i-1); x -= 1; move
  d(i-1); y += 1; move
  c(i-1)



walk :: Integer -> DS.Seq (Word16, Word16)
walk = snd . fst . flip runState (0, 0) . runWriterT . (move >> ) . b





n_xy :: Word32 -> (Word16, Word16)
n_xy n = go 1 n 16 (0, 0) where
  go _ _ 0 p      = p
  go t n k (x, y) =
    let (bx, by, nt) = table ! (t, b0 n, b1 n)
    in go nt (n `shift` 2) (pred k) (x `shift` 1 + bx, y `shift` 1 + by)
  b0 = fromEnum . flip testBit 31
  b1 = fromEnum . flip testBit 30
  table = array ((1, 0, 0), (4, 1, 1)) [
        ((1, 0, 0), (0, 0, 2)),   ((2, 0, 0), (0, 0, 1)),   ((3, 0, 0), (1, 1, 4)),   ((4, 0, 0), (1, 1, 3)),
        ((1, 0, 1), (1, 0, 1)),   ((2, 0, 1), (0, 1, 2)),   ((3, 0, 1), (0, 1, 3)),   ((4, 0, 1), (1, 0, 4)),
        ((1, 1, 0), (1, 1, 1)),   ((2, 1, 0), (1, 1, 2)),   ((3, 1, 0), (0, 0, 3)),   ((4, 1, 0), (0, 0, 4)),
        ((1, 1, 1), (0, 1, 4)),   ((2, 1, 1), (1, 0, 3)),   ((3, 1, 1), (1, 0, 2)),   ((4, 1, 1), (0, 1, 1))]



xy_n :: (Word16, Word16) -> Word32
xy_n p = go 1 p 16 0 where
  go _ _      0 n = n
  go t (x, y) k n =
    let (b0, b1, nt) = table ! (t, b x, b y)
    in go nt ((***) `join` (`shift` 1) $ (x, y)) (pred k) $ n `shift` 2 + b0 `shift` 1 + b1
  b = fromEnum . flip testBit 15
  table = array ((1, 0, 0), (4, 1, 1)) [
        ((1, 0, 0), (0, 0, 2)),   ((2, 0, 0), (0, 0, 1)),   ((3, 0, 0), (1, 0, 3)),   ((4, 0, 0), (1, 0, 4)),
        ((1, 0, 1), (1, 1, 4)),   ((2, 0, 1), (0, 1, 2)),   ((3, 0, 1), (0, 1, 3)),   ((4, 0, 1), (1, 1, 1)),
        ((1, 1, 0), (0, 1, 1)),   ((2, 1, 0), (1, 1, 3)),   ((3, 1, 0), (1, 1, 2)),   ((4, 1, 0), (0, 1, 4)),
        ((1, 1, 1), (1, 0, 1)),   ((2, 1, 1), (1, 0, 2)),   ((3, 1, 1), (0, 0, 4)),   ((4, 1, 1), (0, 0, 3))]



data Rectangle = R { ll, ur :: (Word16, Word16) }
               deriving (Eq, Show, Read)

data RData = RData { mbr :: Rectangle, lhv :: Word32, rects :: [Rectangle] }
           deriving (Eq, Show, Read)

type RTree = Tree RData



instance (Num a, Num b) => Num (a, b) where
  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
  (x1, y1) * (x2, y2) = (x1 * x2, y1 * y2)
  (x1, y1) - (x2, y2) = (x1 - x2, y1 - y2)

  abs           = abs *** abs
  signum (x, y) = undefined
  fromInteger i = (fromInteger i, fromInteger i)

r_n r = xy_n $ join (***) (`div` 2) $ sum $ map ($ r) [ll, ur]



data RError = Duplicate Rectangle
            | Unimplemented
            deriving (Eq, Show, Read)

instance Error RError where
  noMsg = undefined



insert :: RTree -> Rectangle -> Either RError RTree

insert t@(Node (RData r v []      ) (_:_)) rect = throwError Unimplemented

insert t@(Node (RData r v rs@(_:_)) []   ) rect
  | rect `elem` rs  = throwError $ Duplicate rect
  | length rs == ln = throwError $ Unimplemented
  | otherwise       = do
    return $ tree $ setLabel (rootLabel t) { rects = ll ++ [rect] ++ lr } $ fromTree t
    -- TODO: update lhv and mbr
    where
      (ll, lr) = break (( >= r_n rect) . r_n) rs
