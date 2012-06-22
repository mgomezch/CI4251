{-# LANGUAGE
    UnicodeSyntax
  #-}

module Main {-(main)-} where

import Control.Arrow               (first)
import Control.Arrow.Unicode       ((⁂))
import Control.Category.Unicode    ((⋙))
import Control.Concurrent          (forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.STM      (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO, readTVar, writeTVar)
import Control.Exception           (finally, uninterruptibleMask_)
import Control.Monad               (forever, join, mapM, mapM_, mzero)
import Control.Monad.Random        (getRandom)
import Control.Monad.Unicode       ((=≪), (≫), (≫=))
import Data.IORef                  (newIORef, readIORef, writeIORef)
import Data.List                   (map, sum)
import Data.Maybe                  (Maybe, maybe)
import Data.Sequence               ((<|), (|>), Seq, ViewR((:>)), empty, viewr)
import Prelude                     (($), (*), (==), (^), Bounded, Enum, Eq, Show, Int, IO, enumFromTo, flip, fromEnum, minBound, maxBound, pred, return, succ, toEnum, undefined)
import Prelude.Unicode             ((∘), (≤))
import System.IO                   (getChar, print)
import System.Random               (Random, random, randomR)



delay = 500 * 10^3



infixl 5 ⊲
infixl 5 ⊳
(⊲) = (<|)
(⊳) = (|>)



data Persona
  = Hombre
  | Mujer
  | PersonalDeLimpieza
  deriving (Bounded, Enum, Eq, Show)

pr ∷ Persona → Int
pr p = case p of
  Hombre             → 49
  Mujer              → 49
  PersonalDeLimpieza → 2

-- Probabilidad acumulada hasta este tipo de persona, inclusive
accPr ∷ Persona → Int
accPr = sum ∘ map pr ∘ enumFromTo minBound

-- Probabilidad acumulada hasta este tipo de persona, no inclusive
accPr' ∷ Persona → Int
accPr' p = if p == minBound then 0 else accPr (pred p)

unAccPr ∷ Int → Persona
unAccPr n
  | n ≤ accPr Hombre             = Hombre
  | n ≤ accPr Mujer              = Mujer
  | n ≤ accPr PersonalDeLimpieza = PersonalDeLimpieza

instance Random Persona where
  random = randomR (minBound, maxBound)
  randomR = (succ ∘ accPr') ⁂ accPr ⋙ randomR ⋙ (⋙ first unAccPr)



main ∷ IO ()
main = do
  queue ← newTVarIO (empty :: Seq Persona)
  ids ← newIORef []
  let
    start = uninterruptibleMask_ $ writeIORef ids =≪ mapM (($ queue) ⋙ forkIOUnmasked) [generator, printer]
    sleep = atomically mzero
    clean = readIORef ids ≫= mapM_ killThread
    forkIOUnmasked io = forkIOWithUnmask $ \ u -> u io
  (start ≫ sleep) `finally` clean



foreverDelayed ∷ IO α → IO β
foreverDelayed a = forever (a ≫ threadDelay delay)
-- Manuel `asTypeOf` foreverDelayed

generator ∷ Random α ⇒ TVar (Seq α) → IO ()
generator queue = foreverDelayed $ atomically ∘ push queue =≪ getRandom

printer ∷ Show α ⇒ TVar (Seq α) → IO ()
printer queue = foreverDelayed $ maybe (return ()) print =≪ atomically (pop queue)



push ∷ TVar (Seq α) → α → STM ()
push queue a = modifyTVar queue (a ⊲)

pop ∷ TVar (Seq α) → STM (Maybe α)
pop queue = do
  q ← readTVar queue
  case viewr q of
    (as :> a) → writeTVar queue as ≫ return (return a)
    _         → return mzero
