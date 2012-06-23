{-# LANGUAGE
    BangPatterns,
    NamedFieldPuns,
    RecordWildCards,
    UnicodeSyntax
  #-}

module Main {-(main)-} where

import Control.Arrow.Unicode        ((⁂))
import Control.Category.Unicode     ((⋙))
import Control.Monad.Unicode        ((=≪), (≫), (≫=))
import Data.Bool.Unicode            ((∨))
import Data.Eq.Unicode              ((≠), (≡))
import Data.Function.Unicode        ((∘))
import Data.Ord.Unicode             ((≤))
import Prelude.Unicode              ((⧺))

import Control.Arrow                (first)
import Control.Concurrent           (forkIO, forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)
import Control.Concurrent.STM.TVar  (TVar, modifyTVar, newTVarIO, readTVar, writeTVar)
import Control.Exception            (finally, uninterruptibleMask_)
import Control.Monad                (forever, guard, mapM, mapM_, mzero, return, when)
import Control.Monad.STM            (STM, atomically, retry)
import Data.Eq                      (Eq)
import Data.Foldable                (foldl')
import Data.Function                (($))
import Data.IORef                   (newIORef, readIORef, writeIORef)
import Data.Int                     (Int)
import Data.List                    (drop, length, map, sum)
import Data.Maybe                   (Maybe(Nothing, Just), fromJust, maybe)
import Data.Sequence                ((<|), (|>), Seq, ViewL((:<)), empty, viewl)
import Data.String                  (String)
import Data.Word                    (Word)
import Prelude                      ( (*), (+), (-), (^)
                                    , Bounded, minBound, maxBound
                                    , Enum, enumFromTo, pred, succ
                                    , Show, show
                                    )
import System.IO                    (IO, putStrLn)
import System.Random                (Random, getStdRandom, random, randomR)



genDelay ∷ Int
genDelay = 500 * 10^3

delay ∷ Persona → Int
delay p = case p of
  Hombre             →   500 * 10^3
  Mujer              →  1000 * 10^3
  PersonalDeLimpieza → 10000 * 10^3

jitter ∷ Persona → Int
jitter p = case p of
  Hombre             →  200 * 10^3
  Mujer              →  400 * 10^3
  PersonalDeLimpieza → 1000 * 10^3

cap ∷ Persona → Word
cap p = case p of
  Hombre             → 3
  Mujer              → 3
  PersonalDeLimpieza → 1

pr ∷ Persona → Word
pr p = case p of
  Hombre             → 49
  Mujer              → 49
  PersonalDeLimpieza → 2



infixl 5 ⊲
infixl 5 ⊳
(⊲) = (<|)
(⊳) = (|>)



data Persona
  = Hombre
  | Mujer
  | PersonalDeLimpieza
  deriving (Bounded, Enum, Eq)

instance Show Persona where
  show p = case p of
    Hombre             → "H"
    Mujer              → "M"
    PersonalDeLimpieza → "L"

-- Probabilidad acumulada hasta este tipo de persona, inclusive
accPr ∷ Persona → Word
accPr = sum ∘ map pr ∘ enumFromTo minBound

-- Probabilidad acumulada hasta este tipo de persona, no inclusive
accPr' ∷ Persona → Word
accPr' p = if p ≡ minBound then 0 else accPr (pred p)

unAccPr ∷ Word → Persona
unAccPr n
  | n ≤ accPr Hombre             = Hombre
  | n ≤ accPr Mujer              = Mujer
  | n ≤ accPr PersonalDeLimpieza = PersonalDeLimpieza

instance Random Persona where
  random = randomR (minBound, maxBound)
  randomR = (succ ∘ accPr') ⁂ accPr ⋙ randomR ⋙ (⋙ first unAccPr)



newtype Baño = Baño { unBaño ∷ Maybe (Persona, Word) }

instance Show Baño where
  show Baño {..} = case unBaño of
    Nothing     → "Baño libre        "
    Just (p, n) → "Baño ocupado (" ⧺ show n ⧺ " " ⧺ show p ⧺ ")"



data Shared = S
  { output ∷ TChan String
  , queue  ∷ TVar (Seq Persona)
  , baño   ∷ TVar Baño
  }



main ∷ IO ()
main = do
  output ← newTChanIO
  queue  ← newTVarIO empty
  baño   ← newTVarIO $ Baño Nothing
  ids    ← newIORef []
  let
    threads =
      [ generator
      , printer
      , door
      ]

    s = S {..}
    start = uninterruptibleMask_ $ writeIORef ids =≪ mapM fork threads
    sleep = atomically mzero
    clean = readIORef ids ≫= mapM_ killThread

    fork t = forkIOWithUnmask $ \ u -> u (t s)

  atomically $ report s
  (start ≫ sleep) `finally` clean



generator ∷ Shared → IO ()
generator s @ S {..} = forever $ do
  r ← getStdRandom random

  let
    push = case r of
      PersonalDeLimpieza → pushFront
      _                  → pushBack

  atomically $ do
    push s r
    writeTChan output $ "Nuevo " ⧺ show r
    report s

  threadDelay genDelay


printer ∷ Shared → IO ()
printer S {..} = forever $ do
  putStrLn =≪ (atomically $ readTChan output)


door ∷ Shared → IO ()
door s @ S {..} = forever $ do
  p ← atomically $ do
    p ← maybe retry return =≪ pop s
    b ← readTVar baño
    case unBaño b of
      Nothing → writeTVar baño $ Baño $ Just (p, 1)
      Just (bp, n) → do
        when (p ≠ bp ∨ n ≡ cap p) retry
        writeTVar baño $ Baño $ Just (p, succ n)
    writeTChan output $ "Entra " ⧺ show p
    report s
    return p

  let
    t = delay  p
    d = jitter p
  t ← getStdRandom $ randomR (t - d, t + d)
  forkIO $ user p t s


user ∷ Persona → Int → Shared → IO ()
user p t s @ S {..} = do
  threadDelay t
  atomically $ do
    let f (x, n) = guard (n ≠ 1) ≫ return (x, pred n)
    modifyTVar baño (Baño ∘ f ∘ fromJust ∘ unBaño)
    writeTChan output $ "Sale " ⧺ show p ⧺ " (tardó " ⧺ show t ⧺ ")"
    report s




report ∷ Shared → STM ()
report S {..} = do
  q ← readTVar queue
  b ← readTVar baño

  let
    total, ms, fs, ls ∷ Int
    (ms, fs, ls) = foldl' inc (0, 0, 0) q
    total = ms + fs + ls

    inc ∷ (Int, Int, Int) → Persona → (Int, Int, Int)
    inc (!m, !f, !l) p = case p of
      Hombre             → (succ m,      f,      l)
      Mujer              → (     m, succ f,      l)
      PersonalDeLimpieza → (     m,      f, succ l)

  writeTChan output $
    show b ⧺ "; "
    ⧺ show total ⧺ " T ("
    ⧺ show ms    ⧺ " H; "
    ⧺ show fs    ⧺ " M; "
    ⧺ show ls    ⧺ " L): "
    ⧺ drop (length "fromList ") (show q)


pushFront ∷ Shared → Persona → STM ()
pushFront S {..} a = modifyTVar queue (a ⊲)


pushBack ∷ Shared → Persona → STM ()
pushBack S {..} a = modifyTVar queue (⊳ a)


pop ∷ Shared → STM (Maybe Persona)
pop S {..} = do
  q ← readTVar queue
  case viewl q of
    (a :< as) → writeTVar queue as ≫ return (return a)
    _         → return mzero
