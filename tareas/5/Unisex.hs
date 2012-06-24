{-# LANGUAGE
    BangPatterns,
    DeriveDataTypeable,
    ImplicitParams,
    MagicHash,
    RecordWildCards,
    TemplateHaskell,
    UnicodeSyntax
  #-}

-- Supress currently unavoidable CmdArgs warnings
{-# OPTIONS_GHC
    -Wall
    -fno-warn-missing-fields
    -fno-warn-missing-signatures
    -fno-warn-unused-binds
  #-}

module Main (main) where

import Control.Arrow.Unicode           ((⁂))
import Control.Category.Unicode        ((⋙))
import Control.Monad.Unicode           ((=≪), (≫), (≫=))
import Data.Bool.Unicode               ((∨))
import Data.Eq.Unicode                 ((≠), (≡))
import Data.Function.Unicode           ((∘))
import Data.Ord.Unicode                ((≤))
import Prelude.Unicode                 ((⧺))

import Control.Arrow                   (first)
import Control.Concurrent              (forkIO, forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.STM.TChan    (TChan, newTChanIO, readTChan, writeTChan)
import Control.Concurrent.STM.TVar     (TVar, modifyTVar, newTVarIO, readTVar, writeTVar)
import Control.Exception               (finally, uninterruptibleMask_)
import Control.Monad                   (forever, guard, mapM, mapM_, mzero, return, when)
import Control.Monad.STM               (STM, atomically, retry)
import Data.Bool                       ((||), Bool(False))
import Data.Data                       (Data)
import Data.Eq                         (Eq)
import Data.Foldable                   (foldl')
import Data.Function                   (($))
import Data.Generics.Aliases           (mkQ)
import Data.Generics.Schemes           (everything)
import Data.IORef                      (newIORef, readIORef, writeIORef)
import Data.Int                        (Int)
import Data.List                       (drop, length, map, sum)
import Data.Maybe                      (Maybe(Nothing, Just), fromJust, maybe)
import Data.Ord                        ((<))
import Data.Sequence                   ((<|), (|>), Seq, ViewL((:<)), empty, viewl)
import Data.String                     (String)
import Data.Typeable                   (Typeable)
import Prelude                         ( (*), (+), (-), (^), otherwise
                                       , Bounded, minBound, maxBound
                                       , Enum, enumFromTo, pred, succ
                                       , Show, show
                                       )
import System.Console.CmdArgs.Implicit (groupname, help, ignore, helpArg, name, program, summary, typ, versionArg)
import System.Console.CmdArgs.Quote    ((&=#), cmdArgs#, cmdArgsQuote)
import System.Exit                     (exitFailure)
import System.IO                       (IO, putStrLn)
import System.Random                   (Random, RandomGen, getStdRandom, randomR)




infixl 5 ⊲
(⊲) ∷ α → Seq α → Seq α
(⊲) = (<|)

infixl 5 ⊳
(⊳) ∷ Seq α → α → Seq α
(⊳) = (|>)



data Persona
  = Hombre
  | Mujer
  | PersonalDeLimpieza
  deriving (Bounded, Enum, Eq)

instance Show Persona where
  show p = case p of
    Hombre             → "M"
    Mujer              → "F"
    PersonalDeLimpieza → "C"

pr ∷ (?args ∷ Unisex) ⇒ Persona → Int
pr p = f ?args
  where
    f = case p of
      Hombre             → mprob
      Mujer              → fprob
      PersonalDeLimpieza → cprob

-- Probabilidad acumulada hasta este tipo de persona, inclusive
accPr ∷ (?args ∷ Unisex) ⇒ Persona → Int
accPr = sum ∘ map pr ∘ enumFromTo minBound

-- Probabilidad acumulada hasta este tipo de persona, no inclusive
accPr' ∷ (?args ∷ Unisex) ⇒ Persona → Int
accPr' p = if p ≡ minBound then 0 else accPr (pred p)

unAccPr ∷ (?args ∷ Unisex) ⇒ Int → Persona
unAccPr n
  | n ≤ accPr Hombre = Hombre
  | n ≤ accPr Mujer  = Mujer
  | otherwise        = PersonalDeLimpieza

randomP ∷ (?args ∷ Unisex, RandomGen γ) ⇒ γ → (Persona, γ)
randomP = randomRP (minBound, maxBound)

randomRP ∷ (?args ∷ Unisex, RandomGen γ) ⇒ (Persona, Persona) → γ → (Persona, γ)
randomRP = (succ ∘ accPr') ⁂ accPr ⋙ randomR ⋙ (⋙ first unAccPr)



newtype Baño = Baño { unBaño ∷ Maybe (Persona, Int) }

instance Show Baño where
  show Baño {..} = maybe hit miss unBaño where
    hit         = "Baño libre        "
    miss (p, n) = "Baño ocupado (" ⧺ show n ⧺ " " ⧺ show p ⧺ ")"



data Shared = S
  { args   ∷ Unisex
  , output ∷ TChan String
  , queue  ∷ TVar (Seq Persona)
  , baño   ∷ TVar Baño
  }

data Unisex = Unisex
  { gdelay, grange ∷ Int
  , mdelay, mrange, mcapacity, mprob ∷ Int
  , fdelay, frange, fcapacity, fprob ∷ Int
  , cdelay, crange, ccapacity, cprob ∷ Int
  }
  deriving (Data, Show, Typeable)

delay ∷ (?args ∷ Unisex) ⇒ Persona → Int
delay p = f ?args
  where
    f = case p of
      Hombre             → mdelay
      Mujer              → fdelay
      PersonalDeLimpieza → cdelay

range ∷ (?args ∷ Unisex) ⇒ Persona → Int
range p = f ?args
  where
    f = case p of
      Hombre             → mrange
      Mujer              → frange
      PersonalDeLimpieza → crange

capacity ∷ (?args ∷ Unisex) ⇒ Persona → Int
capacity p = f ?args
  where
    f = case p of
      Hombre             → mcapacity
      Mujer              → fcapacity
      PersonalDeLimpieza → ccapacity



generator ∷ (?args ∷ Unisex) ⇒ Shared → IO ()
generator s @ S {..} = forever $ do
  r ← getStdRandom $ randomP

  let
    push = case r of
      PersonalDeLimpieza → pushFront
      _                  → pushBack

  atomically $ do
    push s r
    writeTChan output $ "Nuevo " ⧺ show r
    report s

  let
    d = gdelay ?args
    j = grange ?args
  t ← getStdRandom $ randomR (d - j, d + j)
  threadDelay t


printer ∷ Shared → IO ()
printer S {..} = forever $ putStrLn =≪ (atomically $ readTChan output)


door ∷ (?args ∷ Unisex) ⇒ Shared → IO ()
door s @ S {..} = forever $ do
  p ← atomically $ do
    p ← maybe retry return =≪ pop s
    b ← readTVar baño
    case unBaño b of
      Nothing → writeTVar baño $ Baño $ return (p, 1)
      Just (bp, n) → do
        when (p ≠ bp ∨ n ≡ capacity p) retry
        writeTVar baño $ Baño $ return (p, succ n)
    writeTChan output $ "Entra " ⧺ show p
    report s
    return p

  let
    d = delay p
    j = range p
  t ← getStdRandom $ randomR (d - j, d + j)
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
    ⧺ show ms    ⧺ " M; "
    ⧺ show fs    ⧺ " F; "
    ⧺ show ls    ⧺ " C): "
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



$(cmdArgsQuote [d|
  unisex = Unisex
    { gdelay    =  500000 &=# groupname "Generación"             &=# name "g" &=# typ "μs" &=# help "Tiempo promedio de generación"
    , grange    =  100000                                        &=# name "G" &=# typ "μs" &=# help "Radio de variación del tiempo promedio de generación"

    , mdelay    =  400000 &=# groupname "\nHombres"              &=# name "m" &=# typ "μs" &=# help "Tiempo promedio de uso del baño por hombres"
    , mrange    =  200000                                        &=# name "M" &=# typ "μs" &=# help "Radio de variación del tiempo promedio de uso del baño por hombres"
    , mcapacity =       3                                                     &=# typ "n"  &=# help "Número máximo de hombres que pueden usar el baño simultáneamente"
    , mprob     =      49                                                     &=# typ "%"  &=# help "Probabilidad de generación de hombres"

    , fdelay    =  700000 &=# groupname "\nMujeres"              &=# name "f" &=# typ "μs" &=# help "Tiempo promedio de uso del baño por mujeres"
    , frange    =  400000                                        &=# name "F" &=# typ "μs" &=# help "Radio de variación del tiempo promedio de uso del baño por mujeres"
    , fcapacity =       3                                                     &=# typ "n"  &=# help "Número máximo de mujeres que pueden usar el baño simultáneamente"
    , fprob     =      49                                                     &=# typ "%"  &=# help "Probabilidad de generación de mujeres"

    , cdelay    = 1000000 &=# groupname "\nPersonal de limpieza" &=# name "c" &=# typ "μs" &=# help "Tiempo promedio de ocupación del baño por personal de limpieza"
    , crange    =  100000                                        &=# name "C" &=# typ "μs" &=# help "Radio de variación del tiempo promedio de ocupación del baño por personal de limpieza"
    , ccapacity =       1                                                     &=# typ "n"  &=# help "Número máximo de personal de limpieza que puede trabajar en el baño simultáneamente"
    , cprob     =       2                                                     &=# typ "%"  &=# help "Probabilidad de generación de personal de limpieza"
    }
    &=# program "Unisex"
    &=# summary "Solución de Manuel Gómez <targen@gmail.com> a la tarea 5 de CI4251 (Programación funcional avanzada) en Abril–Julio de 2012 en la Universidad Simón Bolívar"
    &=# helpArg [help "Mostrar este mensaje de ayuda"]
    &=# versionArg [ignore]

  runArgs = cmdArgs# unisex :: IO Unisex
  |])



main ∷ IO ()
main = do
  args ← runArgs

  let anyNegative = everything (||) (mkQ False ((< 0) :: Int → Bool)) args
  when anyNegative $ do
      putStrLn "Ningún argumento numérico puede ser negativo."
      exitFailure

  output ← newTChanIO
  queue  ← newTVarIO empty
  baño   ← newTVarIO $ Baño mzero
  ids    ← newIORef mzero

  let
    threads = let ?args = args in [generator, printer, door]

    s = S {..}
    start = uninterruptibleMask_ $ writeIORef ids =≪ mapM fork threads
    sleep = forever $ threadDelay maxBound
    clean = readIORef ids ≫= mapM_ killThread

    fork t = forkIOWithUnmask $ \ u -> u (t s)

  atomically $ report s
  (start ≫ sleep) `finally` clean
