{-# LANGUAGE
    BangPatterns,
    DeriveDataTypeable,
    MagicHash,
    RecordWildCards,
    TemplateHaskell,
    UnicodeSyntax
  #-}

-- Supress currently unavoidable CmdArgs warnings
{-# OPTIONS_GHC
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
import Data.Data                       (Data)
import Data.Eq                         (Eq)
import Data.Foldable                   (foldl')
import Data.Function                   (($))
import Data.IORef                      (newIORef, readIORef, writeIORef)
import Data.Int                        (Int)
import Data.List                       (any, drop, length, map, sum)
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

pr ∷ Unisex → Persona → Int
pr args p = f args
  where
    f = case p of
      Hombre             → mprob
      Mujer              → fprob
      PersonalDeLimpieza → cprob

-- Probabilidad acumulada hasta este tipo de persona, inclusive
accPr ∷ Unisex → Persona → Int
accPr args = sum ∘ map (pr args) ∘ enumFromTo minBound

-- Probabilidad acumulada hasta este tipo de persona, no inclusive
accPr' ∷ Unisex → Persona → Int
accPr' args p = if p ≡ minBound then 0 else accPr args (pred p)

unAccPr ∷ Unisex → Int → Persona
unAccPr args n
  | n ≤ accPr args Hombre = Hombre
  | n ≤ accPr args Mujer  = Mujer
  | otherwise             = PersonalDeLimpieza

randomP ∷ (RandomGen γ) ⇒ Unisex → γ → (Persona, γ)
randomP args = randomRP args (minBound, maxBound)

randomRP ∷ (RandomGen γ) ⇒ Unisex → (Persona, Persona) → γ → (Persona, γ)
randomRP args = (succ ∘ accPr' args) ⁂ accPr args ⋙ randomR ⋙ (⋙ first (unAccPr args))



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

delay ∷ Unisex → Persona → Int
delay args p = f args
  where
    f = case p of
      Hombre             → mdelay
      Mujer              → fdelay
      PersonalDeLimpieza → cdelay

range ∷ Unisex → Persona → Int
range args p = f args
  where
    f = case p of
      Hombre             → mrange
      Mujer              → frange
      PersonalDeLimpieza → crange

capacity ∷ Unisex → Persona → Int
capacity args p = f args
  where
    f = case p of
      Hombre             → mcapacity
      Mujer              → fcapacity
      PersonalDeLimpieza → ccapacity



generator ∷ Shared → IO ()
generator s @ S {..} = forever $ do
  r ← getStdRandom $ randomP args

  let
    push = case r of
      PersonalDeLimpieza → pushFront
      _                  → pushBack

  atomically $ do
    push s r
    writeTChan output $ "Nuevo " ⧺ show r
    report s

  let
    d = gdelay args
    j = grange args
  t ← getStdRandom $ randomR (d - j, d + j)
  threadDelay $ t


printer ∷ Shared → IO ()
printer S {..} = forever $ do
  putStrLn =≪ (atomically $ readTChan output)


door ∷ Shared → IO ()
door s @ S {..} = forever $ do
  p ← atomically $ do
    p ← maybe retry return =≪ pop s
    b ← readTVar baño
    case unBaño b of
      Nothing → writeTVar baño $ Baño $ return (p, 1)
      Just (bp, n) → do
        when (p ≠ bp ∨ n ≡ capacity args p) retry
        writeTVar baño $ Baño $ return (p, succ n)
    writeTChan output $ "Entra " ⧺ show p
    report s
    return p

  let
    d = delay args p
    j = range args p
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
  when (
    any (< 0) $ map ($ args)
      [ gdelay, grange
      , mdelay, mrange, mcapacity, mprob
      , fdelay, frange, fcapacity, fprob
      , cdelay, crange, ccapacity, cprob
      ]
    ) $ do
      putStrLn "Ningún argumento numérico puede ser negativo."
      exitFailure

  output ← newTChanIO
  queue  ← newTVarIO empty
  baño   ← newTVarIO $ Baño mzero
  ids    ← newIORef mzero
  let
    threads = [generator, printer, door]

    s = S {..}
    start = uninterruptibleMask_ $ writeIORef ids =≪ mapM fork threads
    sleep = forever $ threadDelay maxBound
    clean = readIORef ids ≫= mapM_ killThread

    fork t = forkIOWithUnmask $ \ u -> u (t s)

  atomically $ report s
  (start ≫ sleep) `finally` clean
