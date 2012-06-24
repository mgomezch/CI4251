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
import Data.Bool.Unicode               ((∧), (∨))
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
import Data.Bool                       (Bool(False))
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
import Data.Tuple                      (fst)
import Data.Typeable                   (Typeable)
import Prelude                         ( (*), (+), (-), (^), otherwise, undefined
                                       , Bounded, minBound, maxBound
                                       , Enum, enumFromTo, pred, succ
                                       , Show, show
                                       )
import System.Console.CmdArgs.Implicit (Ann, groupname, help, ignore, helpArg, name, program, summary, typ, versionArg)
import System.Console.CmdArgs.Quote    ((&=#), cmdArgs#, cmdArgsQuote)
import System.Exit                     (exitFailure)
import System.IO                       (IO, putStrLn)
import System.Random                   (Random, RandomGen, getStdRandom, newStdGen, randomR)



def ∷ String → Int
def a = case a of
  "gdelay" →  500000
  "grange" →  100000

  "mdelay" →  400000
  "mrange" →  200000
  "mcap"   →       3
  "mprob"  →      49

  "fdelay" →  700000
  "frange" →  400000
  "fcap"   →       3
  "fprob"  →      49

  "cdelay" → 1000000
  "crange" →  100000
  "ccap"   →       1
  "cprob"  →       2
  _        → undefined



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

pr ∷ (?args ∷ Args) ⇒ Persona → Int
pr p = f ?args
  where
    f = case p of
      Hombre             → mprob
      Mujer              → fprob
      PersonalDeLimpieza → cprob

-- Probabilidad acumulada hasta este tipo de persona, inclusive
accPr ∷ (?args ∷ Args) ⇒ Persona → Int
accPr = sum ∘ map pr ∘ enumFromTo minBound

-- Probabilidad acumulada hasta este tipo de persona, no inclusive
accPr' ∷ (?args ∷ Args) ⇒ Persona → Int
accPr' p = if p ≡ minBound then 0 else accPr (pred p)

unAccPr ∷ (?args ∷ Args) ⇒ Int → Persona
unAccPr n
  | n ≤ accPr Hombre = Hombre
  | n ≤ accPr Mujer  = Mujer
  | otherwise        = PersonalDeLimpieza

randomP ∷ (?args ∷ Args, RandomGen γ) ⇒ γ → (Persona, γ)
randomP = randomRP (minBound, maxBound)

randomRP ∷ (?args ∷ Args, RandomGen γ) ⇒ (Persona, Persona) → γ → (Persona, γ)
randomRP = (succ ∘ accPr') ⁂ accPr ⋙ randomR ⋙ (⋙ first unAccPr)



newtype Baño = Baño { unBaño ∷ Maybe (Persona, Int) }

instance Show Baño where
  show Baño {..} = maybe hit miss unBaño where
    hit         = "Baño libre        "
    miss (p, n) = "Baño ocupado (" ⧺ show n ⧺ " " ⧺ show p ⧺ ")"



data Shared = S
  { output ∷ TChan String
  , queue  ∷ TVar (Seq Persona)
  , baño   ∷ TVar Baño
  , countC ∷ TVar Int
  }

data Args = Args
  { gdelay, grange              ∷ Int
  , mdelay, mrange, mcap, mprob ∷ Int
  , fdelay, frange, fcap, fprob ∷ Int
  , cdelay, crange, ccap, cprob ∷ Int
  , c1                          ∷ Bool
  }
  deriving (Data, Show, Typeable)

delay ∷ (?args ∷ Args) ⇒ Persona → Int
delay p = f ?args
  where
    f = case p of
      Hombre             → mdelay
      Mujer              → fdelay
      PersonalDeLimpieza → cdelay

range ∷ (?args ∷ Args) ⇒ Persona → Int
range p = f ?args
  where
    f = case p of
      Hombre             → mrange
      Mujer              → frange
      PersonalDeLimpieza → crange

cap ∷ (?args ∷ Args) ⇒ Persona → Int
cap p = f ?args
  where
    f = case p of
      Hombre             → mcap
      Mujer              → fcap
      PersonalDeLimpieza → ccap



generator ∷ (?args ∷ Args) ⇒ Shared → IO ()
generator s @ S {..} = forever $ do
  g ← newStdGen

  atomically $ do
    c ← readTVar countC
    let
      r  = fst $ randomP g
      r' = fst $ randomRP (Hombre, Mujer) g

      p =
        if c1 ?args
           ∧ r ≡ PersonalDeLimpieza
           ∧ c ≠ 0
        then r'
        else r

      push = case p of
        PersonalDeLimpieza → pushFront
        _                  → pushBack

    push s p
    when (p ≡ PersonalDeLimpieza) $ modifyTVar countC succ
    writeTChan output $ "Nuevo " ⧺ show p
    report s

  let
    d = gdelay ?args
    j = grange ?args
  t ← getStdRandom $ randomR (d - j, d + j)
  threadDelay t


printer ∷ Shared → IO ()
printer S {..} = forever $ putStrLn =≪ (atomically $ readTChan output)


door ∷ (?args ∷ Args) ⇒ Shared → IO ()
door s @ S {..} = forever $ do
  g ← newStdGen

  (p, t) ← atomically $ do
    p ← maybe retry return =≪ pop s
    b ← readTVar baño
    case unBaño b of
      Nothing → writeTVar baño $ Baño $ return (p, 1)
      Just (bp, n) → do
        when (p ≠ bp ∨ n ≡ cap p) retry
        writeTVar baño $ Baño $ return (p, succ n)

    let
      d = delay p
      j = range p
      t = fst $ randomR (d - j, d + j) g

    writeTChan output $ "Entra " ⧺ show p ⧺ "(t = " ⧺ show t ⧺ "μs)"
    report s
    return (p, t)

  forkIO $ user p t s


user ∷ Persona → Int → Shared → IO ()
user p t s @ S {..} = do
  threadDelay t
  atomically $ do
    let f (x, n) = guard (n ≠ 1) ≫ return (x, pred n)
    modifyTVar baño (Baño ∘ f ∘ fromJust ∘ unBaño)
    when (p ≡ PersonalDeLimpieza) $ modifyTVar countC pred
    writeTChan output $ "Sale " ⧺ show p ⧺ " (t = " ⧺ show t ⧺ "μs)"
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


hdelay, hrange, hcap, hprob ∷ String
hdelay = "Tiempo promedio de uso del baño"
hrange = "Radio de variación del tiempo de uso"
hcap   = "Número máximo que puede usar el baño simultáneamente"
hprob  = "Probabilidad de generación"
hc1    = "(off) Restringir la ocurrencia simultánea a 1"

gG, gM, gF, gC ∷ Ann
gG = groupname "Generación"
gM = groupname "\nHombres"
gF = groupname "\nMujeres"
gC = groupname "\nPersonal de limpieza"

h ∷ String → Ann
h a = help $ "(" ⧺ show (def a) ⧺ ") " ⧺ case a of
  "mdelay" → hdelay; "mrange" → hrange; "mprob" → hprob; "mcap" → hcap
  "fdelay" → hdelay; "frange" → hrange; "fprob" → hprob; "fcap" → hcap
  "cdelay" → hdelay; "crange" → hrange; "cprob" → hprob; "ccap" → hcap
  "gdelay" → "Tiempo promedio de generación"
  "grange" → "Radio de variación del tiempo promedio de generación"
  _        → undefined

$(cmdArgsQuote [d|
  unisex = let n = name; t = typ in Args
    { gdelay = def "gdelay" &=# gG &=# n "g" &=# t "μs" &=# h "gdelay"
    , grange = def "grange"        &=# n "G" &=# t "μs" &=# h "grange"

    , mdelay = def "mdelay" &=# gM &=# n "m" &=# t "μs" &=# h "mdelay"
    , mrange = def "mrange"        &=# n "M" &=# t "μs" &=# h "mrange"
    , mcap   = def "mcap"                    &=# t "n"  &=# h "mcap"
    , mprob  = def "mprob"                   &=# t "%"  &=# h "mprob"

    , fdelay = def "fdelay" &=# gF &=# n "f" &=# t "μs" &=# h "fdelay"
    , frange = def "frange"        &=# n "F" &=# t "μs" &=# h "frange"
    , fcap   = def "fcap"                    &=# t "n"  &=# h "fcap"
    , fprob  = def "fprob"                   &=# t "%"  &=# h "fprob"

    , cdelay = def "cdelay" &=# gC &=# n "c" &=# t "μs" &=# h "cdelay"
    , crange = def "crange"        &=# n "C" &=# t "μs" &=# h "crange"
    , ccap   = def "ccap"                    &=# t "n"  &=# h "ccap"
    , cprob  = def "cprob"                   &=# t "%"  &=# h "cprob"
    , c1     = False               &=# n "s"            &=# help hc1
    }
    &=# program "Unisex"
    &=# summary
      ( "Solución de Manuel Gómez <targen@gmail.com> a la tarea 5 de "
      ⧺ "CI4251 (Programación funcional avanzada) en Abril–Julio de "
      ⧺ "2012 en la Universidad Simón Bolívar"
      )
    &=# helpArg [help "Mostrar este mensaje de ayuda"]
    &=# versionArg [ignore]

  runArgs = cmdArgs# unisex :: IO Args
  |])



main ∷ IO ()
main = do
  args ← runArgs

  let anyNegative = everything (∨) (mkQ False ((< 0) :: Int → Bool)) args
  when anyNegative $ do
      putStrLn "Ningún argumento numérico puede ser negativo."
      exitFailure

  output ← newTChanIO
  queue  ← newTVarIO empty
  baño   ← newTVarIO $ Baño mzero
  countC ← newTVarIO 0
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
