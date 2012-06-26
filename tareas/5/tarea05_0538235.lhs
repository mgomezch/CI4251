\documentclass[11pt,fleqn]{article}

\usepackage{color}

\definecolor{brown}{rgb}{0.7,0.2,0}
\definecolor{darkgreen}{rgb}{0,0.6,0.1}
\definecolor{darkgrey}{rgb}{0.4,0.4,0.4}
\definecolor{lightgrey}{rgb}{0.95,0.95,0.95}

\usepackage[utf8]{inputenc}
\usepackage[spanish]{babel}
\usepackage{lmodern}
\usepackage[T1]{fontenc}
\usepackage{listings}
\usepackage[colorlinks=true,urlcolor=blue]{hyperref}
\usepackage{verbatim}

\lstset{
   language=Haskell,
   gobble=2,
   frame=single,
   framerule=1pt,
   showstringspaces=false,
   basicstyle=\footnotesize\ttfamily,
   keywordstyle=\textbf,
   extendedchars=\true,
   inputencoding=utf8,
   backgroundcolor=\color{lightgrey},
   literate={á}{{\'a}}1
            {é}{{\'e}}1
            {í}{{\'i}}1
            {ó}{{\'o}}1
            {ú}{{\'u}}1
            {ü}{{\"u}}1
            {ñ}{{\~n}}1
            {Á}{{\'A}}1
            {É}{{\'E}}1
            {Í}{{\'I}}1
            {Ó}{{\'O}}1
            {Ú}{{\'U}}1
            {Ü}{{\"U}}1
            {μ}{{$\mu$}}1
}



\begin{document}

\title{CI4251 - Programación Funcional Avanzada \\
Solución de la tarea 5}

\author{Manuel Gómez\\
05-38235\\
\href{mailto:targen@gmail.com}{<targen@gmail.com>}}

\date{Junio 25, 2012}

\maketitle

\pagebreak



\section{El baño unisex (10 puntos)}

Esta solución explota algunas extensiones a \textit{Haskell} disponibles
en el compilador GHC:

\begin{lstlisting}[linerange={2-10000}]

> {-# LANGUAGE
>     BangPatterns,
>     DeriveDataTypeable,
>     ImplicitParams,
>     MagicHash,
>     RecordWildCards,
>     TemplateHaskell
>   #-}

\end{lstlisting}

Se utilizó una herramienta de procesamiento de opciones de línea de
comandos llamada \texttt{CmdArgs}\cite{CmdArgs} que genera código a
tiempo de compilación con \textit{Template Haskell}; en su versión
actual, no es posible hacer que ese código generado tenga la forma
necesaria para evitar todas las advertencias sobre posibles errores que
GHC puede detectar, así que se usan estas opciones para encender todas
las advertencias salvo las que son actualmente inevitables:

\begin{lstlisting}[linerange={2-10000}]

> {-# OPTIONS_GHC
>     -Wall
>     -fno-warn-missing-fields
>     -fno-warn-missing-signatures
>     -fno-warn-unused-binds
>   #-}

\end{lstlisting}

Esta solución consiste de un programa que realiza la simulación
especificada; como se produce un ejecutable, se declara el módulo
como debe ser:

\begin{lstlisting}[linerange={2-10000}]

> module Main (main) where

\end{lstlisting}

La lista de símbolos importados de otros módulos es un poco grande y
resultaría incómodo hacer que quepa en este documento, así que solo se
incluye en su código fuente.

\begin{comment}

> import Control.Arrow                   ((***), first)
> import Control.Category                ((>>>))
> import Control.Concurrent              (forkIO, forkIOWithUnmask, killThread, threadDelay)
> import Control.Concurrent.STM.TChan    (TChan, newTChanIO, readTChan, writeTChan)
> import Control.Concurrent.STM.TVar     (TVar, modifyTVar, newTVarIO, readTVar, writeTVar)
> import Control.Exception               (finally, uninterruptibleMask_)
> import Control.Monad                   ((=<<), (>>), (>>=), forever, guard, mapM, mapM_, mzero, return, when)
> import Control.Monad.STM               (STM, atomically, retry)
> import Data.Bool                       ((&&), (||), Bool(False))
> import Data.Data                       (Data)
> import Data.Eq                         ((/=), (==), Eq)
> import Data.Foldable                   (foldl')
> import Data.Function                   (($), (.))
> import Data.Generics.Aliases           (mkQ)
> import Data.Generics.Schemes           (everything)
> import Data.IORef                      (newIORef, readIORef, writeIORef)
> import Data.Int                        (Int)
> import Data.List                       (drop, length, map, sum)
> import Data.Maybe                      (Maybe(Nothing, Just), fromJust, maybe)
> import Data.Ord                        ((<), (<=))
> import Data.Sequence                   ((<|), (|>), Seq, ViewL((:<)), empty, viewl)
> import Data.String                     (String)
> import Data.Tuple                      (fst)
> import Data.Typeable                   (Typeable)
> import Prelude                         ( (*), (+), (++), (-), (^), otherwise, undefined
>                                        , Bounded, minBound, maxBound
>                                        , Enum, enumFromTo, pred, succ
>                                        , Show, show
>                                        )
> import System.Console.CmdArgs.Implicit (Ann, groupname, help, ignore, helpArg, name, program, summary, typ, versionArg)
> import System.Console.CmdArgs.Quote    ((&=#), cmdArgs#, cmdArgsQuote)
> import System.Exit                     (exitFailure)
> import System.IO                       (IO, putStrLn)
> import System.Random                   (Random, RandomGen, getStdRandom, newStdGen, randomR)

\end{comment}

\pagebreak

Los parámetros por defecto de la simulación se capturan en una función
para facilitar el manejo de opciones de línea de comandos:

\begin{lstlisting}[linerange={2-10000}]

> def :: String -> Int
> def a = case a of
>   "gdelay" ->   500000
>   "grange" ->   100000
>
>   "mdelay" ->   400000
>   "mrange" ->   200000
>   "mcap"   ->        3
>   "mprob"  ->       49
>
>   "fdelay" ->   700000
>   "frange" ->   400000
>   "fcap"   ->        3
>   "fprob"  ->       49
>
>   "cdelay" -> 10000000
>   "crange" ->  1000000
>   "ccap"   ->        1
>   "cprob"  ->        2
>   _        -> undefined

\end{lstlisting}

Aunque el uso de \textit{strings} no es ideal, fue lo que permitió el
mejor equilibrio entre reducción de repetición de código, la posibilidad
de usar \texttt{CmdArgs} de manera cómoda, y escribir el código en un
formato razonable con líneas cortas que quepan en este documento --- más
adelante será claro por qué.

Los usuarios del baño se representan con valores de un tipo de datos
abstracto hasta el momento en que entran al baño, que es cuando tiene
sentido considerarlos entes concurrentes\footnote{Es posible simular
este problema representando a cada agente como un hilo desde que es
generado hasta que termina de usar el baño ---se puede hacer que la
entrada al baño de cada uno espere por la disponibilidad del baño y por
que el que estaba de último en la cola cuando se generó ya haya
entrado--- pero una solución así hubiera requerido usar una cantidad de
variables compartidas proporcional al tamaño de la cola, mientras que la
solución presentada en este documento utiliza una cantidad fija de
variables compartidas.}:

\begin{lstlisting}[linerange={2-10000}]

> data Persona
>   = Hombre
>   | Mujer
>   | PersonalDeLimpieza
>   deriving (Bounded, Enum, Eq)

\end{lstlisting}

\pagebreak

La representación textual es abreviada porque se usa para mostrar en el
terminal el estado de la simulación, y es bastante incómodo si se
muestra más que una letra por cada usuario del baño en la cola:

\begin{lstlisting}[linerange={2-10000}]

> instance Show Persona where
>   show p = case p of
>     Hombre             -> "M"
>     Mujer              -> "F"
>     PersonalDeLimpieza -> "C"

\end{lstlisting}

Como se desea poder generar valores aleatorios de este tipo, lo ideal
sería hacer que instancie la clase \texttt{Random} definida en
\texttt{System.Random}.  Esto funciona bien si se mantiene fija la
distribución de probabilidad, pero es deseable que sea un parámetro de
la simulación que pueda variarse entre corridas para estudiar mejor el
problema.  Como las firmas de las funciones de la clase \texttt{Random}
no admiten más parámetros que los que tienen, que son insuficientes para
poder alterar la distribución, no es posible\footnote{Sí es posible,
pero requiere que esas funciones, que son puras, dependan de más que sus
parámetros; esto es fácil de lograr con \texttt{unsafePerformIO} e
\texttt{IORef}s, pero se decidió usar una solución pura aunque
implique desaprovechar los mecanismos definidos en las bibliotecas
estándar para la clase \texttt{Random}} generar los valores deseados
usando directamente los mecanismos de esa clase.  Sin embargo, se pueden
imitar.

Primero, se define una función que produce la probabilidad de generación
de un cierto tipo de agente de la simulación; ese dato se obtiene de un
parámetro implícito que proviene de la configuración de la simulación
\footnote{La manera usual de manejar una configuración constante podría
involucrar al \textit{monad} \texttt{Reader}, pero se decidió usar el
mecanismo de parámetros implícitos porque simplifica la forma del código
un poco más de lo que lo hubiera hecho el uso de \textit{monads}.  Este
tema se discute en un artículo breve pero interesante de Edward Z. Yang
\cite{YangImplicitParams}.} definida por los valores por defecto antes
mostrados y las opciones de línea de comandos.

\begin{lstlisting}[linerange={2-10000}]

> pr :: (?args :: Args) => Persona -> Int
> pr p = f ?args
>   where
>     f = case p of
>       Hombre             -> mprob
>       Mujer              -> fprob
>       PersonalDeLimpieza -> cprob

\end{lstlisting}

\pagebreak

También resultará útil definir una función que calcule la probabilidad
acumulada desde el primer tipo de agente hasta alguno en particular,
inclusive:

\begin{lstlisting}[linerange={2-10000}]

> accPr :: (?args :: Args) => Persona -> Int
> accPr = sum . map pr . enumFromTo minBound

\end{lstlisting}

Y otra que hace lo mismo pero sin incluir al tipo de agente indicado:

\begin{lstlisting}[linerange={2-10000}]

> accPr' :: (?args :: Args) => Persona -> Int
> accPr' p = if p == minBound then 0 else accPr (pred p)

\end{lstlisting}

Y otra que transforme un número aleatorio en los rangos definidos por
las funciones anteriores a un tipo de agente:

\begin{lstlisting}[linerange={2-10000}]

> unAccPr :: (?args :: Args) => Int -> Persona
> unAccPr n
>   | n <= accPr Hombre = Hombre
>   | n <= accPr Mujer  = Mujer
>   | otherwise         = PersonalDeLimpieza

\end{lstlisting}

Ahora, dado un rango de tipos de agentes, se puede calcular un tipo de
agente aleatorio en ese rango, y se puede calcular un tipo de agente
arbitrario tomando el rango completo:

\begin{lstlisting}[linerange={2-10000}]

> randomRP
>   :: (?args :: Args, RandomGen g)
>   => (Persona, Persona)
>   -> g
>   -> (Persona, g)
> randomRP = (succ.accPr') *** accPr
>            >>> randomR
>            >>> (>>> first unAccPr)
>
> randomP :: (?args :: Args, RandomGen g) => g -> (Persona, g)
> randomP = randomRP (minBound, maxBound)

\end{lstlisting}

Estas dos últimas funciones evidentemente corresponden a las necesarias
para una definición mínima de la clase \texttt{System.Random}, pero sus
tipos son ligeramente distintos: toman un parámetro implícito con las
opciones de línea de comandos que se propaga automáticamente a las demás
funciones para que hagan el cálculo según la distribución de
probabilidad especificada al ejecutar la simulación.  Esos parámetros,
claro, tienen por defecto los valores especificados en el enunciado de
la tarea.

\pagebreak

El estado del baño se representa con un tipo sencillo:

\begin{lstlisting}[linerange={2-10000}]

> newtype Baño = Baño { unBaño :: Maybe (Persona, Int) }
>
> instance Show Baño where
>   show Baño {..} = maybe hit miss unBaño where
>     hit = "Baño libre        "
>     miss (p, n) =
>       "Baño ocupado (" ++ show n ++ " " ++ show p ++ ")"

\end{lstlisting}

El baño puede estar libre, o puede tener una cierta cantidad de agentes
del mismo tipo.  Los espacios de más en la representación textual del
baño vacío son intencionales: mejoran ligeramente la regularidad de la
impresión del estado de la simulación.

Los datos compartidos entre hilos se agrupan en un registro para
facilitar su pasaje a donde hagan falta:

\begin{lstlisting}[linerange={2-10000}]

> data Shared = S
>   { output :: TChan String
>   , queue  :: TVar (Seq Persona)
>   , baño   :: TVar Baño
>   , countC :: TVar Int
>   }

\end{lstlisting}

\begin{enumerate}
  \item
    \texttt{output} es un canal al que cualquier hilo puede escribir
    texto que desee producir atómicamente a la salida.  Esto es
    necesario para que no se mezcle la escritura de varios hilos; habrá
    un solo hilo encargado de leer de ese canal y producir la salida.

  \item
    \texttt{queue} contiene a la cola para entrar al baño.

  \item
    \texttt{baño} contiene el estado del baño.

  \item
    \texttt{countC} contiene al número total de agentes de personal de
    limpieza activos en la simulación, tanto dentro del baño como en
    la cola.  Se usa para controlar el modo de simulación en el cual
    solo puede haber uno a la vez.
\end{enumerate}

\pagebreak

La configuración de la simulación modificable por opciones de línea de
comandos se almacenan en otro registro que facilita su pasaje implícito
a todas las partes del programa que requieran información de los
parámetros de la simulación:

\begin{lstlisting}[linerange={2-10000}]

> data Args = Args
>   { gdelay, grange              :: Int
>   , mdelay, mrange, mcap, mprob :: Int
>   , fdelay, frange, fcap, fprob :: Int
>   , cdelay, crange, ccap, cprob :: Int
>   , c1                          :: Bool
>   }
>   deriving (Data, Show, Typeable)

\end{lstlisting}

\begin{enumerate}
  \item
    Los campos \texttt{delay} especifican tiempos promedio de espera
    para ciertas acciones, como el tiempo de uso del baño de un tipo
    de agente, o el tiempo de espera entre cada generación de agente.

  \item
    Los tiempos antes mencionados son generados aleatoriamente con una
    distribución uniforme en un intervalo cuyo centro está en un campo
    \texttt{delay}, y cuyo radio está en el campo \texttt{range} que le
    corresponde.

  \item
    Los campos \texttt{cap} especifican cuántos agentes de cada tipo
    pueden utilizar el baño simultáneamente.

  \item
    Los campos \texttt{prob} especifican la probabilidad de generación
    de cada tipo de agente.  La probabilidad es entera y está en base
    a la suma de todas; así, por ejemplo, si \texttt{mprob = 3},
    \texttt{fprob = 5} y \texttt{cprob = 1}, entonces la probabilidad
    de generar una mujer es $\frac{5}{9}$.

  \item
    Los campos \texttt{g} se relacionan con generación, los \texttt{m}
    con hombres, los \texttt{f} con mujeres, y los \texttt{c} con el
    personal de limpieza.

  \item
    El campo \texttt{c1} especifica si la simulación debe forzar que
    pueda existir a lo sumo un personal de limpieza a la vez.
\end{enumerate}

Las instancias de \texttt{Data} y \texttt{Typeable} son necesarias para
el trabajo con \texttt{CmdArgs}, y además se explotan para recorrer la
estructura en la verificación de correctitud de las opciones de línea
de comandos usando funciones de programación genérica disponibles en
el módulo \texttt{Data.Generics}.

\pagebreak

Resulta conveniente definir algunas funciones para extraer datos de este
registro según el tipo de agente en consideración --- y como tienen un
patrón común sencillo, se abstrae:

\begin{lstlisting}[linerange={2-10000}]

> getArgs
>   :: (?args :: Args)
>   => (Args -> Int) -> (Args -> Int) -> (Args -> Int)
>   -> Persona -> Int
> getArgs m f c p = getter ?args where
>   getter = case p of
>     Hombre             -> m
>     Mujer              -> f
>     PersonalDeLimpieza -> c
>
> delay, range, cap :: (?args :: Args) => Persona -> Int
> delay = getArgs mdelay fdelay cdelay
> range = getArgs mrange frange crange
> cap   = getArgs mcap   fcap   ccap

\end{lstlisting}

La simulación es controlada por varios hilos que interactúan a través
de variables transaccionales: uno encargado de la impresión del estado
de la simulación, un generador de agentes, un coordinador de la entrada
al baño, y los hilos de cada agente que está en el baño.  El generador,
el impresor y el coordinador de entrada simplemente operan en ciclos
infinitos haciendo sus tareas; los hilos de agentes que usan el baño,
en cambio, desperdician tiempo una vez y luego desaparecen.

El hilo generador produce un generador de números aleatorios, y con él
genera un tipo de agente aleatoriamente según la distribución de
probabilidad adecuada; luego, encola a un agente de ese tipo en la parte
de la cola que le corresponde, y finalmente espera un tiempo aleatorio
antes de la siguiente generación:

\begin{lstlisting}[linerange={2-10000}]

> generator :: (?args :: Args) => Shared -> IO ()
> generator s @ S {..} = forever $ do
>   g <- newStdGen
>
>   atomically $ do
>     c <- readTVar countC
>     let
>       r  = fst $ randomP g
>       r' = fst $ randomRP (Hombre, Mujer) g
>
>       p =
>         if c1 ?args
>            && r == PersonalDeLimpieza
>            && c /= 0
>         then r'
>         else r
>
>       push = case p of
>         PersonalDeLimpieza -> pushFront
>         _                  -> pushBack
>
>     push s p
>     when (p == PersonalDeLimpieza) $ modifyTVar countC succ
>     writeTChan output $ "Nuevo " ++ show p
>     report s
>
>   let
>     d = gdelay ?args
>     j = grange ?args
>   t <- getStdRandom $ randomR (d - j, d + j)
>   threadDelay t

\end{lstlisting}

Se genera tanto un agente arbitrario (en \texttt{r}) como un agente que
solo puede ser hombre o mujer (en \texttt{r'}), excluyendo al caso del
personal de limpieza, para majenar el modo de simulación donde solo
puede existir un personal de limpieza a la vez; si ya existía en alguna
parte, entonces se genera \texttt{r'} en vez de \texttt{r}.

El hilo que imprime es muy sencillo:

\begin{lstlisting}[linerange={2-10000}]

> printer :: Shared -> IO ()
> printer S {..} = forever $ do
>   putStrLn =<< (atomically $ readTChan output)

\end{lstlisting}

El hilo que coordina la entrada de agentes al baño debe esperar a que
haya un agente en la cola, y cuando lo haya, debe esperar a que pueda
entrar al baño (porque está desocupado, o porque está ocupado por
otros agentes de su mismo tipo y no ha saturado su capacidad para ese
tipo de agente; cuando sea así, debe sacar a ese agente de la cola y
modificar el estado del baño para indicar que está adentro.  Finalmente,
debe crear el hilo que representa al agente haciendo en el baño lo que
tenga que hacer y perdiendo tiempo.  Ese tiempo que el agente perderá
dentro del baño es calculado por este hilo.

\begin{lstlisting}[linerange={2-10000}]

> door :: (?args :: Args) => Shared -> IO ()
> door s @ S {..} = forever $ do
>   g <- newStdGen
>
>   (p, t) <- atomically $ do
>     p <- maybe retry return =<< pop s
>     b <- readTVar baño
>     case unBaño b of
>       Nothing -> writeTVar baño $ Baño $ return (p, 1)
>       Just (bp, n) -> do
>         when (p /= bp || n == cap p) retry
>         writeTVar baño $ Baño $ return (p, succ n)
>
>     let
>       d = delay p
>       j = range p
>       t = fst $ randomR (d - j, d + j) g
>
>     writeTChan output $
>       "Entra " ++ show p ++ " (t = " ++ show t ++ "μs)"
>     report s
>     return (p, t)
>
>   forkIO $ user p t s

\end{lstlisting}

Finalmente, los hilos que representan a los usuarios del baño son
bastante más simples; solo deben perder el tiempo que les corresponde
perder, actualizar el estado del baño para indicar que salieron, y
terminar:

\begin{lstlisting}[linerange={2-10000}]

> user :: Persona -> Int -> Shared -> IO ()
> user p t s @ S {..} = do
>   threadDelay t
>   atomically $ do
>     let f (x, n) = guard (n /= 1) >> return (x, pred n)
>     modifyTVar baño (Baño . f . fromJust . unBaño)
>     when (p == PersonalDeLimpieza) $ modifyTVar countC pred
>     writeTChan output $
>       "Sale " ++ show p ++ " (t = " ++ show t ++ "μs)"
>     report s

\end{lstlisting}



Cada vez que una acción transaccional opera sobre el estado mutable
compartido, debe producir una representación textual del nuevo estado de
la simulación en el canal de donde el hilo impresor lee para imprimir:

\begin{lstlisting}[linerange={2-10000}]

> report :: Shared -> STM ()
> report S {..} = do
>   q <- readTVar queue
>   b <- readTVar baño
>
>   let
>     total, ms, fs, ls :: Int
>     (ms, fs, ls) = foldl' inc (0, 0, 0) q
>     total = ms + fs + ls
>
>     inc :: (Int, Int, Int) -> Persona -> (Int, Int, Int)
>     inc (!m, !f, !l) p = case p of
>       Hombre             -> (succ m,      f,      l)
>       Mujer              -> (     m, succ f,      l)
>       PersonalDeLimpieza -> (     m,      f, succ l)
>
>   writeTChan output $
>     show b ++ "; "
>     ++ show total ++ " T ("
>     ++ show ms    ++ " M; "
>     ++ show fs    ++ " F; "
>     ++ show ls    ++ " C): "
>     ++ drop (length "fromList ") (show q)

\end{lstlisting}

El formato no es particularmente explícito, pero es razonable para ver
la evolución del estado en un terminal.

Resultó conveniente definir algunas operaciones transaccionales simples
que abstraen ligeramente el trabajo con la cola:

\begin{lstlisting}[linerange={2-10000}]

> pushFront :: Shared -> Persona -> STM ()
> pushFront S {..} a = modifyTVar queue (a <|)
>
> pushBack :: Shared -> Persona -> STM ()
> pushBack S {..} a = modifyTVar queue (|> a)
>
> pop :: Shared -> STM (Maybe Persona)
> pop S {..} = do
>   q <- readTVar queue
>   case viewl q of
>     (a :< as) -> writeTVar queue as >> return (return a)
>     _         -> return mzero

\end{lstlisting}

El texto de ayuda para las opciones de línea de comandos tiene cierta
regularidad:

\begin{lstlisting}[linerange={2-10000}]

> hdelay, hrange, hcap, hprob, hc1 :: String
> hdelay = "Tiempo promedio de uso del baño"
> hrange = "Radio de variación del tiempo de uso"
> hcap   = "Número máximo que puede usar el baño simultáneamente"
> hprob  = "Probabilidad de generación"
> hc1    = "(off) Restringir la ocurrencia simultánea a 1"

\end{lstlisting}

Las opciones de línea de comandos están agrupadas según el tipo de
agente que afectan, y las que controlan la generación están aparte:

\begin{lstlisting}[linerange={2-10000}]

> gG, gM, gF, gC :: Ann
> gG = groupname "Generación"
> gM = groupname "\nHombres"
> gF = groupname "\nMujeres"
> gC = groupname "\nPersonal de limpieza"

\end{lstlisting}

En la especificación de las opciones de línea de comandos hay ciertos
patrones comunes que vale la pena abstraer aunque sea para que las
líneas queden más cortas y quepan en este documento:

\begin{lstlisting}[linerange={2-10000}]

> h :: String -> Ann
> h a = help $ "(" ++ show (def a) ++ ") " ++ case a of
>   "mdelay" -> hdelay; "mrange" -> hrange
>   "mprob"  -> hprob ; "mcap"   -> hcap
>   "fdelay" -> hdelay; "frange" -> hrange
>   "fprob"  -> hprob ; "fcap"   -> hcap
>   "cdelay" -> hdelay; "crange" -> hrange
>   "cprob"  -> hprob ; "ccap"   -> hcap
>   "gdelay" -> "Tiempo promedio de generación"
>   "grange" -> "Radio de variación del tiempo de generación"
>   _        -> undefined

\end{lstlisting}

Las opciones de línea de comando se procesan usando un mecanismo del
paquete \texttt{CmdArgs} que explota \textit{Template Haskell} para
permitir definir las opciones usando una sintaxis relativamente simple
\footnote{Algunas factorizaciones no son viables porque esa parte tiene
un desarrollo algo incompleto en la versión más reciente disponible en
\textit{Hackage}, pero igual resulta ser un paquete muy conveniente.
Se evitó usar el mecanismo implícito tradicional porque depende de
suposiciones sobre efectos secundarios en cómputos puros que ya han sido
invalidadas con mejoras al optimizador de GHC en algunos cambios de
versión, y también se evitó usar el mecanismo explícito tradicional
por ser demasiado... explícito.}:

\begin{lstlisting}[linerange={2-10000}]

> $(cmdArgsQuote [d|
>  unisex = let n = name; t = typ in Args
>   {gdelay=def "gdelay" &=# gG &=# n"g" &=# t"μs" &=# h"gdelay"
>   ,grange=def "grange"        &=# n"G" &=# t"μs" &=# h"grange"
>
>   ,mdelay=def "mdelay" &=# gM &=# n"m" &=# t"μs" &=# h"mdelay"
>   ,mrange=def "mrange"        &=# n"M" &=# t"μs" &=# h"mrange"
>   ,mcap  =def "mcap"                   &=# t"n"  &=# h"mcap"
>   ,mprob =def "mprob"                  &=# t"%"  &=# h"mprob"
>
>   ,fdelay=def "fdelay" &=# gF &=# n"f" &=# t"μs" &=# h"fdelay"
>   ,frange=def "frange"        &=# n"F" &=# t"μs" &=# h"frange"
>   ,fcap  =def "fcap"                   &=# t"n"  &=# h"fcap"
>   ,fprob =def "fprob"                  &=# t"%"  &=# h"fprob"
>
>   ,cdelay=def "cdelay" &=# gC &=# n"c" &=# t"μs" &=# h"cdelay"
>   ,crange=def "crange"        &=# n"C" &=# t"μs" &=# h"crange"
>   ,ccap  =def "ccap"                   &=# t"n"  &=# h"ccap"
>   ,cprob =def "cprob"                  &=# t"%"  &=# h"cprob"
>   ,c1    =False               &=# n"s"           &=# help hc1
>   }
>   &=# program "Unisex"
>   &=# summary
>     (  "Solución de Manuel Gómez <targen@gmail.com> a la "
>     ++ "tarea 5 de CI4251 (Programación funcional avanzada) "
>     ++ "en Abril-Julio de 2012 en la Universidad Simón Bolívar"
>     )
>   &=# helpArg [help "Mostrar este mensaje de ayuda"]
>   &=# versionArg [ignore]
>
>  runArgs = cmdArgs# unisex :: IO Args
>  |])

\end{lstlisting}

El cómputo principal del programa primero debe ejecutar el procesamiento
de las opciones de línea de comando y verificar que ninguno de los
parámetros sea inválido:

\begin{lstlisting}[linerange={2-10000}]

> main :: IO ()
> main = do
>   args <- runArgs
>
>   when
>     (everything (||) (mkQ False ((< 0) :: Int -> Bool)) args)
>     $ do
>       putStrLn "Ningún argumento numérico puede ser negativo."
>       exitFailure

\end{lstlisting}

\texttt{Data.Generics} facilita mucho el trabajo: permite recorrer el
registro de argumentos de línea de comandos filtrando los campos que
almacena por su tipo, y así se puede evaluar un predicado sobre cada
miembro de tipo \texttt{Int} sin tener que escribir código que acceda
a cada parte de la estructura.  Y, claro, las instancias de
\texttt{Data} y \texttt{Typeable} que hacen todo el trabajo fueron
derivadas automáticamente.

Luego hay que crear las variables de estado compartido:

\begin{lstlisting}[linerange={2-10000}]

>   output <- newTChanIO
>   queue  <- newTVarIO empty
>   baño   <- newTVarIO $ Baño mzero
>   countC <- newTVarIO 0
>   ids    <- newIORef mzero

\end{lstlisting}

La última variable, \texttt{ids}, se usa para asegurar que todos los
hilos que ejecutan ciclos infinitos sean interrumpidos cuando el hilo
principal sea interrumpido por una excepción --- como una solicitud de
interrupción proveniente del terminal por tipear
\texttt{{\textasciicircum}C}, que es la manera usual de terminar la
simulación.  Normalmente todos los hilos terminan cuando el principal es
interrumpido, pero en GHCi no necesariamente pasa, así que los hilos
deben crearse con cuidado para evitar una condición de carrera que
dejaría corriendo a los hilos de la simulación aunque el programa
principal se interrumpa --- si no, luego no pueden matarse, y es
bastante incómodo para el desarrollo.

El \textit{binding} del parámetro implícito con la configuración de la
simulación debe hacerse al mencionar a los hilos:

\begin{lstlisting}[linerange={2-10000}]

>   let
>     ts = let ?args = args in [generator, printer, door]

\end{lstlisting}

Los campos del registro de variables compartidas a pasar al resto del
programa están precisamente en \textit{bindings} que tienen los mismos
nombres que los campos que les corresponden, así que crear el registro
resulta trivial con las extensiones de GHC activadas por
\texttt{RecordWildCards}:

\begin{lstlisting}[linerange={2-10000}]

>     s = S {..}

\end{lstlisting}

El programa primero muestra el estado inicial de la simulación, y luego
inicia con mucho cuidado a los hilos para evitar la condición de carrera
antes mencionada:

\begin{lstlisting}[linerange={2-10000}]

>     start = uninterruptibleMask_ $ writeIORef ids =<< mapM f ts
>     sleep = forever $ threadDelay maxBound
>     clean = readIORef ids >>= mapM_ killThread
>
>     f t = forkIOWithUnmask $ \ u -> u (t s)
>
>   atomically $ report s
>   (start >> sleep) `finally` clean

\end{lstlisting}

Esta solución fue desarrollada para y probada con GHC 7.4.2 usando las
librerías disponibles en la versión 2012.2.0.0 del \textit{Haskell
Platform}\footnote{Salvo porque se usó \texttt{unix-2.5.1.1} en vez de
\texttt{unix-2.5.1.0}, pero los cambios no afectan a este programa.}, y
algunos paquetes adicionales:

\begin{itemize}
  \item cmdargs-0.9.5
  \item random-1.0.1.1
  \item stm-2.3
  \item syb-0.3.6.1
  \item transformers-0.3.0.0
\end{itemize}

\pagebreak

\begin{thebibliography}{9}

\bibitem{CmdArgs}
  Neil Mitchell,
  \href{http://community.haskell.org/~ndm/cmdargs/}{
    \emph{CmdArgs: Easy Command Line Processing}
  }

\bibitem{YangImplicitParams}
  Edward Z. Yang,
  \href{http://blog.ezyang.com/2010/07/implicit-parameters-in-haskell/}{
    \emph{Reader monad and implicit parameters}
  }
  Inside 233,
  26 de julio, 2010.

\end{thebibliography}

\end{document}
