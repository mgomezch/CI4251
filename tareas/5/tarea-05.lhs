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
            {Ñ}{{\~N}}1
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
en el compilador GHC.

\begin{lstlisting}[linerange={2-10}]

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
comando llamada \texttt{CmdArgs} que genera código a tiempo de
compilación con \textit{Template Haskell}; en su versión actual, no es
posible hacer que ese código generado tenga la forma necesaria para
evitar todas las advertencias sobre posibles errores que GHC puede
detectar, así que se usan estas opciones para encender todas las
advertencias salvo las que son actualmente inevitables:

\begin{lstlisting}[linerange={2-7}]

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

\begin{lstlisting}[linerange={2-2}]

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
> import Control.Monad                   ((=<), (>>), (>>=), forever, guard, mapM, mapM_, mzero, return, when)
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
> import Data.String                     ((++), String)
> import Data.Tuple                      (fst)
> import Data.Typeable                   (Typeable)
> import Prelude                         ( (*), (+), (-), (^), otherwise, undefined
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

Los parámetros por defecto de la simulación se capturan en una función
para facilitar el manejo de opciones de línea de comando:

\begin{lstlisting}[linerange={2-21}]

> def :: String -> Int
> def a = case a of
>   "gdelay" ->  500000
>   "grange" ->  100000
>
>   "mdelay" ->  400000
>   "mrange" ->  200000
>   "mcap"   ->       3
>   "mprob"  ->      49
>
>   "fdelay" ->  700000
>   "frange" ->  400000
>   "fcap"   ->       3
>   "fprob"  ->      49
>
>   "cdelay" -> 1000000
>   "crange" ->  100000
>   "ccap"   ->       1
>   "cprob"  ->       2
>   _        -> undefined

\end{lstlisting}

Aunque el uso de \textit{strings} no es ideal, fue lo que permitió el
mejor equilibrio entre reducción de repetición de código, la posibilidad
de usar \texttt{CmdArgs} de manera cómoda, y escribir el código en un
formato razonable con líneas cortas que quepan en este documento.

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

\begin{lstlisting}[linerange={2-6}]

> data Persona
>   = Hombre
>   | Mujer
>   | PersonalDeLimpieza
>   deriving (Bounded, Enum, Eq)

\end{lstlisting}

La representación textual es abreviada porque se usa para mostrar en el
terminal el estado de la simulación, y es bastante incómodo si se
muestra más que una letra por cada usuario del baño en la cola:

\begin{lstlisting}[linerange={2-6}]

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
\cite{YangImplicitParams}} definida por los valores por defecto
antes mostrados y las opciones de línea de comando.

\begin{lstlisting}[linerange={2-8}]

> pr :: (?args :: Args) => Persona -> Int
> pr p = f ?args
>   where
>     f = case p of
>       Hombre             -> mprob
>       Mujer              -> fprob
>       PersonalDeLimpieza -> cprob

\end{lstlisting}

También resultará útil definir una función que calcule la probabilidad
acumulada desde el primer tipo de agente hasta alguno en particular,
inclusive:

\begin{lstlisting}[linerange={2-3}]

> accPr :: (?args :: Args) => Persona -> Int
> accPr = sum . map pr . enumFromTo minBound

\end{lstlisting}

Y otra que hace lo mismo pero sin incluir al tipo de agente indicado:

\begin{lstlisting}[linerange={2-3}]

> accPr' :: (?args :: Args) => Persona -> Int
> accPr' p = if p == minBound then 0 else accPr (pred p)

\end{lstlisting}

Y otra que transforme un número aleatorio en los rangos definidos por
las funciones anteriores a un tipo de agente:

\begin{lstlisting}[linerange={2-6}]

> unAccPr :: (?args :: Args) => Int -> Persona
> unAccPr n
>   | n <= accPr Hombre = Hombre
>   | n <= accPr Mujer  = Mujer
>   | otherwise         = PersonalDeLimpieza

\end{lstlisting}

Ahora, dado un rango de tipos de agentes, se puede calcular un tipo de
agente aleatorio en ese rango, y se puede calcular un tipo de agente
arbitrario tomando el rango completo:

\begin{lstlisting}[linerange={2-12}]

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
opciones de línea de comando que se propaga automáticamente a las demás
funciones para que hagan el cálculo según la distribución de
probabilidad especificada al ejecutar la simulación.  Esos parámetros,
claro, tienen por defecto los valores especificados en el enunciado de
la tarea.

El estado del baño se representa con un tipo sencillo:

\begin{lstlisting}[linerange={2-11}]

> newtype Baño = Baño { unBaño :: Maybe (Persona, Int) }
>
> instance Show Baño where
>   show Baño {..} = maybe hit miss unBaño where
>     hit         = "Baño libre        "
>     miss (p, n) = "Baño ocupado ("
>                   ++ show n
>                   ++ " "
>                   ++ show p
>                   ++ ")"

\end{lstlisting}

El baño puede estar libre, o puede tener una cierta cantidad de agentes
del mismo tipo.  Los espacios de más en la representación textual del
baño vacío son intencionales: mejoran ligeramente la regularidad de la
impresión del estado de la simulación.

Los datos compartidos entre hilos se agrupan en un registro para
facilitar su pasaje a donde hacen falta:

\begin{lstlisting}[linerange={2-7}]

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

La configuración de la simulación modificable por opciones de línea de
comando se almacenan en otro registro que facilita su pasaje implícito
a todas las partes del programa que requieran información de los
parámetros de la simulación:

\begin{lstlisting}[linerange={2-9}]

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

Resulta conveniente definir algunas funciones para extraer datos de este
registro según el tipo de agente en consideración --- y como tienen un
patrón común sencillo, se abstrae:

\begin{lstlisting}[linerange={2-15}]

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

\begin{lstlisting}[linerange={2-32}]

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

\begin{lstlisting}[linerange={2-4}]

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

\begin{lstlisting}[linerange={2-25}]

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
>       "Entra " ++ show p ++ "(t = " ++ show t ++ "μs)"
>     report s
>     return (p, t)
>
>   forkIO $ user p t s

\end{lstlisting}

Finalmente, los hilos que representan a los usuarios del baño son
bastante más simples; solo deben perder el tiempo que les corresponde
perder, actualizar el estado del baño para indicar que salieron, y
terminar:

\begin{lstlisting}[linerange={2-11}]

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



\pagebreak

\begin{thebibliography}{9}

\bibitem{YangImplicitParams}
  Edward Z. Yang,
  \href{http://blog.ezyang.com/2010/07/implicit-parameters-in-haskell/}{
    \emph{Reader monad and implicit parameters}
  }
  Inside 233,
  26 de julio, 2010.

\end{thebibliography}

\end{document}
