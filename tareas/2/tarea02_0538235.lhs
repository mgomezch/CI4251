\documentclass[11pt,fleqn]{article}

\usepackage{tikz}
\usepackage{multicol}
\usepackage{latexsym}
\usepackage{array}
\usepackage[english,spanish]{babel}
\usepackage{lmodern}
\usepackage{listings}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[colorlinks=true,urlcolor=blue]{hyperref}
\usepackage{xcolor}

\usepackage{framed}
\usepackage{verbatim}

\usepackage{algorithmic}
\usepackage{algorithm}

\usetikzlibrary{positioning,shapes,folding,positioning,shapes,trees}

\hypersetup{
  colorlinks=true,
  linkcolor=blue,
  urlcolor=blue
}

\definecolor{brown}{rgb}{0.7,0.2,0}
\definecolor{darkgreen}{rgb}{0,0.6,0.1}
\definecolor{darkgrey}{rgb}{0.4,0.4,0.4}
\definecolor{midgrey}{rgb}{0.9,0.9,0.9}
\definecolor{lightgrey}{rgb}{0.95,0.95,0.95}



\lstset{
   language=Haskell,
   gobble=2,
   frame=single,
   framerule=1pt,
   showstringspaces=false,
   basicstyle=\footnotesize\ttfamily,
   keywordstyle=\textbf,
   backgroundcolor=\color{lightgrey}
}

\newenvironment{answer}{%
  \def\FrameCommand{\fboxsep=\FrameSep \fcolorbox{black}{midgrey}}%
  \color{black}\MakeFramed {\FrameRestore}}%
 {\endMakeFramed}

\begin{document}

\begin{comment}

Dejé comentadas las listas de símbolos a importar por si el proceso de pruebas de esta solución utiliza más de lo que ella misma necesita.  Por si acaso.

> import qualified Data.Foldable as DF -- (mapM_)
> import qualified Data.Map      as DM -- (fromList, lookup)
> import qualified Data.Sequence as DS -- (Seq, singleton)
>
> import Control.Monad.Error  -- (Error , throwError, noMsg    )
> import Control.Monad.State  -- (State , modify    , runState )
> import Control.Monad.Writer -- (Writer, tell      , runWriter)
>
> import Data.Maybe (fromJust)


\end{comment}

\title{CI4251 - Programación Funcional Avanzada\\
Solución de la tarea 2}

\author{Manuel Gómez\\
05-38235\\
\href{mailto:targen@gmail.com}{<targen@gmail.com>}}

\date{\today}

\maketitle

\pagebreak

\section*{Arboles de Expresiones y su Evaluación}

\begin{lstlisting}
  import qualified Data.Map as DM
\end{lstlisting}

Considere el siguiente TAD diseñado para representar árboles abstractos
conteniendo expresiones aritméticas sobre números enteros y variables:

\begin{lstlisting}[linerange={2-8}]

> data Exp = Const Int            -- constante manifiesta
>          | Var String           -- nombre de variable
>          | Add Exp Exp
>          | Sub Exp Exp
>          | Mul Exp Exp
>          | Div Exp Exp
>          deriving (Eq,Show)

\end{lstlisting}

Queremos evaluar las expresiones representadas por esos árboles en
el contexto de un ambiente de referencia \emph{inmutable} que asocia
valores a variables apoyándose en \texttt{Data.Map}

\begin{lstlisting}[linerange={2-5}]

> env = DM.fromList [ 
>                     ("foo",42), ("bar",69),
>                     ("baz",27), ("qux",0)
>                   ]

\end{lstlisting}

La intención del tipo de datos es construir expresiones como
\footnote{La indentación en las expresiones es simplemente para
facilitar su comprensión -- es importante que Ud. construya
expresiones de prueba más complejas para ejercitar correctamente
su solución. Note que la expresión \texttt{ex6}
contiene una división por cero}

\begin{lstlisting}[linerange={2-11}]

> ex1 = (Const 21)
> ex2 = (Var "foo")
> ex3 = (Mul (Const 2) ex1)
> ex4 = (Add (Mul ex2 (Const 5)) (ex3))
> ex5 = (Add (Div (Const 42) (Const 2)) 
>            (Mul (Const 3) (Const 7))
>       )
> ex6 = (Div (Var "bar") 
>            (Sub (Add (Const 2) (Const 3)) (Const 5)))
> ex7 = (Div (Add (Const 6) (Const 7)) (Const 13))

\end{lstlisting}

Evaluar esas expresiones de manera \emph{pura} es simple

\begin{lstlisting}[linerange={2-7}]

> eval (Const i) = i
> eval (Var n)   = fromJust (DM.lookup n env)
> eval (Add l r) = (eval l) + (eval r)
> eval (Sub l r) = (eval l) - (eval r)
> eval (Mul l r) = (eval l) * (eval r)
> eval (Div l r) = (eval l) `div` (eval r)

\end{lstlisting}

\pagebreak

\section*{Evaluador con cálculo de costos (3 puntos)}

Se desea implantar un evaluador de expresiones que sea capaz de
contabilizar la cantidad de sumas, restas, multiplicaciones, divisiones y
consultas a la tabla de símbolos que se efectúan durante el cómputo.

El evaluador debe estar encapsulado en una función
tal que pueda evaluarse en GHCi directamente, i.e.

\begin{verbatim}
ghci> evalwithstats ex4
Result: 252
EvalState {adds = 1, subs = 0, muls = 2, divs = 0, vars = 1}
\end{verbatim}

Para conseguirlo, es necesario que Ud. provea:

\begin{itemize}
\item
   El tipo de datos que almacena las estadísticas

   \begin{answer}
     Resulta útil la notación especial para tipos de datos que parecen registros:

\begin{lstlisting}[linerange={2-8}]

> data EvalState = EvalState {
>   adds :: Integer,
>   subs :: Integer,
>   muls :: Integer,
>   divs :: Integer,
>   vars :: Integer
> } deriving Show

\end{lstlisting}

     Como los valores de este tipo se usan como estados en el \textit{monad} \texttt{State}, y ejecutar un cómputo en ese \textit{monad} implica suministrar un estado inicial a un proceso que lo transforma, el estado inicial debe ser un valor del tipo \texttt{EvalState} con todos los contadores en cero:

\begin{lstlisting}[linerange={2-9}]

> initialState :: EvalState
> initialState = EvalState {
>   adds = 0,
>   subs = 0,
>   muls = 0,
>   divs = 0,
>   vars = 0
> }

\end{lstlisting}
   \end{answer}

\item
   Un combinador monádico aprovechando \texttt{Control.Monad.State}
   que complete la evaluación de las expresiones acarreando
   las estadísticas
   \begin{lstlisting}
  evalST :: {- Firma para aprovechar Control.Monad.State -}
   \end{lstlisting}

   \begin{answer}
     Los combinadores monádicos que deben definirse procesan varios casos de operaciones binarias con un patrón común: computar el valor del operando izquierdo, computar el valor del operando derecho, y efectuar alguna acción usando el resultado de combinar con un operador puro a los dos valores computados anteriormente.  Resulta conveniente abstraer este patrón en un combinador monádico genérico.

     El combinador debe recibir
     \begin{enumerate}
       \item la acción monádica que recibe un árbol de expresión y computa su resultado,
       \item el árbol de la expresión del operando izquierdo,
       \item el árbol de la expresión del operando derecho,
       \item el operador puro que combina los valores computados de los operandos, y
       \item la acción monádica que usa el resultado del operador puro y computa el resultado final.
     \end{enumerate}

\begin{lstlisting}[linerange={2-12}]

> evalBin :: Monad m => (Exp -> m Int)
>                    -> Exp
>                    -> Exp
>                    -> (Int -> Int -> Int)
>                    -> (Int -> m Int)
>                    -> m Int
>
> evalBin eval l r op act = do
>   el <- eval l
>   er <- eval r
>   act $ el `op` er

\end{lstlisting}

     El cómputo de \texttt{evalST} explota el mecanismo de transformación de estados que provee el \textit{monad} \texttt{State} para incrementar los contadores de tipos de operación en \texttt{EvalState} en cada nodo relevante del árbol, y el valor que calcula es el entero resultante de evaluar la expresión:

\begin{lstlisting}[linerange={2-2}]

> evalST :: Exp -> State EvalState Int

\end{lstlisting}

     No se cuenta el número de constantes, así que el estado se preserva intacto al evaluar un nodo \texttt{Const}:

\begin{lstlisting}[linerange={2-2}]

> evalST (Const i) = return i

\end{lstlisting}

     Los nodos \texttt{Var} se cuentan, y además producen excepciones si no existe una variable en el ambiente de referencia con el nombre indicado en el nodo:

\begin{lstlisting}[linerange={2-7}]

> evalST (Var n) = do
>   modify $ \ s -> s { vars = vars s + 1 }
>   return $ maybe miss hit try
>   where miss = error $ "Var " ++ n ++ " not found"
>         hit = id
>         try = DM.lookup n env

\end{lstlisting}

     Los nodos que representan a operaciones binarias requieren efectuar una modificación al estado: el nuevo estado será igual al anterior salvo por el campo del registro que indica el número de operaciones encontradas del tipo relevante, que se incrementa; el resultado final, por su parte, es simplemente el resultado del operador aplicado a los valores computados para los subárboles:

\begin{lstlisting}[linerange={2-12}]

> evalST (Add l r) = evalBin evalST l r (+) $
>   ((modify $ \ s -> s { adds = adds s + 1 }) >>) . return
>
> evalST (Sub l r) = evalBin evalST l r (-) $
>   ((modify $ \ s -> s { subs = subs s + 1 }) >>) . return
>
> evalST (Mul l r) = evalBin evalST l r (*) $
>   ((modify $ \ s -> s { muls = muls s + 1 }) >>) . return
>
> evalST (Div l r) = evalBin evalST l r div $
>   ((modify $ \ s -> s { divs = divs s + 1 }) >>) . return

\end{lstlisting}
   \end{answer}

\item
   La función principal de evaluación que aprovecha \texttt{evalST}
   y las funciones monádicas de \texttt{Control.Monad.State}
   para efectuar la evaluación y mostrar los resultados

   \begin{lstlisting}[linerange={2-3}]

> evalwithstats :: Exp -> IO ()

   \end{lstlisting}

   \begin{answer}
     La función principal evalúa sobre el estado inicial al transformador de estados correspondiente a la evaluación del árbol dado:

\begin{lstlisting}[linerange={2-4}]

> evalwithstats e =
>   mapM_ putStrLn ["Result: " ++ show result, show state]
>   where (result, state) = evalST e `runState` initialState

\end{lstlisting}
   \end{answer}
\end{itemize}

Si la expresión a evaluar contiene una división por cero, o intenta
utilizar un símbolo que no está definido en la tabla de símbolos, es
suficiente que emita un mensaje de error usando \texttt{error}.

\begin{verbatim}
ghci> evalwithstats ex6
Result: *** Exception: divide by zero
ghci> evalwithstats (Var "meh")
evalwithstats (Var "meh")
Result: *** Exception: Var meh not found
\end{verbatim}

\section*{Evaluador con traza de operaciones (3 puntos)}

Se desea implantar un evaluador de expresiones que sea capaz de
registrar los resultados de las operaciones intermedias y
consultas a la tabla de símbolos que se efectúan durante el cómputo.

El evaluador debe estar encapsulado en una función
tal que pueda evaluarse en GHCi directamente, i.e.

\begin{verbatim}
ghci> evalwithlog ex4
Result: 252
Exp: Var "foo" -> Val: 42
Exp: Const 5 -> Val: 5
Exp: Mul (Var "foo") (Const 5) -> Val: 210
Exp: Const 2 -> Val: 2
Exp: Const 21 -> Val: 21
Exp: Mul (Const 2) (Const 21) -> Val: 42
Exp: Add (Mul (Var "foo") (Const 5)) (Mul (Const 2) (Const 21)) -> Val: 252
\end{verbatim}

Para conseguirlo, es necesario que Ud. provea:

\begin{itemize}
\item
   Un combinador monádico aprovechando \texttt{Control.Monad.Writer}
   que complete la evaluación de las expresiones acarreando
   la información sobre cómputos intermedios
   \begin{lstlisting}
  evalWR :: {- Firma para aprovechar Control.Monad.Writer -}
   \end{lstlisting}
   \emph{Debe} utilizar \texttt{Data.Sequence} como estructura de
   acumulación para los resultados intermedios.

   \begin{answer}
     Conviene declarar un \textit{alias} para el tipo de \textit{monad} usado para \texttt{evalWR}; \texttt{EvalLogger} es un cómputo en el \textit{monad} \texttt{Writer} que mantiene una bitácora con pares que contienen cada árbol de expresión y el valor resultante de evaluarlo:

\begin{lstlisting}[linerange={2-2}]

> type EvalLogger = Writer (DS.Seq (Exp, Int)) Int

\end{lstlisting}

     La acción monádica \texttt{actWR} se usa para producir datos en la bitácora al evaluar un árbol de expresión; recibe el árbol completo y el resultado de evaluarlo, emite el par correspondiente a la bitácora, y produce el resultado de la evaluación del árbol que recibió:

\begin{lstlisting}[linerange={2-3}]

> actWR :: Exp -> Int -> EvalLogger
> actWR e res = tell (DS.singleton (e, res)) >> return res

\end{lstlisting}

     La definición de \texttt{evalWR} es similar a la de \texttt{evalST}, pero las acciones monádicas que usa no modifican un estado sino que usan a \texttt{actWR} para emitir información a la bitácora:

\begin{lstlisting}[linerange={2-13}]

> evalWR :: Exp -> EvalLogger
> evalWR e@(Const i) = actWR e i
>
> evalWR e@(Var n) = maybe miss hit try where
>   miss = error $ "Var " ++ n ++ " not found"
>   hit v = actWR e v
>   try = DM.lookup n env
>
> evalWR e@(Add l r) = evalBin evalWR l r (+) $ actWR e
> evalWR e@(Sub l r) = evalBin evalWR l r (-) $ actWR e
> evalWR e@(Mul l r) = evalBin evalWR l r (*) $ actWR e
> evalWR e@(Div l r) = evalBin evalWR l r div $ actWR e

\end{lstlisting}
   \end{answer}

\item
   La función principal de evaluación que aprovecha \texttt{evalWR}
   y las funciones monádicas de \texttt{Control.Monad.Writer}
   para efectuar la evaluación y mostrar el registro de operaciones
   intermedias

  \begin{lstlisting}[linerange={2-2}]

> evalwithlog :: Exp -> IO ()

  \end{lstlisting}

   \begin{answer}
     La función principal recupera el resultado y la bitácora producidos al computar el valor del árbol de expresión suministrado y los muestra en el formato deseado; en particular, los pares de valor y subárbol producidos en la bitácora son transformados en cadenas de caracteres por la función auxiliar \texttt{logline}:

\begin{lstlisting}[linerange={2-7}]

> evalwithlog e = do
>   putStrLn ("Result: " ++ show result)
>   DF.mapM_ logline log
>   where (result, log) = runWriter $ evalWR e
>         logline (e', v) =
>           putStrLn $ "Exp: " ++ show e' ++ " -> " ++ show v

\end{lstlisting}
   \end{answer}
\end{itemize}

Si la expresión a evaluar contiene una división por cero, o intenta
utilizar un símbolo que no está definido en la tabla de símbolos, es
suficiente que emita un mensaje de error usando \texttt{error}.

\begin{verbatim}
ghci> evalwithlog ex6
Result: *** Exception: divide by zero
ghci> evalwithlog (Var "meh")
Result: *** Exception: Var meh not found
\end{verbatim}

\section*{Evaluador con recuperación de errores (4 puntos)}

Para completar esta parte de la Tarea es necesario que estudie el tipo de
datos \texttt{Data.Either}, sabiendo que es una instancia del
Monad \texttt{Error} descrito en \texttt{Control.Monad.Error}.

Se desea implantar un evaluador de expresiones que sea capaz de
indicar si el resultado de la operación ha sido exitoso, o bien si
ha ocurrido algún error. En este sentido, el evaluador debe estar
preparado para reaccionar antes tres errores

\begin{lstlisting}[linerange={2-5}]

> data ExpError = DivisionPorCero
>               | NumeroDeMalaSuerte
>               | VariableNoExiste String
>               deriving (Show)

\end{lstlisting}

Los errores \texttt{DivisionPorCero} y \texttt{VariableNoExiste} permiten
reportar los problemas que ya hemos enfrentado en los evaluadores
anteriores, mientras que el error \texttt{NumeroDeMalaSuerte} ocurrirá
\emph{siempre} que durante el cómputo aparezca el número 13. Por lo tanto,
la evaluación debe reportar el error \texttt{NumeroDeMalaSuerte} si se encuentra
alguna variable con el valor 13, si aparece la constante manifiesta 13
o se obtiene 13 como resultado de algún cómputo intermedio.

Si una expresión contiene múltiples errores, el evaluador sólo debe
reportar el \emph{primer} error encontrado en la travesía \emph{inorder}
implícita de evaluación.

El evaluador debe estar encapsulado en una función
tal que pueda evaluarse en GHCi directamente, i.e.

\begin{verbatim}
ghci> evalwithcare ex4
Result: 252
ghci> evalwithcare (Const 13)
Rayos: NumeroDeMalaSuerte
ghci> evalwithcare (Div (Const 42) (Sub (Const 4) (Const 4))
Rayos: DivisionPorCero
ghci> evalwithcare (Var "meh")
Rayos: VariableNoExiste "meh"
\end{verbatim}

Para conseguirlo, es necesario que Ud. provea:

\begin{itemize}
\item
   Una instancia de \texttt{ExpError} en la clase \texttt{Error}.

   \begin{answer}
     La clase \texttt{Error} define funciones pensadas para la acción monádica genérica \texttt{fail :: String -> m a} que produce un error a partir de un \texttt{String} según el significado que tenga producir un error en cada \textit{monad} particular \texttt{m}.  Sin embargo, se desea producir errores del tipo \texttt{ExpError}, y no hay una correspondencia clara entre los valores de este tipo y la idea de un error sin mensaje y un error con mensaje.

     Independientemente de esto, es de interés hacer que \texttt{ExpError} sea una instancia de la clase \texttt{Error} porque (el enunciado de la tarea lo pide y) esto permite usar la instancia \texttt{Error e => Monad Either e} con el tipo \texttt{ExpError} para el manejo de errores en la evaluación de árboles de expresión.  Por lo tanto, se define una instancia cuyas funciones no servirán de mucho:

\begin{lstlisting}[linerange={2-3}]

> instance Error ExpError where
>   noMsg = undefined

\end{lstlisting}
   \end{answer}

\item
   Un combinador monádico aprovechando \texttt{Data.Either}
   que complete la evaluación de las expresiones reportando errores
   si los encontrara
   \begin{lstlisting}
  evalEX :: {- Firma para aprovechar Data.Either -}
   \end{lstlisting}

   \begin{answer}
     Conviene declarar un \textit{alias} para el tipo de \textit{monad} usado para \texttt{evalEX}; \texttt{EvalErrorHandler} es un cómputo en el \textit{monad} \texttt{Either ExpError} que produce un entero y puede fallar con un valor del tipo \texttt{ExpError}:

\begin{lstlisting}[linerange={2-2}]

> type EvalErrorHandler = Either ExpError Int

\end{lstlisting}

     Se define una acción monádica que decide si un entero dado es desagradable y lo reporta con la acción monádica \texttt{throwError}:

\begin{lstlisting}[linerange={2-5}]

> triskaidekaphobia :: Int -> EvalErrorHandler
> triskaidekaphobia i =
>   if i == 13 then throwError NumeroDeMalaSuerte
>              else return i

\end{lstlisting}

     La definición de \texttt{evalEX} es similar a las de \texttt{evalST} y \texttt{evalWR}, pero las acciones monádicas que usa detectan condiciones de error:

\begin{lstlisting}[linerange={2-18}]

> evalEX :: Exp -> EvalErrorHandler
> evalEX (Const i) = triskaidekaphobia i
>
> evalEX e@(Var n) = maybe miss hit try where
>   miss = throwError $ VariableNoExiste n
>   hit = triskaidekaphobia
>   try = DM.lookup n env
>
> evalEX (Add l r) = evalBin evalEX l r (+) triskaidekaphobia
> evalEX (Sub l r) = evalBin evalEX l r (-) triskaidekaphobia
> evalEX (Mul l r) = evalBin evalEX l r (*) triskaidekaphobia
>
> evalEX e@(Div l r) = do
>   el <- evalEX l
>   er <- evalEX r
>   if er == 0 then throwError DivisionPorCero
>              else triskaidekaphobia $ el `div` er

\end{lstlisting}

     No se usa \texttt{evalBin} para el caso de la división porque se requiere verificar por separado el valor del segundo operando, y el diseño de ese combinador monádico contempla solo acciones finales que trabajan únicamente con el resultado del operador, pero no con sus operandos por separado.
   \end{answer}

\item
   La función principal de evaluación que aprovecha \texttt{evalEX}
   y las funciones monádicas de \texttt{Control.Monad.Writer}
   para efectuar la evaluación y mostrar el resultado exitoso o
   con el primero error encontrado

  \begin{lstlisting}[linerange={2-2}]

> evalwithcare :: Exp -> IO ()

  \end{lstlisting}

   \begin{answer}
     La función principal simplemente muestra el resultado del cómputo en el formato deseado:

  \begin{lstlisting}[linerange={2-4}]

> evalwithcare = putStrLn . either miss hit . evalEX where
>   miss = ("Rayos: " ++) . show
>   hit = ("Result: " ++) . show

\end{lstlisting}
   \end{answer}
\end{itemize}

\section*{Consideraciones}

Note que \emph{todos} los tipos de datos participantes en el flujo de
cómputo son instancias de Monad por lo tanto, todos los combinadores deben
ser absolutamente monádicos. En otras palabras, está \textbf{mal} hacer
\emph{pattern-matching} para determinar si algo es \texttt{Just} o
\texttt{Nothing}, o bien \texttt{Left} o \texttt{Right}.

Las librerías \texttt{Data.Either} y \texttt{Data.Maybe} tienen
combinadores monádicos -- en sus respectivos Monad, obviamente -- que les
permiten evitar la escritura de \emph{pattern matching}.

\end{document}
