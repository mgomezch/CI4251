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
   frame=single,
   framerule=1pt,
   gobble=2,
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

> import Data.Sequence (Seq)
> import Data.Foldable (Foldable, foldr)
> import Prelude hiding (foldr)

\end{comment}

\title{CI4251 - Programación Funcional Avanzada\\
Solución de la tarea 1}

\author{Manuel Gómez\\
05-38235\\
\href{mailto:targen@gmail.com}{<targen@gmail.com>}}

\date{\today}

\maketitle

\pagebreak

\section{Fold abstracto}

\begin{itemize}
\item
  \textbf{(1 punto)} -- considere la función \texttt{dropWhile}
  provista en el Preludio y en \texttt{Data.List}. Provea una
  implantación de \texttt{dropWhile} empleando el \texttt{fold}
  más apropiado según el caso.

  \begin{answer}
    Una definición satisfactoria de \texttt{dropWhile} debería ser
    igual de estricta que la versión de \texttt{dropWhile} provista en
    las librerías de Haskell.  Por esta razón se decidió usar
    \texttt{foldr} para la solución.

    La primera aproximación a la solución fue usar \texttt{foldr} de la
    manera ingenua:

\begin{lstlisting}
  dropWhile p l = foldr  [] l where
    next x acc = if p x then x else x:acc
\end{lstlisting}

    Esa definición es inválida para \texttt{dropWhile} y, de hecho, es
    equivalente a \texttt{filter . (not .)}.  La diferencia entre el
    comportamiento de las dos funciones es que \texttt{dropWhile} solo
    elimina de la lista a los elementos contiguos en el inicio de la
    lista que cumplan el predicado, mientras que esta definición elimina
    todos los elementos de cualquier parte de la lista si cumplen el
    predicado.

    Es imposible hacer que \texttt{foldr} interrumpa su recorrido por la
    lista y pueda a la vez generarla directamente y completa si los
    valores que produce son simplemente las listas parciales sin más
    información y si su función de combinación es un combinador.  Esto
    llevó a la siguiente aproximación a la solución: enriquecer los
    valores producidos por \texttt{fold} con información adicional para
    poder cambiar entre el proceso de eliminación de elementos
    indeseados del inicio de la lista y preservación de los elementos
    deseados para el resultado.

\begin{lstlisting}
  dropWhile p l = fst $ foldr next ([], []) l where
    next x (f, s) = if p x then (f, x:s) else (x:s, x:s)
\end{lstlisting}

    Los valores producidos por \texttt{foldr} en esta definición son
    tuplas con dos listas del mismo tipo de la lista que se procesa.  El
    primer elemento de cada tupla se usa para construir acumulativamente
    el resultado de la función, y por eso el \texttt{foldr} es seguido
    de \texttt{fst} para recuperar ese resultado.  El segundo elemento
    de cada tupla se usa para construir la lista original desde su
    final.  Aunque esta solución funciona para listas finitas, esto
    último, claro, la hace estricta en la lista, así que no es una
    solución aceptable.

    El problema de escribir esta función sin recursión explícita y en
    términos de \texttt{fold} fue considerado en detalle por Bernie Pope
    en su artículo \textit{Getting a Fix from the Right Fold} publicado
    en \href{http://www.haskell.org/wikiupload/1/14/TMR-Issue6.pdf}{The
    Monad.Reader Issue 6}.  Las primeras dos aproximaciones a la
    solución presentadas por Pope son precisamente las dos recién
    presentadas \textit{[y sus explicaciones son perturbantemente
    similares a las que escribí acá, aunque las escribí antes de
    encontrar ese artículo]} y se presentan dos soluciones adicionales
    basadas en usar \texttt{fold} para construir una función mediante
    composición que haga lo que se requiere (y termina definiendo
    \texttt{fix} en términos de \texttt{foldr}, por lo que la última
    solución propuesta tiene la forma usual de una transformación de una
    función explícitamente recursiva a una apropiada para ser pasada a
    \texttt{fix} para recuperar la recursión).

    Gran parte de la dificultad de definir \texttt{dropWhile} con
    \texttt{foldr} proviene de intentar escribir la función de
    combinación suministrada a \texttt{foldr} como un combinador; sin
    embargo, si la función de combinación tiene acceso directo a la
    lista original, una solución mucho más sencilla emerge:

\begin{lstlisting}[linerange={2-4}]

> dropWhile :: (a -> Bool) -> [a] -> [a]
> dropWhile p l = foldr next l l where
>   next x acc = if p x then tail acc else l

\end{lstlisting}

    Como esta solución usa \texttt{foldr} y su función de combinación
    no es estricta en la lista, funciona con listas infinitas sin ningún
    problema.
  \end{answer}

\item
  \textbf{(2 puntos)} -- Provea una implantación de \texttt{foldl}
  usando \texttt{foldr}.

\begin{lstlisting}
  foldl :: (a -> b -> a) -> a -> [b] -> a
  foldl = {- Algo con foldr -}
\end{lstlisting}

  Incluya un diagrama que ilustre el funcionamiento de su implantación.

  \begin{answer}
    El funcionamiento normal de \texttt{foldl} es evaluar su función de combinación sobre el valor suministrado y el primer elemento de la lista para obtener el primer valor generado, que luego se usa para evaluar la función de combinación con el segundo elemento de la lista, y así sucesivamente hasta acabar la lista.  La técnica para expresar a \texttt{foldl} como \texttt{foldr} consiste en reconocer que ese proceso es una transformación sucesiva del valor suministrado a \texttt{foldl} con funciones que toman ese valor y producen el siguiente, así que podría construirse primero la función que realiza esa transformación mediante composición, y finalmente aplicarse la función resultante sobre el valor suministrado a \texttt{foldl}.

\begin{lstlisting}[linerange={2-3}]

> foldl :: (a -> b -> a) -> a -> [b] -> a
> foldl = \ f z l -> (foldr (flip (.)) id $ map (flip f) l) z

\end{lstlisting}
  \end{answer}

\end{itemize}

\section{Foldable y Functor}

Considere el tipo de datos

\begin{lstlisting}[linerange={2-6}]

> data Dream b a = Dream a
>                | Limbo (b,a) 
>                | Within a (Seq (Dream b a))
>                | Nightmare b
>                deriving (Show)

\end{lstlisting}

\begin{itemize}
\item
  \textbf{(1 puntos)} -- Construya la instancia \texttt{Functor}
  para el tipo \texttt{Dream b}.

  \begin{answer}
    Para hacer que \texttt{Dream b} sea una instancia de la clase
    \texttt{Functor}...

\begin{lstlisting}[linerange={2-2}]

> instance Functor (Dream b) where

\end{lstlisting}

    ...hay que definir \texttt{fmap :: (a -> c) -> Dream b a -> Dream b
    c}...

\begin{lstlisting}[linerange={2-2}]

>   fmap f d =

\end{lstlisting}

    ...de forma tal que se mantenga la estructura del \texttt{Dream}
    suministrado pero se transformen todos los datos del tipo \texttt{a}
    que tenga almacenados usando la función pasada a \texttt{fmap}.  Por
    lo tanto, hay que considerar cada constructor del tipo:

\begin{lstlisting}[linerange={2-2}]

>     case d of

\end{lstlisting}

    \pagebreak

    El primer constructor, \texttt{Dream}, toma un dato del tipo
    \texttt{a} que debemos transformar con \texttt{f}:

\begin{lstlisting}[linerange={2-2}]

>       Dream a      -> Dream $ f a

\end{lstlisting}

    El constructor \texttt{Limbo} es casi igual de sencillo, pero tiene
    otro constructor atravesado (el del par) y un dato que hay que
    preservar:

\begin{lstlisting}[linerange={2-2}]

>       Limbo (b, a) -> Limbo (b, f a)

\end{lstlisting}

    El constructor \texttt{Within} es el más complicado: tiene un dato
    que debe transformarse con la función dada, y además es recursivo:
    incluye a una estructura de datos que contiene valores del propio
    tipo \texttt{Dream b a}.  Para él habrá que hacer una llamada
    recursiva al \texttt{fmap} que se está definiendo.  Afortunadamente,
    la estructura de datos \texttt{Seq}, que contiene a los valores que
    se deben transformar con \texttt{f}, define una instancia de la
    clase \texttt{Functor}, así que es sencillo aplicar una función a
    cada uno de los valores que almacena: se usará el \texttt{fmap} de
    \texttt{Seq} para aplicar el \texttt{fmap} de \texttt{Dream b a} a
    cada valor que el \texttt{Seq} almacena.  O en términos más
    comprensibles, \textit{I heard you like \texttt{fmap}}...

\begin{lstlisting}[linerange={2-2}]

>       Within a ds  -> Within (f a) $ fmap (fmap f) ds

\end{lstlisting}

    El último constructor es el más sencillo de todos: como no involucra
    ni directa ni indirectamente a valores del tipo \texttt{a}, solo hay
    que preservar el valor:

\begin{lstlisting}[linerange={2-2}]

>       Nightmare b  -> Nightmare b

\end{lstlisting}
  \end{answer}

\item
  \textbf{(2 puntos)} -- Construya la instancia \texttt{Foldable}
  para el tipo \texttt{Dream b}.

  \begin{answer}
    Para hacer que \texttt{Dream b} sea una instancia de la clase
    \texttt{Foldable}...

\begin{lstlisting}[linerange={2-2}]

> instance Foldable (Dream b) where

\end{lstlisting}

    ...hay que definir \texttt{foldr :: (a -> c -> c) -> c -> Dream b a
    -> c}...

\begin{lstlisting}[linerange={2-2}]

>   foldr f z d =

\end{lstlisting}

    ...de forma tal que se haga colapsar la estructura del
    \texttt{Dream} suministrado haciendo un cómputo acumulativo con
    \texttt{f} sobre los valores \texttt{a} que estén en la estructura.
    Por lo tanto, hay que considerar cada constructor del tipo:

\begin{lstlisting}[linerange={2-2}]

>     case d of

\end{lstlisting}

    Los constructores \texttt{Dream} y \texttt{Limbo} toman un solo dato
    del tipo \texttt{a}; como se desea colapsar esos constructores a
    valores del tipo \texttt{c} aplicando la función \texttt{f}, se hace
    precisamente eso:

\begin{lstlisting}[linerange={2-3}]

>       Dream a      -> f a z
>       Limbo (b, a) -> f a z

\end{lstlisting}

    El constructor recursivo \texttt{Within}, claro, es el más
    complicado: los \texttt{Dream b a} están contenidos en un
    \texttt{Seq}, así que hay que colapsar el \texttt{Seq} a un solo
    valor que se obtiene de colapsar cada \texttt{Dream b a} a un valor
    que se usa como inicio en el colapso del siguiente.  Afortunadamente
    \texttt{Seq} define una instancia de \texttt{Foldable}, \textit{and
    I heard you like \texttt{foldr}}...

\begin{lstlisting}[linerange={2-2}]

>       Within a ds  -> f a $ foldr (flip $ foldr f) z ds

\end{lstlisting}

    El último constructor es el más sencillo de todos: como no involucra
    ni directa ni indirectamente a valores del tipo \texttt{a}, solo hay
    que colapsarlo al valor constante suministrado:

\begin{lstlisting}[linerange={2-2}]

>       Nightmare b  -> z

\end{lstlisting}
  \end{answer}
\end{itemize}

\section{Monoid}

Considere el tipo de datos \texttt{(Data.Map k v)} comentado en clase,
que tiene algún tipo de datos \texttt{k} como clave y sobre el cual
queremos permitir \emph{múltiples valores} asociados a una clave.

Proponga un tipo de datos concreto apoyado en \texttt{Data.Map}
que permita esa funcionalidad, y entonces:

\begin{itemize}
\item
  \textbf{(3 puntos)} -- Construya la instancia \texttt{Monoid}
  para este tipo de datos. En la instancia queremos que al
  combinar dos \texttt{Map}, si hay claves repetidas, se
  \emph{unan} los valores asociados.

\item
  \textbf{(1 punto)} -- Escriba un ejemplo de uso con al menos
  \emph{tres} tablas involucradas, que contengan claves \emph{repetidas}
  cuyos valores deban combinarse para ejercitar el \texttt{Monoid}
  a la medida.

\end{itemize}
\end{document}
