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
\lstset{literate=%
{á}{{\'a}}1
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
}

\newenvironment{answer}{%
  \def\FrameCommand{\fboxsep=\FrameSep \fcolorbox{black}{midgrey}}%
  \color{black}\MakeFramed {\FrameRestore}}%
 {\endMakeFramed}

\begin{document}

\begin{comment}

> import qualified Data.Foldable as DF (foldr)
> import qualified Data.Map      as DM (Map, empty, fold, foldWithKey, insert, keys, lookup, update)
> import qualified Data.Sequence as DS (Seq, fromList)
>
> import Text.ParserCombinators.Parsec
>
> import Control.Applicative     ((<$>), (<*>))
> import Control.Monad           (join)
> import Control.Monad.Instances ()
> import Data.Char               (isAscii, isLetter, isPrint, isSpace)
> import Data.Monoid             (mappend)
> import System.Environment      (getArgs)

\end{comment}

\title{CI4251 - Programación Funcional Avanzada\\
Solución de la tarea 3}

\author{Manuel Gómez\\
05-38235\\
\href{mailto:targen@gmail.com}{<targen@gmail.com>}}

\date{Junio 02, 2012}

\maketitle

\pagebreak

\section*{Archivos de Configuración}

Los archivos INI son un mecanismo simple y conveniente para declarar la
configuración de aplicaciones. Son archivos de texto simple, que contienen
definiciones de parámetros de la forma \emph{clave-valor}, agrupados
en secciones. Además, y para conveniencia del usuario, es posible incluir
comentarios dentro del archivo para facilitar su comprensión.

Si bien existen muchas variantes para representar archivos INI, en esta
tarea emplearemos una versión simplificada que permita ejercitar algunas
habilidades particulares tanto para escribir el reconocedor, como para su
representación en memoria.

Así, un ejemplo ilustra el estilo de archivos INI con los cuales vamos a
trabajar.
\footnote{Usted es responsable de construir otros archivos de ejemplo,
correctos e incorrectos, para convencerse de que su programa hace lo
que debe hacer -- no dude en consultar enviando sus ejemplos a la
\emph{lista de correo} para que todos se beneficien.}
Consideremos el archivo \texttt{ejemplo.ini}

\begin{verbatim}
; Este es un comentario.
[connection]
database = Pg                ; Preservar mayúsculas!
hostname = db.example.com
   port  = 5432         ; Note el uso liberal de espacios
username = 
password = s3cr3t

[people]
stooges    = curly   larry    moe
       stooges = joe shemp
; stooges contendrá a los cinco y en ese orden
wizards =  Gandalf   "The Wizard of Oz" 'Harry Potter'
; valores entre comillas se consideran atómicos
ninjas =
; La clave ninjas está definida, pero no tiene valor


 [Empty]

     [the-last-section]
sense = this file makes none
help = is on the way
\end{verbatim}

En base a este ejemplo, definiremos la estructura de los archivos INI
considerados válidos para esta tarea.

\begin{itemize}
\item
  Un archivo INI está compuesto por \emph{una} o más secciones,
  separadas entre sí por al menos \emph{una} línea en blanco.
\item
  El símbolo \texttt{;} indica la presencia de un comentario,
  puede estar en cualquier posición del archivo y su efecto es
  ignorar el resto de la línea.
\item
  El nombre de una sección está encerrado entre corchetes -- no puede
  haber espacios entre los corchetes y el nombre.
\item
  Cada sección tiene \emph{cero} o más definiciones.
\item
  Cada definición consta de exactamente \emph{un} nombre (la
  \emph{clave}), el símbolo \texttt{=} y \emph{cero} o más
  \emph{valores}. Puede haber cualquier cantidad de espacios en
  blanco antes de la clave, entre la clave y el símbolo \texttt{=},
  y entre el símbolo \texttt{=} y el valor, de existir este último.
\item
  Los nombres de sección y las claves son cadenas que comienzan
  con una letra y continúan con letras, dígitos o el símbolo \texttt{-}.
\item
  Los valores son cadenas con cualquier símbolo imprimible.
\item
  Múltiples valores estarán separados por espacios en blanco.
\item
  Si se desea un valor que incluya espacios en blanco o el \texttt{;},
  todo el valor debe estar encerrado entre comillas dobles o simples.
  No interesa representar caracteres de escape como en los lenguajes
  de programación.
\item
  Sólo utilizaremos el conjunto ASCII entre 32 y 126.
\end{itemize}

Si Ud. cree que hay ambigüedades en la definición, o piensa que he omitido
algún caso particular, por favor pregunte en la \emph{lista de correo}
proveyendo algún ejemplo concreto de aquella construcción que le está
causando dudas. Noten que despejaré dudas hasta el
miércoles 2012-05-29 23:59 VET.

\pagebreak

\section*{Reconocedor de Archivos INI (4 puntos)}

Se desea implantar un reconocedor que sea capaz de procesar un
archivo INI y determinar si su estructura sintáctica es correcta o no.
Usted debe construir el reconocedor aprovechando al máximo los
combinadores provistos por \texttt{Parsec} de manera que esté
encapsulado en una función tal que pueda evaluarse en GHCi
directamente, i.e.

\begin{lstlisting}
  parseINI :: String -> Either ParseError AppConfig
\end{lstlisting}

No es obligatorio que su reconocedor sea determinístico --
de hecho, para algunas partes le resultará conveniente
explotar el \emph{backtracking}.

Puede usar las versiones 2 o 3 de \texttt{Parsec} --
¡no las mezcle!

\begin{answer}

  Por falta de tiempo, y porque la firma dada para \texttt{parseINI} sugería hacerlo directamente, no se implementó por separado un reconocedor que únicamente verifique el formato sin producir los valores que representan la información dada.  El reconocedor completo está en las respuestas siguientes.

\end{answer}

\section*{Representación en Memoria (4 puntos)}

Complete las definiciones de tipos de datos
\begin{lstlisting}
  type Name      = String
  type Key       = String
  data Section   = 
  data Value     = 
  data AppConfig = 
\end{lstlisting}
seguramente aprovechando \texttt{Data.Map} y \texttt{Data.Seq}, de modo
que permitan representar en memoria la configuración tomada de un archivo INI.
La representación debe ser tal que facilite el acceso a claves particulares
dentro de una sección para establecer si están definidas, y de estarlo
acceder a sus valores asociados.

La interfaz de programación mínima debe contener las funciones

\begin{lstlisting}
  empty          :: AppConfig
  addSection     :: AppConfig -> Name -> AppConfig
  getSection     :: AppConfig -> Name -> Maybe Section
  getSectionKeys :: AppConfig -> Name -> Maybe [Key]
  addValue       :: AppConfig -> Name -> Key -> Value -> AppConfig
  getValue       :: AppConfig -> Name -> Key -> Maybe Value
\end{lstlisting}

\begin{itemize}
  \item
    Aplicar \texttt{addSection} para agregar una sección que ya
    existe, no tiene ningún efecto.
  \item
    Aplicar \texttt{addValue} para agregar valores a una clave
    existente debe \emph{acumular} los nuevos valores preservando
    el orden.
\end{itemize} 

Modifique el reconocedor construido previamente para que produzca como
resultado un valor del tipo \texttt{AppConfig} que contenga la
representación adecuada para el archivo INI procesado.

\begin{answer}

  Aunque el uso de \texttt{data} y constructores nuevos atravesados en la
  representación de los datos resulta algo incómoda para escribir algunas
  de las funciones pedidas, se preservó esa forma y se aprovechó para
  definir instancias de \texttt{Show} que facilitan otras partes del
  trabajo.

\begin{lstlisting}[linerange={2-13}]

> type Name      = String
> type Key       = String
> data Section   = Section {
>                    sname :: Name,
>                    keys :: DM.Map Key Value
>                  }
> data Value     = Value {
>                    vals :: DS.Seq ValueQ
>                  }
> data AppConfig = AppConfig {
>                    sections :: DM.Map Name Section
>                  }

\end{lstlisting}

  Se usó el tipo adicional \texttt{ValueQ} para diferenciar entre los
  tipos de valor encontrados los archivos INI: sin comillas, y con cada
  tipo de comillas.  La función \texttt{mkValueQ} toma una cadena de
  caracteres y determina a qué categoría debería pertenecer, y la
  instancia \texttt{Show ValueQ} imprime las cadenas según esto:

\begin{lstlisting}[linerange={2-21}]

> data ValueQ = Unquoted String
>             | SingleQuoted String
>             | DoubleQuoted String
>
> instance Show ValueQ where
>   show (Unquoted s)     =         s
>   show (SingleQuoted s) = "'"  ++ s ++ "'"
>   show (DoubleQuoted s) = "\"" ++ s ++ "\""
>
> mkValueQ :: String -> ValueQ
> mkValueQ s = f s where
>   f = case (
>     '\"' `elem` s,
>     '\'' `elem` s,
>     ';'  `elem` s || any isSpace s
>    ) of
>     (False, False, False) -> Unquoted
>     (True , False, _    ) -> SingleQuoted
>     (True , True , _    ) -> error "Error de comillas en valor."
>     _                     -> DoubleQuoted

\end{lstlisting}

  Las instancias de \texttt{Show} de los tipos \texttt{Value},
  \texttt{Section} y \texttt{AppConfig} facilitan su visualización en el
  formato pedido en la siguiente sección de la tarea:

\begin{lstlisting}[linerange={2-19}]

> instance Show Value where
>   show v = DF.foldr (\ x acc -> show x
>                              ++ " "
>                              ++ acc
>                     ) "" $ vals v
>
> instance Show Section where
>   show s = DM.foldWithKey (\ k a acc -> sname s
>                                      ++ "."
>                                      ++ k
>                                      ++ " = "
>                                      ++ show a
>                                      ++ "\n"
>                                      ++ acc
>                           ) "" $ keys s
>
> instance Show AppConfig where
>   show ac = DM.fold (\ x acc -> show x ++ acc) "" $ sections ac

\end{lstlisting}

  Las funciones pedidas están basadas en \texttt{Data.Map},
  \texttt{Data.Sequence} y \texttt{Data.Monoid}:

\begin{lstlisting}[linerange={2-30}]

> empty :: AppConfig
> empty = AppConfig $ DM.empty
>
> addSection :: AppConfig -> Name -> AppConfig
> addSection ac n = ac { sections = maybe insert noop lookup }
>   where noop   = const oldS
>         oldS   = sections ac
>         emptyS = Section n DM.empty
>         lookup = DM.lookup n oldS
>         insert = DM.insert n emptyS oldS
>
> getSection :: AppConfig -> Name -> Maybe Section
> getSection ac n = DM.lookup n $ sections ac
>
> getSectionKeys :: AppConfig -> Name -> Maybe [Key]
> getSectionKeys ac n = DM.keys . keys <$> getSection ac n
>
> addValue :: AppConfig -> Name -> Key -> Value -> AppConfig
> addValue ac n k v = ac { sections = DM.update uS n aS }
>   where
>     aS     = sections $ addSection ac n
>     uS s   = Just $ s { keys = maybe (iV s) (cV s) $ lV s }
>     lV s   = DM.lookup k    $ keys s
>     iV s   = DM.insert k  v $ keys s
>     cV s _ = DM.update uV k $ keys s
>     uV val = Just $ val { vals = vals val `mappend` vals v }
>
> getValue :: AppConfig -> Name -> Key -> Maybe Value
> getValue a n k = DM.lookup k . keys =<< DM.lookup n (sections a)

\end{lstlisting}

  La función principal del reconocimiento y análisis de archivos INI
  (pedida para la sección anterior de la tarea en forma simplificada,
  pero solo dada en su forma extendida por razones de tiempo):

\begin{lstlisting}[linerange={2-3}]

> parseINI :: String -> Either ParseError AppConfig
> parseINI input = runParser ini empty "INI parser input" input

\end{lstlisting}

  Se utiliza el \textit{parser} \texttt{ini} construido con
  \textit{Parsec} para procesar el archivo de entrada.  \textit{Parsec}
  está basado en un \textit{monad} de transformaciones de estados y es
  posible proveer un estado propio para que sea procesado durante el
  reconocimiento; este mecanismo se aprovecha para producir la
  representación abstracta en memoria de los datos del archivo
  procesado:

\begin{lstlisting}[linerange={2-2}]

> type INIParser a = GenParser Char AppConfig a

\end{lstlisting}

  El reconocedor \texttt{ini} se construye combinando muchos
  reconocedores más simples, todos basados en el tipo
  \texttt{INIParser}:

\begin{lstlisting}[linerange={2-100}]

> ini :: INIParser AppConfig
> ini = do
>   skipMany $ comment <|> stfu space
>   ss <- sepEndBy section $ skipMany1 $ try $ do
>     option () comment
>     eol
>     blanks
>   option () comment >> eof
>   getState
>
> section :: INIParser ()
> section = do
>   n <- sectionHeader
>   blanks
>   option () comment
>   eol <|> eof
>   blanks
>   ks <- sepEndBy definition $ try $ skipMany1 $ do
>     option () comment
>     eol
>     blanks
>     notFollowedBy $ char '['
>   updateState $ \st->foldl (\acc x->uncurry (aV acc n) x) st ks
>  <?> "section"
>  where aV = addValue
>
> sectionHeader :: INIParser String
> sectionHeader = do
>   between (char '[') (char ']') $ name "section"
>  <?> "section header"
>
> definition :: INIParser (String, Value)
> definition = do
>   n <- name "key"
>   blanks
>   char '='
>   blanks
>   vs <- value `sepEndBy` blanks
>   return (n, Value $ DS.fromList $ map mkValueQ vs)
>  <?> "definition"
>
> comment :: INIParser ()
> comment = do
>   char ';'
>   stfu $ many $ notFollowedBy (eol <|> eof) >> anyChar
>  <?> "comment"
>
> blanks :: INIParser ()
> blanks = do
>   skipMany $ notFollowedBy (eol <|> eof) >> space
>  <?> "whitespace"
>
> name :: String -> INIParser String
> name s = do
>   (:) <$> asciiLetter <*> (many $ asciiLetter <|> char '-')
>  <?> s ++ " name"
>
> value :: INIParser String
> value = do
>   quotedValue '\'' <|> quotedValue '"' <|> unquotedValue
>  <?> "value"
>
> quotedValue :: Char -> INIParser String
> quotedValue c = do
>   join between (char c) $ many $ quotedCharacter c
>  <?> c : "-quoted value"
>
> unquotedValue :: INIParser String
> unquotedValue = do
>   many1 $ satisfyAll [ isAscii
>                      , isPrint
>                      , isNotSpace
>                      , isNormal
>                      ]
>  <?> "unquoted value"
>  where isNotSpace = not . isSpace
>        isNormal   = (`notElem` ";'\"")
>
> asciiLetter :: INIParser Char
> asciiLetter = do
>   satisfyAll [isAscii, isLetter]
>  <?> "ASCII letter"
>
> quotedCharacter :: Char -> INIParser Char
> quotedCharacter c = do
>   satisfyAll [ isAscii
>              , \ x -> isPrint x || x == '\n'
>              , (/= c)
>              ]
>  <?> "quoted character"
>
> eol :: INIParser ()
> eol = do
>   stfu $     try (char '\n' >> option () (stfu $ char '\r'))
>          <|> try (char '\r' >> option () (stfu $ char '\n'))
>  <?> "end of line"
>
> satisfyAll :: [Char -> Bool] -> INIParser Char
> satisfyAll = satisfy . andPredicates

\end{lstlisting}

  Se usaron algunas funciones adicionales que no están relacionadas
  directamente con \texttt{INIParser} pero facilitan el trabajo:

\begin{lstlisting}[linerange={2-6}]

> stfu :: Monad m => m a -> m ()
> stfu = (>> return ())
>
> andPredicates :: [a -> Bool] -> a -> Bool
> andPredicates = flip $ all . flip id

\end{lstlisting}

\end{answer}


\section*{Lector de configuración (2 puntos)}

Finalmente, usted debe completar la función \texttt{main} haciendo su
programa ejecutable de manera tal que:

\begin{itemize}
  \item
    Reciba argumentos por la línea de comandos.
  \item
    El \emph{primer} argumento siempre será el nombre del
    archivo INI a procesar -- puede suponer que el archivo
    siempre existirá.
  \item
    Si la estructura del archivo es incorrecta, reportar el
    error de sintaxis de la manera más clara posible, y
    terminar la ejecución inmediatamente.
  \item
    El resto de los argumentos serán considerados ``preguntas''
    a efectuar al servicio de configuración. Para cada uno
    de los argumentos es necesario consultar la configuración
    mostrando por pantalla el valor o valores correspondientes.
\end{itemize}

Una corrida podría ser

\begin{verbatim}
$ tarea3 ejemplo.ini people.stooges people.magazine people.ninjas
people.stooges = curly larry moe joe shemp
La clave people.magazine no está definida.
people.ninjas =
$ tarea3 ejemplo.ini inexistente connection
La sección inexistente no existe.
connection.database = Pg
connection.hostname = db.example.com
connection.port = 5432
connection.username = 
connection.password = s3cr3t
\end{verbatim}

\begin{answer}

  La mayor parte del trabajo de consultar los datos obtenidos del
  archivo procesado son extraidos de la estructura de datos que los
  representa por la función pura \texttt{query}:

\begin{lstlisting}[linerange={2-20}]

> query :: AppConfig -> String -> String
> query ac q = either badQ goodQ $ runParser p empty "<cmdline>" q
>   where
>     showK s k v = s ++ "." ++ k ++ " = " ++ show v
>
>     badQ  _     = "La consulta " ++ q       ++ " es inválida."
>     noS   s     = "La sección "  ++ s       ++ " no existe."
>     noK   s k   = "La clave "    ++ dot s k ++ " no existe."
>     dot   s k   = s ++ "." ++ k
>
>     goodQ (s,k) = maybe (all s  ) (one   s  ) $ k
>     all   s     = maybe (noS s  ) (show     ) $ getSection ac s
>     one   s k   = maybe (noK s k) (showK s k) $ getValue   ac s k
>
>     p = do
>       s <- name "section"
>       k <- option Nothing (char '.' >> Just <$> name "key")
>       eof
>       return (s, k)

\end{lstlisting}

  El código principal simplemente lee los argumentos de línea de
  comando, lee el archivo indicado, y utiliza a \texttt{query} para
  hacer el trabajo real:

\begin{lstlisting}[linerange={2-5}]

> main = do
>   (file : queries) <- getArgs
>   ac <- either (error . show) id <$> parseINI <$> readFile file
>   mapM_ putStrLn $ map (query ac) queries

\end{lstlisting}

\end{answer}

\end{document}
