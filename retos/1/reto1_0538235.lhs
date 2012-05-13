Esta es la solución de Manuel Gómez al primer reto de `CI4251`.  Como ya fueron enviadas soluciones a este reto, esta solución solo pretende explorar formas alternativas de expresar la solución.  En particular, todas las funciones utilizadas se definen en el estilo *point-free* y no se utiliza *pattern matching* ni *lambdas* explícitos en absoluto.



El reto consiste en expresar BFS usando folds y unfolds:

> import Data.List (unfoldr)



El estilo point-free se beneficia del uso de combinadores para componer las funciones que serán necesarias.  El módulo `Control.Monad.Instances` define una instancia de `Monad` para el (o los) tipo(s) `forall r. (->) r`; esto permitirá usar combinadores de `Monad`s para combinar funciones.

> import Control.Monad.Instances

En particular, resultarán útiles los combinadores `join` y `liftM2` definidos en `Control.Monad`:

> import Control.Monad (join, liftM2, ap, guard)

`guard` se usará para otro fin: participa en la definición de un combinador condicional.

Cada uno de estos combinadores tiene su tipo general, que podría además expresarse en forma específica en el caso de la instancia particular de `Monad` que resulta de interés:

---------------- -------------------------------------------------------
Combinador       `join`

Tipo general     `Monad m => m (m a) -> m a`

Tipo específico  `(t -> t -> a) -> t -> a`

Uso              Aplicar un mismo argumento dos veces a una función
                 currificada.
------------------------------------------------------------------------

---------------- -------------------------------------------------------
Combinador       `liftM2`

Tipo general     `Monad m => (a -> b -> c) -> m a -> m b -> m c`

Tipo específico  `(a -> b -> c) -> (t -> a) -> (t -> b) -> t -> c`

Uso              Precomponer dos funciones que toman un mismo tipo en
                 cada argumento de una función binaria currificada.
------------------------------------------------------------------------

No sería particularmente complejo definir estos combinadores en vez de importarlos (y así apegarse un poco más estrictamente a las condiciones del reto de solo importar `Data.List.unfoldr`), pero no se hizo por comodidad y para explorar las instancias más interesantes de `Monad`.  Podría definirse `join f x = f x x` y `liftM2 f g h x = f (g x) (h x)` (que solo serían válidas para la instancia `Monad ((->) r)`, claro); las expresiones *point-free* serían menos agradables.



Otros combinadores resultarán útiles:

*   `|>` es la composición de funciones en orden inverso al del operador `(.)` de Haskell.  Permite escribir funciones por composición donde el flujo de datos es de izquierda a derecha.

> infixr 1 |>
> (|>) :: (a -> b) -> (b -> c) -> (a -> c)
> (|>) = flip (.)


*   `flip'` y `flip''` cambia el orden de los argumentos de funciones currificadas de aridad superior a dos:

> flip' :: (a -> c -> b -> d) -> a -> b -> c -> d
> flip' = (|> flip)
>
> flip'' :: (b -> c -> a -> d) -> a -> b -> c -> d
> flip'' = flip' flip'


*   `.*` y `.**` precomponen funciones sobre los segundos y terceros argumentos de funciones currificadas:

> infixr 1 .*
> (.*) :: (a -> b -> c) -> (t -> b) -> a -> t -> c
> (.*) = flip |> (.) |> flip'
>
> infixr 1 .**
> (.**) :: (a -> b -> c -> d) -> (t -> c) -> a -> b -> t -> d
> (.**) = flip' (|> (.*))


*   El combinador condicional `<?>`, muchas veces llamado `if'`, que cumple la misma función que la sintaxis especial `if`-`then`-`else` de Haskell, pero como función:

> infixr 1 <?>
> (<?>) :: Bool -> a -> a -> a
> (<?>) = guard |> (flip'' $ const |> flip maybe)



`unfoldr` controla su comportamiento usando valores del tipo Maybe, y resulta conveniente definir una función que los produzca a partir de booleanos:

> toMaybe :: Bool -> a -> Maybe a
> toMaybe = flip flip'' Nothing $ (<?>) .* Just



La función de BFS en forma *point-free* fue hecha con mucho cuidado.  No es mucho más inteligente que la versión con un solo `unfold` y el código es bastante más complejo y menos legible.  La mayor parte de la complejidad de la función proviene de la manipulación de estructuras de datos usando el estilo *point-free*, que evidentemente no es muy adecuado para expresar este problema.

Primero, se define una función que recibe una lista de listas y retorna la misma lista original sin los elementos que sean listas vacías; una buena definición de esta función podría ser `filter $ not . null`; sin embargo, como el reto favorece el uso de *folds*, se usará una definición basada en `foldr`:

> dropNulls :: [[a]] -> [[a]]
> dropNulls = foldr (liftM2 (flip liftM2 id) (null |> (<?>)) (:)) []

La definición de `bfs` construirá los nodos de la búsqueda usando una lista cuyos elementos serán las listas de sucesores de cada nodo.  La concatenación de listas es necesaria para que los nodos se visiten en el órden correcto para BFS: deben añadirse al final de la cola de sucesores, no al principio, así que el uso ingenuo de `(:)` no es una opción.  Lo ideal, claro, sería usar `Data.Sequence.Seq`.

> bfs :: (a -> [a]) -> a -> [a]
> bfs =
>         ((head . head) |>)                                        |>
>         (|> return)                                               |>
>         liftM2 (flip $ foldr (:)) (liftM2 (:) (tail . head) tail) |>
>         liftM2 (,)                (head . head)                   |>
>         liftM2 toMaybe            (not . null)                    |>
>         (dropNulls |>)                                            |>
>         unfoldr                                                   |>
>         ((return . return) |>)

La función `flip $ foldr (:)` hace lo mismo que `(++)`, claro, pero es explícitamente un *fold* porque el reto lo favorece.  Ambas funciones con *folds* explícitos usan `foldr` para poder usarse con listas perezosas infinitas.
