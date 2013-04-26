{- |
 -  Modulo principal
 -  Autores:  German Leon 08-10611, Ricardo Lunar 08-10655
 -}
module Main(
    -- * @Main@
    main
)
where

import CompresorDeArchivos
import System (getArgs)

{- |
   Dado el nombre de un archivo a leer, el nombre de otro en donde
   escribiremos y la eleccion del modo de ejecucion (codificacion o
   decodificacion) transforma los datos de entrada y consigue los
   datos de salida esperados
 -}
main :: IO ()
main = do
    args <- getArgs
    let fuente = args !! 0
        destino = args !! 1
        modo = if (args !! 2) == "codificar" then True else False
    datosLeidos <- leerDeArchivo fuente
    imprimirArchivo destino (transformar modo datosLeidos)
