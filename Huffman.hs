{-|
 -  Este modulo define un Arbol de Huffman
 -  Autores:  German Leon 08-10611
 -            Ricardo Lunar 08-10655
 -}
module Huffman (
    -- * Tipo @Huffman@
    Huffman,
    -- ** Operaciones sobre @Huffman@
    obtenerFrecuencia,
    crearRama,
    crearHuffman,
    generarAsociaciones,
    reconstruirHuffman,
    codificar,
    decodificar
)
where

import BinomialHeap as BHeap

{- |
  Definicion del tipo Arbol de Huffman 
 -}
data Huffman a = Hoja Integer a | Rama Integer (Huffman a) (Huffman a) deriving (Show, Read)

{- |
  Instancia de la operacion de equivalencia en Arboles de Huffman 
 -}
instance (Eq a) => Eq (Huffman a) where
    (Hoja f e)      == (Hoja f2 e2)         = (f == f2) && (e == e2)
    (Rama frec i d) == (Rama frec2 i2 d2)   = (frec == frec2) && (i == i2) && (d == d2)
    _ == _ = False 

{- |
  Instancia de la operacion de comparacion entre Arboles de Huffman 
 -}
instance (Ord a) => Ord (Huffman a) where
    Hoja f e   <= Hoja f2 e2    = f <= f2
    Rama f i d <= Hoja f2 e     = f <= f2
    Hoja f e   <= Rama f2 i d   = f <= f2
    Rama f i d <= Rama f2 i2 d2 = f <= f2

{- |
   Obtiene la frecuencia acumulada que se encuentra en la raiz del Arbol
   de Huffman suministrado
 -}
obtenerFrecuencia :: Huffman a -> Integer
obtenerFrecuencia (Hoja f _) = f
obtenerFrecuencia (Rama f _ _) = f

{- |
   Dados dos Arboles de Huffman, crea un nuevo arbol que los tiene
   como hijos. La frecuencia acumulada del nuevo arbol es la suma de la de
   los nuevos hijos
 -}
crearRama :: Huffman a -> Huffman a -> Huffman a
crearRama i d = Rama (obtenerFrecuencia i + obtenerFrecuencia d) i d

{- |
   Dada una lista de elementos con sus correspondientes frecuencias construye un 
   BinomialHeap de Arboles de Huffman que contengan dichos elementos y sus frecuencias
-}
crearConjunto :: (Ord a) => [(a,Integer)] -> BinomialHeap (Huffman a)
crearConjunto = foldl agregar (BH []) 
    where
        agregar acc (elem,frec) = BHeap.insert (Hoja frec elem) acc

{- |
   Dada una lista que contiene un conjunto de simbolos y su cantidad de 
   ocurrencias, construye un Arbol de Huffman que la represente
 -}
crearHuffman :: (Ord a) => [(a, Integer)] -> Huffman a
crearHuffman l = crearHuffmanAux (crearConjunto l) 

{- |
   Dado un conjunto de Arboles de Huffman representado con un BinomialHeap
   devuelve un unico Arbol de Huffman con los valores de todos los arboles
   iniciales
 -}
crearHuffmanAux :: (Ord a) => BinomialHeap (Huffman a) -> Huffman a
crearHuffmanAux (BH [(Nodo 0 t _)]) = t
crearHuffmanAux c = crearHuffmanAux (BHeap.insert ramaNueva heapSinAB)
    where
        ramaA       = findMin c
        ramaB       = findMin (deleteMin c)
        ramaNueva   = crearRama ramaA ramaB
        heapSinAB   = deleteMin (deleteMin c)

{- |
   Dado un Arbol de Huffman, devuelve una lista de tuplas con el conjunto
   de todos los simbolos ocurrentes en el arbol, junto a la codificacion
   binaria de los mismos (La lista de booleanos representa la lista de bits
   de la codificacion donde False representa el cero y True el uno)
 -}
generarAsociaciones :: (Ord a) => Huffman a -> [(a, [Bool])]
generarAsociaciones a = generarAsociacionesAux a [] []

generarAsociacionesAux :: (Ord a) => Huffman a -> [(a, [Bool])] -> [Bool] -> [(a, [Bool])]
generarAsociacionesAux (Hoja _ elem) asoc acc = (elem, acc ++ [True]):asoc
generarAsociacionesAux (Rama _ i d)  asoc acc = izquierdos ++ derechos
    where 
        izquierdos =  generarAsociacionesAux i asoc (acc ++ [False]) 
        derechos   =  generarAsociacionesAux d asoc (acc ++ [True])

{- |
   Dada una lista de tuplas con un conjunto de simbolos, junto a una
   codificacion binaria de los mismos (La lista de booleanos representa 
   la lista de bits de la codificacion, donde False representa el cero
   y True el uno). Construye un Arbol de Huffman tal que dichas asociaciones
   pudieran haber sido generadas. Para conservar las propiedades de un buen
   Arbol de Huffman, la frecuencia acumulada sera asignada a cero(0) para 
   todas las hojas y ramas del mismo
 -}
reconstruirHuffman :: [(a, [Bool])] -> Huffman a
reconstruirHuffman ((e,[True]):[]) = Hoja 0 e
reconstruirHuffman l = Rama 0 (reconstruirHuffman izq) (reconstruirHuffman der)
    where
        izq = [(e,xs)| (e,(False:xs)) <- l]
        der = [(e,xs)| (e,(True:xs))  <- l]

{- |
   Dada una lista de simbolos devuelve una lista de tuplas donde el primer
   valor de cada tupla  es un simbolo de la lista original y el segundo valor 
   es el numero de ocurrencias de ese simbolo en la lista original
 -}
conseguirOcurrencias :: (Eq a) => [a] -> [(a,Integer)] -> [(a, Integer)]
conseguirOcurrencias [] acc = acc
conseguirOcurrencias (x:xs) acc = conseguirOcurrencias resto ((x, 1 + repet):acc)
    where 
        resto = [e| e <- xs, e /= x]
        repet = sum [1| a <- xs, a == x]

{- |
 - Dado una lista de simbolos, devuelve una tupla. Esta tupla tiene como 
   primer elemento el Arbol de Huffman generado a partir de la lista de
   simbolos dada. Como segundo elemento, tiene una lista donde cada uno
   de los simbolos en la entrada, ha sido reemplazado por su representacion
   binaria y concatenados para formar una sola cadena. 
 -}
codificar :: (Ord a) => [a] -> (Huffman a, [Bool])
codificar l = (arbol, concatMap buscarCodigo l)
    where 
        arbol           = crearHuffman(conseguirOcurrencias l [])
        codigos         = generarAsociaciones(arbol)
        buscarCodigo s  = snd (head (dropWhile (\(e,c) -> e/=s) codigos))

{- |
 - Dado un Arbol de Huffman y luego una lista que representa a una cadena
   de bits, devuelve una lista de simbolos que corresponde a la decodificacion
   de dicha cadena con la informacion del arbol
 -}
decodificar :: Huffman a -> [Bool] -> [a]
decodificar a [] = []
decodificar a c  = buscarSimbolo a c []
    where
        buscarSimbolo (Hoja _ e)   []         acc = acc
        buscarSimbolo (Hoja _ e)   (_:resto)  acc = acc ++ [e] ++ (decodificar a resto)
        buscarSimbolo (Rama _ _ d) (True:xs)  acc = buscarSimbolo d xs acc
        buscarSimbolo (Rama _ i _) (False:xs) acc = buscarSimbolo i xs acc
