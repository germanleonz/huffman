{- |
 -  Adaptacion a Haskell de la implementacion de un heap binomial
 -  escrita en pseudocodigo funcional en el libro referido a
 -  continuacion
 -
 -  Fuente:     Purely Functional Data Structures
 -              Chris Okasaki    
 -              Cambridge University Press, 1998            
 -  Modificado: German Leon 08-10611, Ricardo Lunar 08-10655
 -}
module BinomialHeap (
	-- * Clase @BinomialHeap@ 
    module BinomialHeap) 
where

data BinomialTree a = Nodo Int a [BinomialTree a] deriving (Show, Read)

data BinomialHeap a = BH [BinomialTree a] deriving (Show, Read)

-- | Dado un Arbol consigue el orden de este
orden :: BinomialTree a -> Int
orden (Nodo o e h) = o

-- | Dado un Arbol consigue la raiz de este
raiz :: BinomialTree a -> a
raiz (Nodo o e h) = e  

{- | Dados dos arboles de orden r retorna un arbol de orden r + 1
     con los mismos elementos de los arboles de orden r
-}
enlazar :: (Ord a) => BinomialTree a -> BinomialTree a -> BinomialTree a
enlazar t1@(Nodo r x1 c1) t2@(Nodo _ x2 c2)
    | x1 <= x2 = Nodo (r+1) x1 (t2:c1)
    | otherwise = Nodo (r+1) x2 (t1:c2)

{-| Dado un arbol y una lista de arboles, retorna el arbol enlazado a la
    lista en la posicion que le corresponde
-}
insTree :: (Ord a) => BinomialTree a -> [BinomialTree a] -> [BinomialTree a] 
insTree t [] = [t]
insTree t (x:xs) = if orden t < orden x then t:(x:xs) else insTree (enlazar t x) xs
 
-- | Funcion que permite insertar una instancia de Nodo en el @BinomialHeap@
insert :: (Ord a) => a -> BinomialHeap a -> BinomialHeap a
insert x (BH ts) = BH (insTree (Nodo 0 x []) ts)

-- | Funcion que permite unir dos @BinomialTree@ y devolverlos en solo uno.
mrg :: (Ord a) => [BinomialTree a] -> [BinomialTree a] -> [BinomialTree a]
mrg ts1 [] = ts1 
mrg [] ts2 = ts2
mrg ts1@(t1:ts1') ts2@(t2:ts2') 
    | orden t1 < orden t2 = t1 : mrg ts1' ts2
    | orden t2 < orden t1 = t2 : mrg ts1 ts2'
    | otherwise = insTree (enlazar t1 t2) (mrg ts1' ts2')

-- | Funcion auxiliar a deleteMin para eliminar el minimo de un @BinomialTree@ dado. 
removeMinTree :: (Ord a) => [BinomialTree a] -> (BinomialTree a, [BinomialTree a])
removeMinTree [] = error "Heap vacio"
removeMinTree [t] = (t, [])
removeMinTree (t:ts) = if raiz t < raiz t' then (t, ts) else (t',t:ts')
    where (t',ts') = removeMinTree ts

-- | Funcion que permite encontrar el minimo elemento de un @BinomialHeap@
findMin :: (Ord a) => BinomialHeap a -> a
findMin (BH ts) = raiz t
    where (t,_) = removeMinTree ts
    
-- | Funcion que permite eliminar el minimo elemento de un @BinomialTree@
deleteMin :: (Ord a) => BinomialHeap a -> BinomialHeap a
deleteMin (BH ts) = BH (mrg (reverse ts1) ts2)
    where (Nodo _ x ts1, ts2) = removeMinTree ts
