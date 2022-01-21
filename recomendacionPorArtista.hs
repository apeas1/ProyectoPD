import Test.QuickCheck
import Data.List
import qualified Data.Map as Map
-- Relacionar artistas entre si por los usuarios que les escuchan
-- Lista de artistas

-- Hace falta sacar la lista de artistas por usuario pero para eso hace falta definir bien el tipo Usuario
-- Sacar una lista con todos los artistas
artistasTotal :: Eq a => [[a]] -> [a]
artistasTotal xss = artistasAux xss []

artistasAux [] ys = ys
artistasAux (xs:xss) ys = artistasAux xss (artistasUsuario xs ys)

artistasUsuario :: Eq a => [a] -> [a] -> [a]
artistasUsuario (x:xs) aux
    | aux == [] = xs
    | elem x aux = artistasUsuario xs aux
    | otherwise = artistasUsuario xs (x:aux)



-- Hacer una  lista de listas con los usuarios que escuchan a cada artista

-- Comparar los oyentes de cada artista para ver cuantos tienen en común
-- Teniendo un Map de artistas con sus oyentes

-- ejemplos para probar
m1 = Map.fromList [("BB",[1,4,2]),("AA",[1,2,3]), ("CC", [1, 8, 7, 6]), ("DD", [8, 7 , 9 , 1])]
oyentesIguales [] ys = 0
oyentesIguales  (x:xs) ys
    | elem x ys = 1 + oyentesIguales xs ys
    | otherwise = oyentesIguales xs ys

comparacionOyentes xs yss = [(oyentesIguales xs ys) / l | ys <- yss] 
    where l = sum [1 | x <- xs]

dicEnComun xs ys = Map.fromList (zip xs ys)

filtrado m = Map.keys (Map.filter (>0.3) m )

--Esto nos devuelve un diccionario de un artista y sus artistas relacionados a partir de un diccionario de un artista y sus oyentes
relaciones m = [(k, filtrado (dicEnComun (delete k ks) (comparacionOyentes e (delete e es)))) | (k,e) <- (Map.toList m)]
    where ks = Map.keys m
          es = Map.elems m

-- Recomendar a un usuario los artistas relacionados con sus artistas favoritos que aun no escuche

-- Funcion que, devuelva un diccionario de un usuario y sus artistas preferidos

-- A partir de este diccionario y del diccionario que relaciona artistas devolver una lista con los artistas recomendados

recomendados m1 m2 = undefined
