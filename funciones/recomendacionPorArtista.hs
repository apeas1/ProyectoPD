module Funciones.Recomendacionporartista
()where

import Test.QuickCheck
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
-- Relacionar artistas entre si por los usuarios que les escuchan
-- Lista de artistas
-----------------------------------------------------------------------------------------------------------------------------------------
--                                     La función recomendados ejecuta el resto del archivo                                            --
-----------------------------------------------------------------------------------------------------------------------------------------


-- Hace falta una función que cree el diccionario (usuario, [artistas favoritos]) pero para eso hace falta tener bien definido el tipo usuario
-- ejemplo para probar las funciones
m1 = Map.fromList [(1,["AA","BB","CC","DD"]), (2,["BB","AA"]), (3, ["AA"]), (4,["BB"]), (5,[]), (6, ["CC"]), (7, ["CC", "DD"]),(8, ["CC", "DD"]), (9,["DD"])] 

-- dictArtistaOyente m1 = [("AA",[1,2,3]),("BB",[1,2,4]),("CC",[1,6,7,8]),("DD",[1,7,8,9])]
-- recomendados m1 = fromList [(1,[]),(2,["CC","DD"]),(3,["BB","CC","DD"]),(4,["AA","CC","DD"]),(5,[]),(6,["DD"]),(7,[]),(8,[]),(9,["CC"])]

-- Primero: crear a partir de este diccionario uno cuyas claves son los artistas y valores la lista de sus oyentes [(Artista,[Usuario])]

dictArtistaOyente m = Map.fromList [(x, (oyentes x m)) | x <- xs]
    where xs = artistasTotal m

artistasTotal m = Set.toList ( Set.fromList (concat ( Map.elems m)))



oyentes x m = [k | (k,v) <- (Map.toList m), elem x v]

-- Segundo: comparar los oyentes de cada artista para ver cuantos tienen en común

oyentesIguales [] ys = 0
oyentesIguales  (x:xs) ys
    | elem x ys = 1 + oyentesIguales xs ys
    | otherwise = oyentesIguales xs ys

comparacionOyentes xs yss = [(oyentesIguales xs ys) / l | ys <- yss] 
    where l = sum [1 | x <- xs]

dicEnComun xs ys = Map.fromList (zip xs ys)

filtrado m = Map.keys (Map.filter (>0.3) m )
-- Tercero: recoger las relaciones entre artistas en un diccionario
--Esto nos devuelve un diccionario de un artista y sus artistas relacionados a partir de un diccionario de un artista y sus oyentes
relaciones m = Map.fromList [(k, filtrado (dicEnComun (delete k ks) (comparacionOyentes e (delete e es)))) | (k,e) <- (Map.toList m)]
    where ks = Map.keys m
          es = Map.elems m


-- Cuarto:Recomendar a un usuario los artistas relacionados con sus artistas favoritos que aun no escuche.
-- A partir del diccionario original devolver un diccionario de usuario con los artistas recomendados

recomendados m1  = Map.fromList [(k, obtenerRelacionados v m2) | (k,v) <- Map.toList m1 ]
    where m2 = relaciones  (dictArtistaOyente m1)

obtenerRelacionados  xs m = (Set.toList ( Set.fromList (concat [v | (k,v) <- (Map.toList m), (elem k xs)] ))) \\ xs

