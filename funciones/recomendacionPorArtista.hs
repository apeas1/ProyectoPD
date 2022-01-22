module Funciones.Recomendacionporartista
()where

import Tipos.Usuario
import Tipos.Song
import Tipos.Artist
import Tipos.Album
import Test.QuickCheck
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Relacionar artistas entre si por los usuarios que les escuchan
-- Lista de artistas
-----------------------------------------------------------------------------------------------------------------------------------------
--                                     La función recomendados ejecuta el resto del archivo                                            --
-----------------------------------------------------------------------------------------------------------------------------------------

a = Artist { name = "AA", popularity = 30, genres =["pop"], followers= 3}
b = Artist { name = "BB", popularity = 30, genres = ["pop"], followers = 3}
c = Artist { name = "CC", popularity = 30, genres = ["rock"], followers = 4}
d = Artist { name = "DD", popularity = 30, genres = ["rock", "pop"], followers = 3}

al1 = Album {nameAl= "e", artists= [a], genresAl= ["pop"], label = "una", popularityAl = 23, number_tracks= 4, tracks= [s1] }
al2 = Album {nameAl = "a", artists= [b], genresAl= ["pop"], label = "una", popularityAl = 23, number_tracks= 4, tracks= [s2] }
s1 = Song {title="ho", artist="AA", genre="pop", year="2000", bpm=39, energy=22, danceability=8, dB=22, liveness=1, valence=2, duration=4, acoustic=9}
s2 = Song {title="n", artist="BB", genre="pop", year="1990", bpm=100, energy=52, danceability=8, dB=22, liveness=1, valence=4, duration=3, acoustic=5}

us1 = User 1 [s1,s2] [a, b, c, d] [al1]
us2 = User 2 [s1,s2] [a, b] [al1]
us3 = User 3 [s1] [a] [al2]
us4 = User 4 [s2] [b] [al2]
us5 = User 5 [s2] [] []
us6 = User 6 [s1,s2] [c] [al1, al2]
us7 = User 7 [s1] [c, d] [al2]
us8 = User 8 [s2] [c, d] [al1, al2]
us9 = User 9 [] [d] []

usuarios = [us1, us2, us3, us4, us5, us6, us7, us8, us9]
-- Hace falta una función que cree el diccionario (usuario, [artistas favoritos]) pero para eso hace falta tener bien definido el tipo usuario

parUsA (User i s a al) = (i, (map (\(Artist n p g f) -> n ) a))

convertirEnMap xs = Map.fromList [parUsA x | x <- xs]

-- ejemplo para probar las funciones
m1 = Map.fromList [(1,["AA","BB","CC","DD"]), (2,["BB","AA"]), (3, ["AA"]), (4,["BB"]), (5,[]), (6, ["CC"]), (7, ["CC", "DD"]),(8, ["CC", "DD"]), (9,["DD"])] 

-- dictArtistaOyente m1 = [("AA",[1,2,3]),("BB",[1,2,4]),("CC",[1,6,7,8]),("DD",[1,7,8,9])]
-- recomendados m1 = fromList [(1,[]),(2,["CC","DD"]),(3,["BB","CC","DD"]),(4,["AA","CC","DD"]),(5,[]),(6,["DD"]),(7,[]),(8,[]),(9,["CC"])]

-- Primero: crear a partir de este diccionario uno cuyas claves son los artistas y valores la lista de sus oyentes [(Artista,[Usuario])]

dictArtistaOyente xs = Map.fromList [(x, (oyentes x m)) | x <- (artistasTotal m)]
    where m = convertirEnMap xs

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
recomendados xs  = Map.fromList [(k, obtenerRelacionados v m2) | (k,v) <- Map.toList m1 ]
    where m2 = relaciones  (dictArtistaOyente xs)
          m1 = convertirEnMap xs

obtenerRelacionados  xs m = (Set.toList ( Set.fromList (concat [v | (k,v) <- (Map.toList m), (elem k xs)] ))) \\ xs

