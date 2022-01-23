import Tipos.Usuario (User(..))
import Tipos.Song (Song(..))
import Tipos.Artist (Artist(..))
import Tipos.Album (Album(..))
import Tipos.Playlist (Playlist(..))
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Recomendación por género -- 
recPorGenero users songs = parsearFinal (relMeta ma mb mc)
    where ma = dictUsCa users
          mb = convertirEnMap users 
          mc = dictGenCa (listaGen canciones) songs
-- Crear una playlist por género que escucha cada usuario

-- Géneros que escucha un usuario
ma = dictUsCa usuarios
mb = convertirEnMap usuarios 
mc = dictGenCa (listaGen canciones) canciones
-- Hace falta sacar un diccionario de [(usuarioID, [cancion])] de un usuario con los géneros que escucha
parUsGe (User id s _ _) = (id, Set.toList setG)
    where setG = Set.fromList (map (getGen) s)

getGen (Song a b ge d e f g h i j k l) = ge

convertirEnMap xs = Map.fromList [parUsGe x | x <- xs]

-- Canciones de un género 
-- Desde la lista de canciones, una lista de géneros

listaGen xs = Set.toList  $Set.fromList [ getGen x | x<- xs]
-- Un diccionario que relaciona género y canciones

dictGenCa gs cs = Map.fromList [(g,( filter (\x -> (p x g)) cs)) | g <- gs ]
    where p x y = (getGen x) ==y

-- Crear una playlist por género por usuario

-- Para eso es necesario una relación [(usuario, [(genero, [canciones])])] filtrando las canciones que el usuario ya escucha 
-- Así que también necesitamos un diccionario de usuarios con las canciones que escucha
dictUsCa xs = Map.fromList [(id, s) | (User id s _ _) <- xs]
--Entrada: una lista de canciones, una lista de géneros, y un map [(género, [canción])]
dictPorUsuario xs  g m2
    | Map.null m2 = []
    | elem (fst l) g = ((fst l), (snd l)\\xs): dictPorUsuario xs g (Map.fromList l2)
    | otherwise = dictPorUsuario xs g (Map.fromList l2)
    where (l:l2) =  Map.toList m2
-- Entrada: map [(usuario, [canción])], map [(usuario, [género])], map [(género, [canción])]
relMeta m1 m2 m3 = Map.fromList [(u,(dictPorUsuario (m1 Map.! u) (m2 Map.! u) m3)) | u <- uID ]
    where uID = Map.keys m1

-- Construcción de una playist { nameP = género, genresP =[género], artistsP = [], num_tracks = contar tracksP , tracksP = [lista canciones]}

datosPorPlaylist ps = [Playlist { nameP=k, genresP = [k], artistsP = (map (\(Song _ a _ _ _ _ _ _ _ _ _ _ ) -> a) v), num_tracks= (length v), tracksP=(map (\(Song t _ _ _ _ _ _ _ _ _ _ _ ) -> t) v) } | (k, v) <- ps, v/=[]]
parsearFinal m = [(u, datosPorPlaylist k) | (u, k) <- Map.toList m]

a = Artist { name = "AA", popularity = 30, genres =["pop"], followers= 3}
b = Artist { name = "BB", popularity = 30, genres = ["pop"], followers = 3}
c = Artist { name = "CC", popularity = 30, genres = ["rock"], followers = 4}
d = Artist { name = "DD", popularity = 30, genres = ["rock", "pop"], followers = 3}

al1 = Album {nameAl= "e", artists= [a], genresAl= ["pop"], labelAl = "una", popularityAl = 23, number_tracks= 4, tracks= [s190] }
al2 = Album {nameAl = "a", artists= [b], genresAl= ["pop"], labelAl = "una", popularityAl = 23, number_tracks= 4, tracks= [s200] }

us1 = User 1 [s189,s200, s190, s191, s195] [a, b, c, d] [al1]
us2 = User 2 [s190,s191, s192, s193] [a, b] [al1]
us3 = User 3 [s193, s194, s195, s196, s200, s201] [a] [al2]
us4 = User 4 [s199, s197, s198, s202] [b] [al2]
us5 = User 5 [s203, s189, s199, s204, s205, s195] [] []
us6 = User 6 [s195,s205, s199, s196, s197] [c] [al1, al2]
us7 = User 7 [s198, s199, s200, s201, s197] [c, d] [al2]
us8 = User 8 [s200, s201, s202, s197, s198, s199, s200] [c, d] [al1, al2]
us9 = User 9 [s190, s192, s194, s196, s198, s200, s202, s204] [d] []

usuarios = [us1, us2, us3, us4, us5, us6, us7, us8, us9]

s189  = Song "Take It Easy - 2013 Remaster" "Eagles" "album rock" 1972 139 67 57 (-10) 13 74 212 34 
s190  = Song "Smoke On The Water - Remastered 2012" "Deep Purple" "album rock" 1972 114 68 60 (-9) 11 81 341 13 
s191  = Song "Gimme! Gimme! Gimme! (A Man After Midnight)" "ABBA" "europop" 1979 120 49 75 (-10) 16 54 293 2 
s192  = Song "Love Of My Life - Remastered 2011" "Queen" "glam rock" 1975 154 18 33 (-12) 12 26 217 93 
s193  = Song "Father And Son" "Yusuf / Cat Stevens" "british folk" 1970 136 33 50 (-11) 10 40 221 59 
s194  = Song "Rebel Rebel - 2016 Remaster" "David Bowie" "album rock" 1974 126 69 64 (-16) 28 46 275 21 
s195  = Song "You're The One That I Want - From Grease"  "John Travolta" "hollywood" 1978 107 61 76 (-11) 10 82 170 28 
s196  = Song "The Boys Are Back In Town" "Thin Lizzy" "album rock" 1976 81 71 45 (-10) 21 77 267 23 
s197  = Song "Brandy (You're a Fine Girl)" "Looking Glass" "soft rock" 1972 125 63 72 (-11) 13 83 187 40 
s198  = Song "Starman - 2012 Remaster" "David Bowie" "album rock" 1972 100 45 49 (-10) 54 55 254 17 
s199  = Song "Mamma Mia" "ABBA" "europop" 1975 138 75 75 (-7) 47 83 213 30 
s200  = Song " Life in the Fast Lane - 2013 Remaster" "Eagles" "album rock" 1976 110 76 67 (-7) 5 88 286 10 
s201  = Song "Rock with You - Single Version" "Michael Jackson" "pop" 1979 114 54 81 (-13) 16 85 221 18 
s202  = Song "Another Brick In The Wall Pt. 2 - 2011 Remastered Version" "Pink Floyd" "album rock" 1979 104 39 69 (-16) 25 72 239 8 
s203  = Song "YMCA - Original Version 1978" "Village People" "disco" 1978 127 97 72 (-5) 12 73 287 6 
s204  = Song "Take Me Home Country Roads" "John Denver" "adult standards" 1971 82 40 46 (-9) 12 63 190 76 
s205  = Song "Reelin' In The Years" "Steely Dan" "album rock" 1972 135 76 52 (-10) 11 71 275 12 

canciones = [s189, s190, s191, s192, s193, s194, s195, s196, s197, s198, s199, s200, s201, s202, s203, s204, s205]