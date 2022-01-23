import Tipos.Usuario (User(..))
import Tipos.Song (Song(..))
import Tipos.Artist (Artist(..))
import Tipos.Album (Album(..))
import Tipos.Playlist (Playlist(..))
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Array

-----------------------------------------------------------------------------------------------------------------
-- Recomendar música a partir de una IA, para esto, por k-medias o DBSCAN, con favsongs como conjunto          --
-- de entrenamiento, crear diferentes playlists. A estas playlist, de la base de datos canciones, filtrada con --
-- algun criterio como popularidad, coincidencia de género o artistas relacionados y azar, añadir a las        --
-- playlists por muestreo con rechazo con un umbral alto                                                       -- 
-----------------------------------------------------------------------------------------------------------------

-- Definición de distancia en base a bpm, energy, danceability, dB, valence --
distanciaEuclidea v1 v2 = sqrt $sum [(v1!x - v2!x) ^ 2 | x <- [1..m]]
    where m = snd (bounds v1)
