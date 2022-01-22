module Tipos.Album(Album(..)
)where
import Tipos.Song
import Tipos.Artist

data Album = Album {nameAl :: String,
                    artists :: [Artist],
                    genresAl :: [String],
                    labelAl :: String,
                    popularityAl :: Int,
                    number_tracks :: Int,
                    tracks :: [Song]
                    }deriving (Show)