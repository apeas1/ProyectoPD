module Tipos.Album
(Album
)where
import Tipos.Song
import Tipos.Artist

data Album = Album {name :: String,
                    artists :: [Artist],
                    genres :: [String],
                    label :: String,
                    popularity :: Int,
                    number_tracks :: Int,
                    tracks :: [Song]
                    }deriving (Show)