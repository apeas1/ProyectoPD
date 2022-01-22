module Tipos.Playlist(Playlist (..)
)where

import Tipos.Song
import Tipos.Artist

data Playlist = Playlist {  nameP :: String,
                            genresP :: [String],
                            artistsP :: [Artist],
                            num_tracks :: Int,
                            tracksP :: [Song]
                            }deriving (Show)