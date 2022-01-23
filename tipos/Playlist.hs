module Tipos.Playlist(Playlist (..)
)where

-- import Tipos.Song
-- import Tipos.Artist

data Playlist = Playlist {  nameP :: String,
                            genresP :: [String],
                            artistsP :: [String],
                            num_tracks :: Int,
                            tracksP :: [String]
                            }deriving (Show)