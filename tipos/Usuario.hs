module Tipos.Usuario
(User(..)
)where

import Tipos.Song
import Tipos.Artist
import Tipos.Album

data User = User { userId :: Int,
                   username :: String,
                   favsongs :: [Song],
                   favartists :: [Artist],
                   favalbums :: [Album]

}deriving (Show)