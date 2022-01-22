module Tipos.Artist(Artist (..)
)where

data Artist = Artist {  name :: String,
                        popularity :: Int,
                        genres :: [String],
                        followers :: Int
                        }deriving (Show)