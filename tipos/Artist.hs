module Tipos.Artist(
Artist
)where

data Artist = Artist {  name :: String,
                        popularity :: Int,
                        genres :: [Int],
                        followers :: Int
                        }deriving (Show)