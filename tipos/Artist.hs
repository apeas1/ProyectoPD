<<<<<<< HEAD
module Tipos.Artist(Artist (..)
=======
module Tipos.Artist(
Artist 
>>>>>>> af3643b71acdc826e9f94e6580384d434e06643e
)where

data Artist = Artist {  name :: String,
                        popularity :: Int,
                        genres :: [String],
                        followers :: Int
                        }deriving (Show)