module Tipos.Artist(
name ,
popularity ,
genres ,
followers
)where

data Artist = Artist {  name :: String,
                        popularity :: Int,
                        genres :: [String],
                        followers :: Int
                        }deriving (Show)