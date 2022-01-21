import Song
import Artist

data Album = Album {name :: String,
                    artists :: [Artist],
                    genres :: [String],
                    label :: String,
                    popularity :: Int,
                    number_tracks :: Int,
                    tracks :: [Song]
                    }deriving (Show)