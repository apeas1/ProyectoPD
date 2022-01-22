import Tipos.Song
import Tipos.Artist

data Playlist = Playlist {  name :: String,
                            genres :: [String],
                            artists :: [Artist],
                            num_tracks :: Int,
                            tracks :: [Song]
                            }deriving (Show)