module Song
(Song
)where

data Song = Song {  title :: String,
                    artist :: String,
                    genre :: String,
                    year :: String,
                    bpm :: Int,
                    energy :: Int,
                    danceability :: Int,
                    dB :: Int,
                    liveness :: Int, --probabilidad de que haya una audiencia en la cancion
                    valence :: Int, --cuanto mayor el valor mayor positividad en tono de la cancion
                    duration :: Int,
                    acoustic :: Int
                    }deriving (Eq, Show, Read)

