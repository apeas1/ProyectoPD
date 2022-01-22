module Data.Lectorcsv
(lector
)where

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Char (isAlpha)
import Data.List ()
import Data.Text (Text)
import Text.CSV ( parseCSVFromFile )
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import System.Directory ( doesFileExist, canonicalizePath )
---import Proyecto.Tipos.Song as Song
import GHC.Generics (Generic)

data Song = Song {  title :: !Text,
                    artist :: !Text,
                    genre :: !Text,
                    year :: !Text,
                    bpm :: !Int,
                    energy :: !Int,
                    danceability :: !Int,
                    dB :: !Int,
                    liveness :: !Int, --probabilidad de que haya una audiencia en la cancion
                    valence :: !Int, --cuanto mayor el valor mayor positividad en tono de la cancion
                    duration :: !Int,
                    acoustic :: !Int
                    }deriving (Generic, Show)

instance FromNamedRecord Song where
parseNamedRecord x = Song <$> (x .: "title") <*> (x .: "artist") <*> (x .: "genre") <*> (x .: "year") <*> 
                 (x .: "bpm") <*> (x .: "energy") <*> (x .: "danceability") <*> (x .: "dB") <*>
                 (x .: "liveness") <*> (x .: "valence") <*> (x .: "duration") <*> (x .: "acoustic")

instance ToNamedRecord Song where
  toNamedRecord (Song title artist genre year bpm energy danceability dB liveness valence duration acoustic) = 
    namedRecord ["title" .= title, "artist" .= artist, "genre" .= genre, "year" .= year, "bpm" .= bpm, 
                 "energy" .= energy, "danceability" .= danceability, "dB" .= dB, "liveness" .= liveness,
                 "valence" .= valence, "duration" .= duration, "acoustic" .= acoustic]

instance DefaultOrdered Song where
  headerOrder _ = header ["title",  "artist",  "genre",  "year", "bpm", "energy", 
                          "danceability", "dB", "liveness", "valence", "duration", "acoustic"]

lector :: IO ()
main = do
    f <- BL.readFile "file.csv"
    case decodeByName f of
        Left err      -> print err
        Right (_, xs) -> Song xs $ \(Song x y) -> print (x, y)

---leectorcsv = do
---  contenido <- parseCSVFromFile "19502010.csv"
---  let filas = case contenido of
---             Right filas -> filas
---             _ -> []
---  let cabecera = head filas
---  let registros = decodeByName (tail filas)
---  putStrLn registros
  

 --- sequence_ [frecuencias campo valores | (campo, valores) <- zip cabecera  (traspuesta' registros)]