import System.IO
import System.Directory
import Control.Exception (catch, SomeException)
import Funciones.Recomendacionporartista
import Funciones.RecomendacionPorGenero
import Tipos.Artist
import Tipos.Album
import Tipos.Song
import Tipos.Usuario
import qualified Data.Map as Map
import Data.Char
import Control.Monad.Compat (forever)
---import Data.Lectorcsv

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Bienvenido a nuestro programa de recomendación de música"
  putStrLn "Que desea hacer ?"
  putStrLn "1. recomendacion por artista"
  putStrLn "2. recomendacion por genero"
  putStrLn "x. salir"
  o <- getChar 
  getChar
  putStrLn ""
  if o == '1' then do 
      putStrLn "Opción 1: Recomendacion por artista \n"
      putStrLn "Los artistas recomendados para cada usuario son : \n"
      showRecomendados
      putStrLn ""
      putStrLn "Pulse c para volver al menu, de lo contrario se cerrará el pograma"
      p <- getChar
      getChar 
      if p == 'c' then do
          main
      else do 
          return () 
  else if o == '2' then do 
      putStrLn "Opción 2: Recomendacion por genero \n"
      putStrLn "Los playlists recomendadas para cada usuario son : \n"
      showRecomendadosGenero
      putStrLn ""
      putStrLn "Pulse c para volver al menu, de lo contrario se cerrará el pograma"
      p <- getChar
      getChar 
      if p == 'c' then do
          main
      else do 
          return () 
  else if o == 'x' then do
      return ()
  else do 
      putStrLn "Error"
      main
  putStrLn " Hasta la próxima ! "
  _ <- getLine
  return ()
