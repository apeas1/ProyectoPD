import System.IO
import System.Directory
import Control.Exception (catch, SomeException)
import ProyectoPD.Recomendacionporartista
import ProyectoPD.Data.Lectorcsv

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Bienvenido a nuestro programa de recomendación de música"
  putStrLn "Que desea hacer ?"
  putStrLn "1. recomendar por artista"
  putStrLn "2. recomendar canciones"
  putStrLn "3. ...."
  o <- getChar 
  if o == '1' then do 
      putStrLn "Opción 1: "
  else if o == '2' then do 
      putStrLn "Opción 2: "
      ---lector
  else do 
      putStrLn "Otra opción"
