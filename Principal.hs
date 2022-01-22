import System.IO
import System.Directory
import Control.Exception (catch, SomeException)
-- import Proyecto.Recomendacionporartista

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Bienvenido a nuestro programa de recomendación de música"
  putStrLn "Que desea hacer ?"
  putStrLn "1. recomendar por artista"
  putStrLn "2. ...."
  putStrLn "3. ...."
  o <- getChar 
  if o == '1' then do 
      putStrLn "Opción 1: "
  else do 
      putStrLn "Otra opción"
