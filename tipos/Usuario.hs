import Tipos.Song
import Tipos.Artist
import Tipos.Album

data User = User { userId :: Int,
                   favsongs :: [Song],
                   favartists :: [Artist],
                   favalbums :: [Album]

}