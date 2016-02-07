module Anime.Files where

import Anime.Types
import Data.Binary (Binary (..), Get, encodeFile, decodeFile)
import System.IO (hFlush, stdout)

-- the name of the Binary file including the list of Anime
fileDir :: String
fileDir = "anime.bin"

-- Saving a list of Anime to the Disk
saveComplete :: CompleteCollection -> IO ()
saveComplete = encodeFile $! fileDir

-- saves the modified 'watched'-field
saveWatched :: WatchedAnime -> IO ()
saveWatched w = do
    (Co _ ne ot) <- loadList
    encodeFile fileDir $! (Co w ne ot)

-- saves the modified 'next'-field
saveNext :: NextAnime -> IO ()
saveNext ne = do
    (Co wa _ ot) <- loadList
    encodeFile fileDir $! (Co wa ne ot)

-- saves the modified 'other'-field
saveOther :: OtherAnime -> IO ()
saveOther ot = do
    (Co wa ne _) <- loadList
    encodeFile fileDir $! (Co wa ne ot)

-- Loading a list of Anime from the Disk
loadList :: IO CompleteCollection
loadList = decodeFile $! fileDir

-- showing the List of Anime
showAnimeList :: IO ()
showAnimeList = loadList >>= print
