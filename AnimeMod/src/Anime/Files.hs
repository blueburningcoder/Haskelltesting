module Anime.Files where

import Anime.Types
import Data.Binary (Binary (..), Get, encodeFile, decodeFile)
import System.Directory (doesFileExist)
import Data.List (find)

highestId :: IO ID
highestId = completeList >>= return . (\a -> maximum . map getId $! a)

-- the name of the Binary file including the list of Anime
fileDir :: String
fileDir = "anime.bin"

-- Saving a list of Anime to the Disk
saveComplete :: CompleteCollection -> IO ()
saveComplete c = do
  encodeFile fileDir $! c
  putStrLn "Saved"

-- saves the modified 'watched'-field
saveWatched :: WatchedAnime -> IO ()
saveWatched w = do
  (Co _ ne ot) <- loadList
  saveComplete $! (Co w ne ot)

-- saves the modified 'next'-field
saveNext :: NextAnime -> IO ()
saveNext ne = do
  (Co wa _ ot) <- loadList
  saveComplete $! (Co wa ne ot)

-- saves the modified 'other'-field
saveOther :: OtherAnime -> IO ()
saveOther ot = do
  (Co wa ne _) <- loadList
  saveComplete $! (Co wa ne ot)

-- Loading a list of Anime from the Disk
load :: IO CompleteCollection
load = do
  exist <- doesFileExist fileDir
  if exist then decodeFile $! fileDir else saveComplete (Co [] [] []) >> load

-- strictness prevents reloading ?
loadList :: IO CompleteCollection
loadList = id $! load

-- returns a list of all Anime known
completeList :: IO AllAnime
completeList = loadList >>= return . (\co -> watched co ++ next co ++ other co)

-- showing the List of Anime
showAnimeList :: IO ()
showAnimeList = loadList >>= print


-- gets the single Anime with this Name
getAnimeWithName :: String -> IO (Maybe Anime)
getAnimeWithName nam = do
  list <- completeList
  let res = find (\a -> name a == nam) list
  return res


-- gets the single Anime with this ID
getAnimeWithId :: ID -> IO (Maybe Anime)
getAnimeWithId iD = do
  list <- completeList
  let res = find (\a -> getId a == iD) list
  return res



