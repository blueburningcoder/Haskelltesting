module Anime.Files where

import General
import Anime.Types.Old

import Data.Binary (Binary (..), Get, encodeFile, decodeFile)
import System.Directory (doesFileExist, renameFile)
import Data.List (find, sort, delete, isInfixOf)
import Data.IORef
import Debug.Trace (trace)


-- | returns the highest id of all currently known Anime
highestId :: IO ID
highestId = completeList >>= return . (\a -> maximum . map getId $! a)

-- | just returns it if the given id is free, otherwise returns the next free id
isFree :: ID -> CompleteCollection -> ID
isFree id co =
  case getAnimeWithId' id co of
    Nothing -> id
    Just anime -> isFree (id + 1) co

isFree' :: ID -> IO ID
isFree' id = do
  found <- getAnimeWithId id
  case found of
    Nothing    -> return id
    Just anime -> isFree' (id + 1)

-- | the name of the Binary file including the list of Anime
fileName :: String
fileName = "anime.bin"
tempFileName = ".anime.bin"

-- | Saving a list of Anime to the Disk
saveComplete :: CompleteCollection -> IO ()
saveComplete c = do
  encodeFile tempFileName $! c
  putStrLn "Saved"

-- | saves the modified 'watched'-field
saveWatched :: WatchedAnime -> IO ()
saveWatched w = do
  (Co _ ne ot) <- loadList
  saveComplete $! (Co w ne ot)

-- | returns all watched Anime
getWatched :: IO WatchedAnime
getWatched = loadList >>= return . watched

-- | saves the modified 'next'-field
saveNext :: NextAnime -> IO ()
saveNext ne = do
  (Co wa _ ot) <- loadList
  saveComplete $! (Co wa ne ot)

-- | returns all next Anime
getNext :: IO NextAnime
getNext = loadList >>= return . next

-- | saves the modified 'other'-field
saveOther :: OtherAnime -> IO ()
saveOther ot = do
  (Co wa ne _) <- loadList
  saveComplete $! (Co wa ne ot)

-- | returns all other Anime
getOther :: IO OtherAnime
getOther = loadList >>= return . other

-- | Loading a list of Anime from the Disk
load :: IO CompleteCollection
load = do
  exist <- doesFileExist fileName
  if exist then decodeFile $! fileName else saveComplete (Co [] [] []) >> resetFiles >> load

-- | returns the saved IORef and thus prevents rereading the binary file
loadList :: IO CompleteCollection
loadList = do
  ref <- refComplete
  red <- readIORef ref
  return red

-- | initially saves the IORef
refComplete :: IO (IORef CompleteCollection)
refComplete = load >>= newIORef

-- | returns a list of all Anime known
completeList :: IO AllAnime
completeList = loadList >>= return . (\co -> watched co ++ next co ++ other co)

-- | showing the List of Anime
showAnimeList :: [String] -> IO ()
showAnimeList args =
  case length args of
    0 -> loadList >>= print
    _ -> case isInfixOf (args !! 0) "-human" of
          True  -> loadList >>= putStrLn . listReadable
          False -> selectAnime (unwords args) >>= putStrLn . listAnime


-- | gets the single Anime with this Name
getAnimeWithName :: String -> IO (Maybe Anime)
getAnimeWithName nam = do
  list <- completeList
  let res = find (\a -> name a == nam) list
  return res


-- | gets the single Anime with this ID
getAnimeWithId :: ID -> IO (Maybe Anime)
getAnimeWithId iD = do
  list <- completeList
  let res = find (\a -> getId a == iD) list
  return res

-- | seletcing an Anime based on the name
selectAnime :: String -> IO Anime
selectAnime ""  = prompt "Please enter at least something aout the Anime. : " >>= selectAnime
selectAnime nam = do
  list <- completeList
  let closest = isSameAnime nam $ getClosest list 5 (\a -> isInfixOf nam (name a) )
  let names   = [(getId a, name a) | a <- closest]
  case length closest of
    0 -> testForId nam
    1 -> return $ closest !! 0
    n -> prompt ("Several found. Please specify name or id. " ++ show names ++ " : ") >>= selectAnime

-- | checking if it actually was an id, otherwise just prompting selectAnime again
testForId :: String -> IO Anime
testForId str =
  let might   = maybeRead str :: Maybe ID in
  case might of
    Nothing    -> prompt "Not found. Search for: " >>= selectAnime
    (Just sth) -> do
      an <- getAnimeWithId sth
      case an of
        Nothing      -> prompt "No Anime with this Name or id. Try again. : " >>= selectAnime
        (Just anime) -> return anime

-- | gets the single Anime with this ID from this Collection, for then there is no IO done
getAnimeWithId' :: ID -> CompleteCollection -> Maybe Anime
getAnimeWithId' iD (Co wa ne ot) = do
  let complete = wa ++ ne ++ ot
  find (\a -> getId a == iD) complete

-- | adding an Anime to the saved binary
addAnime :: Anime -> IO ()
addAnime new = do
  oth <- getOther
  saveOther $ addToEnd oth new


-- | sorting the list of known Anime
sortAnime :: IO ()
sortAnime = applyToAll sort


-- | deleting the selected Anime
deleteAnime :: Anime -> IO ()
deleteAnime anime = prompt ("Deleting " ++ name anime ++ ". [Enter] : ") >> applyToAll (delete anime)


-- | applies the given function to all three lists of Anime
applyToAll :: (AllAnime -> AllAnime) -> IO ()
applyToAll f = loadList >>= saveComplete . putInside f

-- | applies a function to every list of Animes
putInside :: (AllAnime -> AllAnime) -> CompleteCollection -> CompleteCollection
putInside f (Co wa ne ot) = Co (f wa) (f ne) (f ot)

-- | actually sorting them in the right category
sortAllAnime :: [String] -> IO ()
sortAllAnime args =
  case length args of
    0 -> completeList >>= saveComplete . (foldl sortToRightCategory (Co [] [] []) )
    _ -> case isInfixOf (args !! 0) "-order" of
          True  -> sortAnime
          False -> sortAllAnime []

-- | sorting one Anime in the Right category
sortToRightCategory :: CompleteCollection -> Anime -> CompleteCollection
sortToRightCategory  co           a@(Anime _ "" NoRating  Unknown Unknown   ) = co
sortToRightCategory (Co wa ne ot) a@(Anime _ _  NoRating  Unknown Unknown   ) = Co wa ne (addToEnd ot a)
sortToRightCategory (Co wa ne ot) a@(Anime _ _  NoRating  w       uw        ) =
  if w == uw && w /= Unknown then Co (addToEnd wa a) ne ot else Co wa (addToEnd ne a) ot
sortToRightCategory (Co wa ne ot) a@(Anime _ _   _        _         _         ) = Co (addToEnd wa a) ne ot

-- | reading the arguments, and turning it in a tuple of the whole name and the maybe ID
readArgs :: [String] -> (String, Maybe ID)
readArgs [] = ("", Nothing)
readArgs li =
  case maybeRead (last li) :: Maybe ID of
    Just id -> (unwords $ init li, Just id)
    Nothing -> (unwords li, Nothing)

-- | renames the newly written file if existent
resetFiles :: IO ()
resetFiles = do
  exist <- doesFileExist tempFileName
  if exist then renameFile tempFileName fileName else return ()
