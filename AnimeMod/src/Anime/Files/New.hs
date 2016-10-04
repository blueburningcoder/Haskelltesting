
-- | New File Management - Work in Progress :)

module Anime.Files.New where

import           General
import           Prelude
import           Anime.Types.New

import           Data.Binary (encodeFile, decodeFile)
import           System.Directory (doesFileExist, renameFile)
import           Data.IORef
import           Data.List (find, sort, delete, isInfixOf)
import qualified Data.Text as T
import qualified Data.Vector as V


-- | Saving the CompleteCollection
--
saveComplete :: Complete -> IO ()
saveComplete c = do
  encodeFile tempFileName $! c
  putStrLn "Saved"


-- | Loading a list of Anime from the Disk
--
load :: IO Complete
load = do
  exist <- doesFileExist fileName
  if exist then decodeFile $! fileName else saveComplete singletonCC >> resetFiles >> load


-- | Returns the saved IORef and thus prevents rereading the binary file
--
loadList :: IO Complete
loadList = do
  ref <- refComplete
  red <- readIORef ref
  return red


-- | Initially saves the IORef
--
refComplete :: IO (IORef Complete)
refComplete = load >>= newIORef


-- | The name of the Binary file including the list of Anime
fileName :: String
fileName = "anime.bin"

-- | The name of the temporary file
tempFileName :: String
tempFileName = ".anime.bin"


-- | Renames the newly written file if existent
--
resetFiles :: IO ()
resetFiles = do
  exist <- doesFileExist tempFileName
  if exist then renameFile tempFileName fileName else return ()


-- | One collection including all Anime, for easier searching and the like
--
completeList :: IO Collection
completeList = loadList >>= return . getAll


-- | Showing the List of Anime
--
showAnimeList :: [String] -> IO ()
showAnimeList args =
  case length args of
    0 -> loadList >>= print
    _ -> case isInfixOf (args !! 0) "-human" of
          True  -> loadList >>= putStrLn . T.unpack . listReadable'
          False -> showAnimeList []
--           False -> selectAnime (unwords args) >>= putStrLn . listAnime

-- | Simply shows all Anime saved
--
simpleShow :: IO ()
simpleShow = completeList >>= putStrLn . T.unpack . listCollection



-- | Gets the single Anime with this Name
--
getAnimeWithName :: String -> IO (Maybe Anime)
getAnimeWithName nam = do
  col <- completeList
  let res = V.find (\a -> name a == T.pack nam) (list col)
  return res


-- | Gets the single Anime with this ID
--
getAnimeWithId :: ID -> IO (Maybe Anime)
getAnimeWithId i = do
  col <- completeList
  let res = V.find (\a -> uniqueId a == i) (list col)
  return res


-- | Selecting an Anime based on the name
--
selectAnime :: String -> IO Anime
selectAnime ""  = prompt "Please enter at least something aout the Anime. : " >>= selectAnime
selectAnime nam = do
  col <- completeList
  let closest = isSameAnime' (T.pack nam) $ getClosest' (list col) 5 (\a -> T.isInfixOf (T.pack nam) (name a) )
  let names   = V.map (\a -> (uniqueId a, name a)) closest
  case V.length closest of
    0 -> testForId nam
    1 -> return $ closest V.! 0
    _ -> prompt ("Several found. Please specify name or id. " ++ show names ++ " : ") >>= selectAnime


-- | Checking if it actually was an id, otherwise just prompting selectAnime again
--
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


-- | Updating the files after checking for inconsistencies and such ... ?
--
updateComplete :: IO ()
updateComplete = undefined


-- | Adding the given Anime to the Category and saves it in the process
addAnime :: Anime -> Category -> IO ()
addAnime a c = loadList >>= saveComplete . (\com -> addAnime' com a c)


-- | Same as addAnime, but does not require IO (returns the new Complete instead)
addAnime' :: Complete -> Anime -> Category -> Complete
addAnime' com a c = replaceCategory com newCol
  where newCol = insert a $ getWithCategory c com


-- | sortAnime :: IO () .... after what criteria ?
-- sortAnime = undefined

{-

-- | Gets the single Anime with this ID from this Collection, for then there is no IO done
getAnimeWithId' :: ID -> CompleteCollection -> Maybe Anime
getAnimeWithId' iD (Co wa ne ot) = do
  let complete = wa ++ ne ++ ot
  find (\a -> getId a == iD) complete

-- | Adding an Anime to the saved binary
addAnime :: Anime -> IO ()
addAnime new = do
  oth <- getOther
  saveOther $ addToEnd oth new


-- | Sorting the list of known Anime
sortAnime :: IO ()
sortAnime = applyToAll sort


-- | Deleting the selected Anime
deleteAnime :: Anime -> IO ()
deleteAnime anime = prompt ("Deleting " ++ name anime ++ ". [Enter] : ") >> applyToAll (delete anime)

-- | Actually sorting them in the right category
sortAllAnime :: [String] -> IO ()
sortAllAnime args =
  case length args of
    0 -> completeList >>= saveComplete . (foldl sortToRightCategory (Co [] [] []) )
    _ -> case isInfixOf (args !! 0) "-order" of
          True  -> sortAnime
          False -> sortAllAnime []

-- | Sorting one Anime in the Right category
sortToRightCategory :: CompleteCollection -> Anime -> CompleteCollection
sortToRightCategory  co           a@(Anime _ "" NoRating  Unknown Unknown   ) = co
sortToRightCategory (Co wa ne ot) a@(Anime _ _  NoRating  Unknown Unknown   ) = Co wa ne (addToEnd ot a)
sortToRightCategory (Co wa ne ot) a@(Anime _ _  NoRating  w       uw        ) =
  if w == uw && w /= Unknown then Co (addToEnd wa a) ne ot else Co wa (addToEnd ne a) ot
sortToRightCategory (Co wa ne ot) a@(Anime _ _   _        _         _         ) = Co (addToEnd wa a) ne ot

-}
