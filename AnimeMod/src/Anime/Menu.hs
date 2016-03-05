module Anime.Menu where

import General
import Anime.Types
import Anime.Files
import Control.Applicative ((<$>), (<*>))
import Data.List (isInfixOf, find)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)


-- | showing a simple help
help :: IO ()
help = do 
  putStrLn "This is the unfinished help section :P\n"
  putStrLn "available right now are:"
  putStrLn "help                - showing this text with some information"
  putStrLn "info                - shows the crunched Data about all Anime"
  putStrLn "show                - shows all Anime in the binary"
  putStrLn "add NAME ID         - prompts the user and then adds the Anime to 'other'"
  putStrLn "edit PROPERTY NAME  - lets the user edit all properties of an Anime"
  putStrLn "sort                - sorts all known Anime"
  putStrLn "del NAME            - not yet implemented"
  putStrLn "\nNAME: name of the Anime, PROPERTY: what element you want to edit, [help] for listing possible ones"
  putStrLn "summary: help, show, add, edit NAME, del NAME"
  putStrLn "Note: There is autocompletion, meaning that even with the minumum unique input, \nyou will get the correct result"

-- | creates a new Anime based only on the name
newAnime :: String -> ID -> Anime
newAnime name id = Anime id name NoRating Unknown Unknown


-- | creating a Anime anew completely
completelyNew :: [String] -> IO Anime
completelyNew []   = do
  list <- loadList
  high_id <- highestId
  name <- prompt "New Anime: "
  new_id <- prompt $! "ID [" ++ show (high_id + 1) ++ "] : "
  completelyNew [name, new_id]
completelyNew args = do
  list <- loadList
  high_id <- highestId
  case length args of
    1 -> do
      new_id <- prompt $! "ID [" ++ show (high_id + 1) ++ "] : "
      completelyNew $! args ++ [new_id]
    2 -> let 
          name = args !! 0
          new_id = args !! 1
         in isFree' (if length new_id > 0 then read new_id else high_id + 1) >>= return . (newAnime name)
    n -> case readArgs args of
          (name, Just id) -> completelyNew [name, show id]
          (name, Nothing) -> completelyNew [name]

-- for whatever reason this is sometimes necessary
addNewAnime :: [String] -> IO ()
addNewAnime args = completelyNew args >>= addAnime

-- | returns only the selected one if the name got exactly specified, otherwise changes nothing
isSame :: String -> [Anime] -> [Anime]
isSame n li = if elem n $ map name li then return . fromJust . find (\a -> name a == n) $ li else li


-- | editing the information about an Anime
edit :: [String] -> IO ()
edit []   = prompt "What property do you want to edit? : " >>= (\p -> prompt "What's the name or id of the Anime you want to edit? : " >>= (\n -> edit (p:n:[]) ) )
edit args = do
  case length args of
    1 -> prompt "What's the name or id of the Anime you want to edit? : " >>= (\n -> edit (args ++ [n]))
    2 -> do
      all <- loadList
      selAn <- selectAnime (args !! 1)
      selPr <- selectProperty (args !! 0)
      new <- prompt "Please enter the value of the new property: "
      let edi = editAnime all selAn selPr $! new
      saveComplete $! modifyInList all (name selAn) edi
    _ -> putStrLn "That were too many arguments. Please Try again." >> edit []

-- | selecting an Anime
selectAnime :: String -> IO Anime
selectAnime ""  = prompt "Plese enter at least something about the Anime. : " >>= selectAnime
selectAnime nam = do
  list <- completeList
  let closest = isSame nam $ getClosest list 5 (\a -> isInfixOf nam (name a) )
  let names   = [(getId a, name a) | a <- closest]
  case length closest of
    0 -> testForId nam
    1 -> return $! closest !! 0
    n -> do prompt ("Several found. Please specify name or id. " ++ show names ++ " : ") >>= selectAnime


-- | checking if it actually was an id, otherwise just prompting selectAnime again
testForId :: String -> IO Anime
testForId str =
  let might   = readMaybe str :: Maybe ID in
  case might of
    Nothing    -> prompt "Not found. Search for: " >>= selectAnime
    (Just sth) -> do
      an <- getAnimeWithId sth
      case an of
        Nothing      -> prompt "Not found. Search for: " >>= selectAnime
        (Just anime) -> return anime


-- | all selectable properties
properties :: [String]
properties = ["id", "name", "rating", "epwatched", "eptotal"]


-- | selecting a property
selectProperty :: String -> IO String
selectProperty p =
  case length closest of
    1 -> return $! head closest
    _ -> prompt ("Please enter one of the following properties. " ++ show closest ++ " : " ) >>= selectProperty
  where
  closest = getClosest properties 5 (\a -> isInfixOf p a)


-- | edits the property of the given Anime and returns it with the new property
editAnime :: CompleteCollection -> Anime -> String -> String -> Anime
editAnime co (Anime id nam rat ep wa) prop new =
  case prop of
    "name" -> (Anime id new rat ep wa)
    "rating" -> (Anime id nam (read $ "Rating " ++ new) ep wa)
    "epwatched" -> (Anime id nam rat (readEpisodes new) wa)
    "eptotal" -> (Anime id nam rat ep (readEpisodes new) )
    "id" -> do
      let newId = isFree (read new) co in (Anime newId nam rat ep wa)
    _ -> error "No Property of an Anime"


-- | modifies only the one Anime specified by name
modifyAnime :: AllAnime -> Anime -> String -> AllAnime
modifyAnime (l:li) new nam = if name l == nam then new:li else l:(modifyAnime li new nam)
modifyAnime []      _   _  = []

-- | modifies only the Anime specified by id
modifyAnime' :: AllAnime -> Anime -> ID -> AllAnime
modifyAnime' (l:li) new id = if getId l == id then new:li else l:(modifyAnime' li new id)
modifyAnime' []      _  _  = []

-- | modifies each occurrence of the anime in each of the lists
modifyInList :: CompleteCollection -> String -> Anime -> CompleteCollection
modifyInList (Co wa ne ot) nam an = (Co (mod wa) (mod ne) (mod ot) )
  where mod = (flip . flip modifyAnime) an nam

-- | deletes the given Anime, or asks to select one if none is given
delAnime :: [String] -> IO ()
delAnime args =
  case length args of
    1 -> selectAnime (args !! 0) >>= deleteAnime
    _ -> prompt "Please enter the name or id of the Anime you wish to delete. : " >>= selectAnime >>= deleteAnime

