-- | This module includes basically the functional main menu, it is dependent upon most other modules

module Anime.Menu.Old where

import General
import Anime.Types.Old
import Anime.Files.Old

import Control.Applicative ((<$>), (<*>))
import Data.List (isInfixOf)
import Text.Read (readMaybe)


-- | Showing a simple help
help :: IO ()
help = do 
  putStrLn "This is the unfinished help section :P\n"
  putStrLn "available right now are:"
  putStrLn "help                            - showing this text with some information"
  putStrLn "show [-human]                   - shows all Anime in the binary, '-human': might be more 'human' readable"
  putStrLn "add  [NAME] [ID]                - prompts the user and then adds the Anime to 'other'"
  putStrLn "edit [PROPERTY] [NAME/ID] [NEW] - lets the user edit all properties of an Anime"
  putStrLn "seen [NUM/next/all] [NAME/ID]   - adds the number [of episodes seen] to the specified Anime"
  putStrLn "del  [NAME/ID]                  - deletes the uniquely identifyable Anime"
  putStrLn "sort [-order]                   - sorts all known Anime, '-order': ordering the id's"
  putStrLn "info                            - shows the crunched Data about all Anime"
  putStrLn "\nsummary: help, show, add, edit, seen, del, sort, info"
  putStrLn "arguments can be put behind the command itself, and will be requested if required."
  putStrLn "Note: There is autocompletion, meaning that even with the minumum unique input,"
  putStrLn "you will get the correct result (commands excluded)."

-- | Creates a new Anime based only on the name
newAnime :: String -> ID -> Anime
newAnime name id = Anime id name NoRating Unknown Unknown


-- | Creating a Anime anew completely
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

-- | For whatever reason this is sometimes necessary
addNewAnime :: [String] -> IO ()
addNewAnime args = completelyNew args >>= addAnime

-- | Editing the information about an Anime
edit :: [String] -> IO ()
edit []   = prompt "What property do you want to edit? : " >>= selectProperty >>= (\p -> edit [p])
edit args = do
  case length args of
    1 -> prompt "What's the name or id of the Anime you want to edit? : " >>= selectAnime >>= (\n -> edit $ args ++ [show $ getId n])
    2 -> prompt "Please enter the value of the new property: " >>= (\new -> edit $ args ++ [new])
    3 -> do
      all <- loadList
      selPr <- selectProperty (args !! 0)
      selAn <- selectAnime (args !! 1)
      let new = args !! 2; edi = editAnime all selAn selPr $! new
      saveComplete $! modifyInList all (name selAn) edi
    _ -> putStrLn "That were too many arguments. Please Try again." >> edit []

-- | All selectable properties
properties :: [String]
properties = ["id", "name", "rating", "epwatched", "eptotal"]


-- | Selecting a property
selectProperty :: String -> IO String
selectProperty p =
  case length closest of
    1 -> return $! head closest
    _ -> prompt ("Please enter one of the following properties. " ++ show closest ++ " : " ) >>= selectProperty
  where
  closest = getClosest properties 5 (\a -> isInfixOf p a)


-- | Edits the property of the given Anime and returns it with the new property
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


-- | Modifies only the one Anime specified by name
modifyAnime :: AllAnime -> Anime -> String -> AllAnime
modifyAnime (l:li) new nam = if name l == nam then new:li else l:(modifyAnime li new nam)
modifyAnime []      _   _  = []

-- | Modifies only the Anime specified by id
modifyAnime' :: AllAnime -> Anime -> ID -> AllAnime
modifyAnime' (l:li) new id = if getId l == id then new:li else l:(modifyAnime' li new id)
modifyAnime' []      _  _  = []

-- | Modifies each occurrence of the anime in each of the lists
modifyInList :: CompleteCollection -> String -> Anime -> CompleteCollection
modifyInList (Co wa ne ot) nam an = (Co (mod wa) (mod ne) (mod ot) )
  where mod = (flip . flip modifyAnime) an nam

-- | Same as modifyInList but with the ID instead of the Name as an argument
modifyInList' :: CompleteCollection -> ID -> Anime -> CompleteCollection
modifyInList' (Co wa ne ot) id an = (Co (mod wa) (mod ne) (mod ot) )
  where mod = (flip . flip modifyAnime') an id

-- | Deletes the given Anime, or asks to select one if none is given
delAnime :: [String] -> IO ()
delAnime args =
  case length args of
    1 -> selectAnime (args !! 0) >>= deleteAnime
    _ -> prompt "Please enter the name or id of the Anime you wish to delete. : " >>= selectAnime >>= deleteAnime

-- | Fast helper for editing
seen :: [String] -> IO ()
seen []   = prompt "How many Episodes is the new count? : " >>= (\u -> seen [u])
seen args =
  case length args of
    1 -> prompt "Please enter the name or id of the Anime you wish to update. : " >>= selectAnime >>= (\a -> seen $ args ++ [show $ getId a])
    2 -> let animF = determineNum $ args !! 0 in do
        anime <- selectAnime $ args !! 1
        loadList >>= saveComplete . (\li -> modifyInList' li (getId anime) (animF anime) )
    n -> seen [head args, fst . readArgs . tail $ args]

-- | Basically tries to read the the argument
determineNum :: String -> (Anime -> Anime)
determineNum "next" = (\ (Anime id nam rat epwa epto) -> Anime id nam rat (succ epwa)        epto)
determineNum "all"  = (\ (Anime id nam rat epwa epto) -> Anime id nam rat epto               epto)
determineNum str    = (\ (Anime id nam rat epwa epto) -> Anime id nam rat (readEpisodes str) epto)


