module Anime.Menu where

import Anime.Types
import Anime.Files
import System.IO (hFlush, stdout)
import Control.Applicative ((<$>), (<*>))
import Data.List (isInfixOf, find)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)


-- showing a simple help
help :: IO ()
help = do 
  putStrLn "This is the unfinished help section :P\n"
  putStrLn "available right now are:"
  putStrLn "help                - showing this text with some information"
  putStrLn "show                - shows all Anime in the binary"
  putStrLn "add                 - prompts the user and then adds the Anime to 'other'"
  putStrLn "edit PROPERTY NAME  - lets the user edit all properties of an Anime"
  putStrLn "sort                - sorts all known Anime"
  putStrLn "del NAME            - not yet implemented"
  putStrLn "\nNAME: name of the Anime, PROPERTY: what element you want to edit, [help] for listing possible ones"
  putStrLn "summary: help, show, add, edit NAME, del NAME"


-- small helper function requesting a string
prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine


-- adding an anime to the saved binary
addAnime :: IO ()
addAnime = do
  new <- completelyNew
  oth <- (loadList >>= return . other)
  saveOther $! addToEnd oth new


-- adds something at the end of the list
addToEnd :: [a] -> a -> [a]
addToEnd (l:li) a = l:(addToEnd li a)
addToEnd [] a = [a]

-- creates a new Anime based only on the name
newAnime :: String -> ID -> Anime
newAnime name id = Anime id name None Unknown Unknown

-- creating a anime anew completely
completelyNew :: IO Anime
completelyNew = do
  name <- prompt "New Anime: "
  high_id <- highestId
  new_id <- prompt $ "ID [" ++ show (high_id + 1) ++ "] : "
  return $ newAnime name ( if length new_id > 0 then read new_id else high_id + 1)


isFree :: ID -> IO ID
isFree id = undefined

-- returns the n first matches of a partwise Element within the list of Elements
getClosest :: [a] -> Int -> (a -> Bool) -> [a]
getClosest [] _ _ = []
getClosest _ 0 _ = []
getClosest (l:li) n p = if p l then l:(getClosest li (n-1) p) else getClosest li n p

-- returns only the selected one if the name got exactly specified, otherwise changes nothing
isSame :: String -> [Anime] -> [Anime]
isSame n li = if elem n $ map name li then return . fromJust . find (\a -> name a == n) $ li else li


-- editing the information about an Anime
edit :: [String] -> IO ()
edit [] = prompt "What property do you want to edit? " >>= (\p -> prompt "What's the name of the Anime you want to edit? " >>= (\n -> edit (p:n:[]) ) )
edit args = do
  case length args of
    1 -> prompt "What's the name or id of the Anime you want to edit? " >>= (\n -> edit (args ++ [n]))
    -- 2 -> selectAnime (args !! 1) >>= (\a -> selectProperty (args !! 0) >>= (\p -> prompt "Please enter the value of the new property: " >>= return . (\v -> editAnime a p v) ) ) >> return ()
    2 -> do
      all <- loadList
      selAn <- selectAnime (args !! 1)
      selPr <- selectProperty (args !! 0)
      new <- prompt "Please enter the value of the new property: "
      let edi = editAnime selAn selPr $! new
      saveComplete $! modifyInList all (name selAn) edi
    _ -> putStrLn "That were too many arguments. Please Try again." >> edit []


-- selecting an Anime
selectAnime :: String -> IO Anime
selectAnime nam = do
  list <- completeList
  let closest = isSame nam $ getClosest list 5 (\a -> isInfixOf nam (name a) )
  let names = [(getId a, name a) | a <- closest]
  case length closest of
    0 -> testForId nam
    1 -> return $! closest !! 0
    n -> do prompt ("Several found. Please specify name or id. " ++ show names ++ " : ") >>= selectAnime

-- checking if it actually was an id
testForId :: String -> IO Anime
testForId str =
  let might = readMaybe str :: Maybe ID in
  case might of
    Nothing -> prompt "Not found. Search for: " >>= selectAnime
    (Just sth) -> do
      an <- getAnimeWithId sth
      case an of
        Nothing -> prompt "Not found. Search for: " >>= selectAnime
        (Just anim) -> return anim

-- all selectable properties
properties :: [String]
properties = ["id", "name", "rating", "epwatched", "eptotal"]


-- selecting a property
selectProperty :: String -> IO String
selectProperty p =
  case length closest of
    1 -> return $! head closest
    _ -> prompt ("Please enter one of the following properties. " ++ show closest ++ " : " ) >>= selectProperty
  where
  closest = getClosest properties 5 (\a -> isInfixOf p a)


-- edits the property of the given Anime and returns it with the new property
editAnime :: Anime -> String -> String -> Anime
editAnime (Anime id nam rat ep wa) prop new =
  case prop of
    "name" -> (Anime id new rat ep wa)
    "rating" -> (Anime id nam (read $ "Rating " ++ new) ep wa)
    "epwatched" -> (Anime id nam rat (read $ "Episodes " ++ new) wa)
    "eptotal" -> (Anime id nam rat ep (read $ "Episodes " ++ new) )
    "id" -> (Anime (read new) nam rat ep wa)
    _ -> error "No Property of an Anime"


-- modifies only one specific Anime
modifyAnime :: AllAnime -> Anime -> String -> AllAnime
modifyAnime [] _ _ = []
modifyAnime (l:li) new nam = if name l == nam then new:li else l:(modifyAnime li new nam)


-- modifies each occurrence of the anime in each of the lists
modifyInList :: CompleteCollection -> String -> Anime -> CompleteCollection
modifyInList (Co wa ne ot) nam an = (Co (mod wa) (mod ne) (mod ot) )
  where mod = (flip . flip modifyAnime) an nam

sort :: IO ()
sort = undefined
