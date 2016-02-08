module Anime.Menu where

import Anime.Types
import Anime.Files
import System.IO (hFlush, stdout)
import Control.Applicative ((<$>), (<*>))
import Data.List (isInfixOf)


-- showing a simple help
help :: IO ()
help = do 
    putStrLn "This is the unfinished help section :P\n"
    putStrLn "available right now are:"
    putStrLn "help                  - showing this text with some information"
    putStrLn "show                  - shows all anime in the database"
    putStrLn "add                   - prompts the user and then adds the anime to 'other'"
    putStrLn "del NAME              - not yet implemented"
    putStrLn "edit PROPERTY NAME    - currently getting implemented"
    putStrLn "  NAME: name of the Anime, PROPERTY: what element you want to edit, [help] for listing possible ones"
    putStrLn "summary: help, show, add, del NAME, edit NAME"


-- small helper function requesting a string
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine


-- adding an anime to the saved binary
addAnime :: IO ()
addAnime = (:) <$> completelyNew <*> (reverse <$> (loadList >>= return . other)) >>= (\n -> saveOther $! reverse n)


-- creates a new Anime based only on the name
newAnime :: String -> Anime
newAnime name = Anime name None Unknown Unknown


-- creating a anime anew completely
completelyNew :: IO Anime
completelyNew = do
    name <- prompt "New Anime: "
    return $! newAnime name


-- only cares about the names of the Anime
toNames :: [Anime] -> [String]
toNames [] = []
toNames (a:li) = (name a):(toNames li)


-- returns the n first matches of a string within the list of strings
getClosest :: [String] -> Int -> String -> [String]
getClosest [] _ _ = []
getClosest _ 0 _ = []
getClosest (l:li) n a = if isInfixOf a l then l:(getClosest li (n-1) a) else getClosest li n a


-- returns a list of all Anime known
getAll :: IO [Anime]
getAll = loadList >>= return . concat . (\an -> [l an | l <- [other, next, watched]])


-- listing the names of the closest Anime to the given String
listClosest :: String -> IO [String]
listClosest name = loadList >>= return . (\list -> concat [(flip . flip getClosest) 3 name . toNames . l $ list | l <- [other, next, watched]])


-- editing the information about an Anime
edit :: [String] -> IO ()
edit [] = prompt "What property do you want to edit? " >>= (\p -> prompt "What's the name of the Anime you want to edit? " >>= (\n -> edit (p:n:[]) ) )
edit args = do
    case length args of
        1 -> prompt "What's the name of the Anime you want to edit? " >>= (\n -> edit (args ++ [n]))
        -- 2 -> selectAnime (args !! 1) >>= (\a -> selectProperty (args !! 0) >>= (\p -> prompt "Please enter the value of the new property: " >>= return . (\v -> editAnime a p v) ) ) >> return ()
        2 -> do
            all <- loadList
            selAn <- selectAnime (args !! 1)
            selPr <- selectProperty (args !! 0)
            new <- prompt "Please enter the value of the new property: "
            let edi = editAnime selAn selPr $! new
            saveComplete $! modifyInList all (name selAn) edi
        _ -> putStrLn "That were too many arguments. Please Try again." >> edit []

-- selecting an anime
selectAnime :: String -> IO Anime
selectAnime nam = do
    closest <- listClosest nam
    all <- getAll
    case length closest of 
        0 -> do prompt "Not found. Search for: " >>= selectAnime
        1 -> return $! (filter (\a -> name a == (head closest)) all) !! 0
        n -> do prompt ("Several found. Please specify. " ++ show closest ++ " : ") >>= selectAnime


-- all selectable properties
properties :: [String]
properties = ["name", "rating", "epwatched", "eptotal"]


-- selecting a property
selectProperty :: String -> IO String
selectProperty p =
    case length closest of
        1 -> return $! head closest
        _ -> prompt "Please enter one of the following properties. [name|rating|epwatched|eptotal] : " >>= selectProperty
    where
    closest = getClosest properties 3 p


-- returns the number of Episodes
getNumber :: IO Episodes
getNumber = prompt "Please enter the number of Episodes" >>= return . read


-- edits the property of the given Anime and returns it with the new property
editAnime :: Anime -> String -> String -> Anime
editAnime (Anime nam rat ep wa) prop new =
    case prop of
        "name" -> (Anime new rat ep wa)
        "rating" -> (Anime nam (read $ "Rating " ++ new) ep wa)
        "epwatched" -> (Anime nam rat (read $ "Episodes " ++ new) wa)
        "eptotal" -> (Anime nam rat ep (read $ "Episodes " ++ new) )
        _ -> error "No Property of an Anime"


-- modifies only one specific Anime
modifyAnime :: AllAnime -> Anime -> String -> AllAnime
modifyAnime [] _ _ = []
modifyAnime (l:li) new nam = if name l == nam then new:li else l:(modifyAnime li new nam)


-- modifies each occurrence of the anime in each of the lists
modifyInList :: CompleteCollection -> String -> Anime -> CompleteCollection
modifyInList (Co wa ne ot) nam an = (Co (mod wa) (mod ne) (mod ot) )
    where mod = (flip . flip modifyAnime) an nam

