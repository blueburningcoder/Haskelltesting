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
addAnime = (:) <$> completelyNew <*> (reverse <$> (loadList >>= return . other)) >>= (\n -> saveOther $! reverse n) >> putStrLn "Saved"

-- creates a new Anime based only on the name
newAnime :: String -> Anime
newAnime name = Anime name None Unknown Unknown

-- creating a anime anew completely
completelyNew :: IO Anime
completelyNew = do
    name <- prompt "New Anime: "
    return $ newAnime name


-- only cares about the names of the Anime
toNames :: [Anime] -> [String]
toNames [] = []
toNames (a:li) = (name a):(toNames li)

-- returns the n first matches of a string within the list of strings
getClosest :: [String] -> Int -> String -> [String]
getClosest [] _ _ = []
getClosest _ 0 _ = []
getClosest (l:li) n a = if isInfixOf a l then l:(getClosest li (n-1) a) else getClosest li n a


getAll :: IO [Anime]
getAll = loadList >>= return . concat . (\an -> [l an | l <- [other, next, watched]])



listClosest :: String -> IO [String]
listClosest name = loadList >>= return . (\list -> concat [(flip . flip getClosest) 5 name . toNames . l $ list | l <- [other, next, watched]])


edit :: [String] -> IO ()
edit [] = prompt "What property do you want to edit? " >>= (\p -> prompt "What's the name of the Anime you want to edit? " >>= (\n -> edit (p:n:[]) ) )
edit args = do
    case length args of
        1 -> prompt "What's the name of the Anime you want to edit? " >>= (\n -> edit (args ++ [n]))
        2 -> select (args !! 1) >>= (\a -> putStrLn $ "Selected: " ++ (show . name $ a))
        _ -> putStrLn "That were too many arguments. Please Try again." >> edit []


select :: String -> IO Anime
select nam = do
    closest <- listClosest nam
    all <- getAll
    case length closest of 
        0 -> do prompt "Not found. Search for: " >>= select
        1 -> return $ (filter (\a -> name a == (head closest)) all) !! 0
        n -> do prompt ("Several found. Please specify. " ++ show closest ++ " : ") >>= select


