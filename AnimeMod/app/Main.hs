module Main where

import Anime.Types
import Anime.Files
import Data.Binary (Binary (..), Get, encodeFile, decodeFile)
import Data.Char   (ord)
import Control.Applicative ((<$>), (<*>))
import System.IO (hFlush, stdout, IO (..))
import System.Environment (getArgs, withArgs)


{-
main' = do
    putStrLn "What do you want to do?\n1 - Show All Anime\n2 - Add Anime\n3 - Delete Anime"
    d <- (getChar >>= (\c -> return $! ord c - ord '0'))
    getChar >> putChar '\b' 
    case d of
        1 -> showAnimeList
        2 -> (:) <$> completelyNew <*> (reverse <$> (loadList >>= return . other)) >>= (\n -> saveOther $! reverse n)
        3 -> undefined

-}

-- the main function
main = do
    args <- getArgs
    case head args of
        "show" -> showAnimeList
        "add" -> (:) <$> completelyNew <*> (reverse <$> (loadList >>= return . other)) >>= (\n -> saveOther $! reverse n)
        "del" -> putStr "Hi there" >> putStr (take 20 $ repeat '\b')
        "help" -> help
        _ -> putStrLn "This feature is not yet implemented ._."

-- showing a simple help
help :: IO ()
help = do 
    putStrLn "This is the unfinished help section :P\n"
    putStrLn "available right now are:"
    putStrLn "help      - showing this text with some information"
    putStrLn "show      - shows all anime in the database"
    putStrLn "add       - prompts the user and then adds the anime to 'other'"
    putStrLn "del NAME  - not yet implemented"
    putStrLn "edit NAME - not yet implemented"
    putStrLn "summary: help, show, add, del NAME, edit NAME"


-- small helper function requesting a string
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

-- creates a new Anime based only on the name
newAnime :: String -> Anime
newAnime name = Anime name None Unknown Unknown

-- creating a anime anew completely
completelyNew :: IO Anime
completelyNew = do
    name <- prompt "New Anime: "
    return $ newAnime name



-- some test-information to play with
testAllAnime = [ newAnime "Gantz", newAnime "Zetsuen no Tempest" ]
estComplete = Co [] [] testAllAnime
