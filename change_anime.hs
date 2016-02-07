#!/usr/bin/env runghc

module Anime where


import Data.Binary (Binary (..), Get, encodeFile, decodeFile)
import Data.Char   (ord)
import Control.Applicative ((<$>), (<*>))
import System.IO (hFlush, stdout, IO (..))
import System.Environment (getArgs, withArgs)



-- All kinda genres that an anime might or might not have
data Genres = Action | Adventure | Comedy | Crime | Fantasy | Fiction | Historical | Horror | Magical 
        | Mystery | Paranoid | Philosophical | Political | Romance | Saga | Satire | ScienceFiction 
        | SliceOfLife | Speculative | Thriller | Urban | Western
        | Strategic | Interesting | Antagonist | Story | Character | Animation
    deriving (Show, Read, Eq)

instance Binary Genres where
    put gen = do put $ show gen
    get = do gen <- get
             return $ read gen

-- A Rating usually consists of a list of genres and a list of their individual scores, 1-10 respectively
data Rating = None | Rating [Genres] [Int] Double
    deriving (Read, Eq)

instance Binary Rating where
    put None = do put (0 :: Int)
    put (Rating gen rat ov) = do
        put (1 :: Int)
        put gen
        put rat
        put ov

    get = do t <- get :: Get Int
             case t of
                0 -> return None
                1 -> do
                gen <- get
                rat <- get
                ov  <- get
                return (Rating gen rat ov)

-- currnetly only showing the overall Rating if necessary
instance Show Rating where
    show None = "•not available•"
    show (Rating _ _ o) = show o

-- The number of Episodes is either Unknown, Zero (yet), or some Int
data Episodes = Unknown | Zero | Int
    deriving (Show, Read, Eq, Ord)

instance Binary Episodes where
    put Unknown = put (-1 :: Int)
    put Zero    = put (0 :: Int)
    put num     = put (1 :: Int) >> put num

    get = do t <- get :: Get Int
             case t of
              (-1) -> return Unknown
              0 -> return Zero
              1 -> do num <- get; return num

type WatchedE = Episodes

-- an anime usually consists of the name, the rating, the episodes and the episodes watched
data Anime = Anime String Rating Episodes WatchedE
    deriving (Eq)

instance Binary Anime where
    put (Anime nam rat ep wa) = do
        put nam
        put rat
        put ep
        put wa
    get = do
        nam <- get
        rat <- get
        ep <- get
        wa <- get
        return $ Anime nam rat ep wa

instance Show Anime where
    show (Anime name rat ep wa) = "Anime: " ++ show name ++ " with rating: " ++ show rat ++ " and " ++ show wa ++ "/" ++ show ep ++ " seen.\n"
 

type AllAnime = [Anime]

type WatchedAnime = AllAnime
type NextAnime    = AllAnime
type OtherAnime   = AllAnime

data CompleteCollection = Co { watched :: WatchedAnime, next :: NextAnime, other :: OtherAnime }
    deriving (Eq)

instance Show CompleteCollection where
    show (Co wa ne ot) = "---- ---- Watched:\n" ++ show wa ++ "\n\n---- ---- Next:\n" ++ show ne ++ "\n\n---- ---- Other known ones:\n" ++ show ot

instance Binary CompleteCollection where
    put (Co wa ne ot) = do put wa; put ne; put ot
    get = do
        wa <- get
        ne <- get
        ot <- get
        return $ (Co wa ne ot)

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
