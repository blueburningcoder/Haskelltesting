-- #!/usr/bin/env runghc -- ... possible with encode/decode-File ?

import Data.Binary

data Genres = Action | Adventure | Comedy | Crime | Fantasy | Fiction | Historical | Horror | Magical 
        | Mystery | Paranoid | Philosophical | Political | Romance | Saga | Satire | ScienceFiction 
        | SliceOfLife | Speculative | Thriller | Urban | Western
        | Strategic | Interesting | Antagonist | Story | Character | Animation
    deriving (Show, Read, Eq)

instance Binary Genres where
    put gen = do put $ show gen
    get = do gen <- get
             return $ read gen

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
instance Show Rating where
    show None = "•not available•"
    show (Rating _ _ o) = show o


data Episodes = Unknown | Zero | Int
    deriving (Show, Read, Eq, Ord)

type WatchedE = Episodes

data Anime = Anime String Rating Episodes WatchedE

type AllAnime = [Anime]
type Watched  = [Anime]
type NWatched = [Anime]

instance Show Anime where
    show (Anime name rat ep wa) = "Anime: " ++ show name ++ " with rating: " ++ show rat ++ " and " ++ show wa ++ "/" ++ show ep ++ " seen."
-- "

{-
loadList :: AllAnime
loadList = decodeFile "anime.txt"
-}

newAnime :: String -> Anime
newAnime name = Anime name None Unknown Unknown


completelyNew :: IO Anime
completelyNew = do 
    putStr "Anime: "
    name <- getLine
    return $ newAnime name

