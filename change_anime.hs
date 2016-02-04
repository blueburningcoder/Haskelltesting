-- #!/usr/bin/env runghc -- ... possible with encode/decode-File ?

import Data.Binary

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

data Anime = Anime String Rating Episodes WatchedE

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



type AllAnime = [Anime]
type Watched  = [Anime]
type NWatched = [Anime]

instance Show Anime where
    show (Anime name rat ep wa) = "Anime: " ++ show name ++ " with rating: " ++ show rat ++ " and " ++ show wa ++ "/" ++ show ep ++ " seen."
-- "

saveList :: AllAnime -> IO ()
saveList list = encodeFile "anime.txt" list

loadList :: IO AllAnime
loadList = decodeFile "anime.txt"

newAnime :: String -> Anime
newAnime name = Anime name None Unknown Unknown


completelyNew :: IO Anime
completelyNew = do 
    putStr "Anime: "
    name <- getLine
    return $ newAnime name

