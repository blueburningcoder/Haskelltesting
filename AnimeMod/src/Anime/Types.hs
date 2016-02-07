module Anime.Types where


import Data.Binary (Binary (..), Get, encodeFile, decodeFile)


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

