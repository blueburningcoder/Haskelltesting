module Anime.Types.Old where

import           General
import           Data.Binary  (Binary (..), Get, encodeFile, decodeFile)
import           Data.Maybe   (fromJust)
import           Data.List    (find)
import           Debug.Trace  (trace)

-- Planned:
--  import           Data.Vector
--  import           Data.Text
-- but requires bigger changes throughout several files (28 functions total at this point)


{-
-- | All kinda genres that an anime might or might not have
data Genres = Action | Adventure | Comedy | Crime | Fantasy | Fiction | Historical | Horror | Magical 
    | Mystery | Paranoid | Philosophical | Political | Romance | Saga | Satire | ScienceFiction 
    | SliceOfLife | Speculative | Thriller | Urban | Western
    | Strategic | Interesting | Antagonist | Story | Character | Animation
  deriving (Show, Read, Eq)

instance Binary Genres where
  put gen = do put $ show gen
  get = do gen <- get
    return $ read gen

-- | A Rating usually consists of a list of genres and a list of their individual scores, 1-10 respectively
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

-- | currently only showing the overall Rating if necessary
instance Show Rating where
  show None = "•not available•"
  show (Rating _ _ o) = show o
-}

-- | a Rating is either nothing or some double value
data Rating = NoRating | Rating Double
  deriving (Read, Eq, Ord)

instance Show Rating where
  show NoRating     = "_._"
  show (Rating rat) = show rat

instance Binary Rating where
  put NoRating = do put (0 :: Int)
  put (Rating r) =
    do put (1 :: Int)
       put r
  get = do t <- get :: Get Int
           case t of
            0 -> return NoRating
            1 -> do r <- get; return (Rating r)

-- | The number of Episodes is either Unknown, Zero (yet), or some Int
data Episodes = Unknown | Zero | Episodes Int
  deriving (Read, Eq, Ord)

instance Enum Episodes where
  succ Unknown      = Zero
  succ Zero         = Episodes 1
  succ (Episodes n) = Episodes $ n + 1
  pred Unknown      = Unknown
  pred Zero         = Unknown
  pred (Episodes 1) = Zero
  pred (Episodes n) = Episodes $ n - 1
  toEnum num        = pred . succ $ Episodes num
  fromEnum Zero     = 0
  fromEnum Unknown  = undefined
  fromEnum (Episodes n) = n

instance Show Episodes where
  show Unknown        = "Unknown"
  show Zero           = "Zero"
  show (Episodes num) = show num

-- | reads the String and turns it into an Episode
readEpisodes :: String -> Episodes
readEpisodes "Unknown" = Unknown
readEpisodes "Zero"    = Zero
readEpisodes "0"       = Zero
readEpisodes str       = case maybeRead $ "Episodes " ++ str of
  Just ep -> ep
  Nothing -> Unknown


instance Binary Episodes where
  put Unknown        = put (-1 :: Int)
  put Zero           = put  (0 :: Int)
  put (Episodes num) = put  (1 :: Int) >> put num

  get = do t <- get :: Get Int
           case t of
            (-1) -> return Unknown
            0 -> return Zero
            1 -> do num <- get; return (Episodes num)

type ID   = Int
type Name = String

-- | an anime usually consists of the name, the rating, the episodes and the episodes watched
data Anime = Anime { getId :: ID, name :: Name, rating :: Rating, watchedEp :: Episodes, totalEp :: Episodes }

instance Eq Anime where
  (Anime id _ _ _ _) == (Anime iD _ _ _ _) = id == iD

instance Ord Anime where
  compare (Anime id _ _ _ _) (Anime iD _ _ _ _) = compare id iD

instance Binary Anime where
  put (Anime id nam rat ep wa) = do
    put id
    put nam
    put rat
    put ep
    put wa
  get = do
    id  <- get
    nam <- get
    rat <- get
    ep  <- get
    wa  <- get
    return $ Anime id nam rat ep wa

instance Show Anime where
  show (Anime id name rat wa ep) = "Anime with id " ++ show id ++ " is " ++ show name ++ " with Rating " ++ show rat ++ " and " ++ show wa ++ "/" ++ show ep ++ " seen.\n"

listAnime :: Anime -> String
listAnime (Anime id name rat waep toep) = begin ++ (concat . take (100 - length begin) . repeat $ " ") ++ " - (" ++ show rat ++ ")"
  where begin = name ++ "; (" ++ show waep ++ "/" ++ show toep ++ ")"

-- | returns only the selected one if the name got exactly specified, otherwise changes nothing
isSameAnime :: String -> [Anime] -> [Anime]
isSameAnime n li = isSame' n name li

type AllAnime = [Anime]

type WatchedAnime = AllAnime
type NextAnime    = AllAnime
type OtherAnime   = AllAnime

-- | the complete Collection of Anime including those watched, those who are going to be watched next and the others, from which only the name might be known.
data CompleteCollection = Co { watched :: WatchedAnime, next :: NextAnime, other :: OtherAnime }
  deriving (Eq)

instance Show CompleteCollection where
  show (Co wa ne ot) = "---- ---- Watched:\n" ++ show wa ++ "\n\n---- ---- Next:\n" ++ show ne ++ "\n\n---- ---- Other known ones:\n" ++ show ot

-- putting all Anime in a Human-readable form
listReadable :: CompleteCollection -> String
listReadable (Co wa ne ot) = unlines . map listAnime $ (wa ++ ne ++ ot)

instance Binary CompleteCollection where
  put (Co wa ne ot) = do put wa; put ne; put ot
  get = do
    wa <- get
    ne <- get
    ot <- get
    return $ (Co wa ne ot)

