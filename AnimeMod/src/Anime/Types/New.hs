-- | The New Types, where they are hopefully good enough to not get reinvented again

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

module Anime.Types.New where

import           Prelude
import           General

import           Data.Binary
import           Data.Vector.Binary()
import           Data.Monoid hiding (All)
import           Data.List    (sort, nub)
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Text as T

import           Debug.Trace


-- | A Rating is either nothing or some rating from 0.0 up to 10.0
--
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
            1 -> do r <- get; return (Rating r)
            _ -> return NoRating



-- | The number of Episodes is either Unknown, Zero (yet), or some Int
--
data Episodes = Unknown | Zero | Episodes Int
  deriving (Eq, Ord, Read)


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

-- | Reads the String and turns it into an Episode
--
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
            1 -> do num <- get; return (Episodes num)
            0 -> return Zero
            _ -> return Unknown



-- | The Category of an Anime, being either only beknown, intended to be seen at some point, 
--   currently getting viewed or having been watched already
--
data Category = Other | Next | Current | Watched | All
  deriving (Show, Read, Eq, Ord)

instance Enum Category where
  succ = undefined
  pred = undefined
  fromEnum Other   = 0
  fromEnum Next    = 1
  fromEnum Current = 2
  fromEnum Watched = 3
  fromEnum All     = 4
  toEnum 0 = Other
  toEnum 1 = Next
  toEnum 2 = Current
  toEnum 3 = Watched
  toEnum 4 = All
  toEnum n = toEnum $ n `mod` 5

instance Binary Category where
  put Other   = put (0 :: Int)
  put Next    = put (1 :: Int)
  put Current = put (2 :: Int)
  put Watched = put (3 :: Int)
  get = do t <- get :: Get Int
           case t of
            3 -> return Watched
            2 -> return Current
            1 -> return Next
            _ -> return Other


-- | Every ID is an Int
type ID   = Int
-- | The Anime's name is not a String, but a Text this time
type Name = T.Text

-- | An Anime usually consists of the name, the rating, the episodes watched and total,
--   as well as the place of whatever category it is in
--
data Anime = Anime { uniqueId  :: {-# UNPACK #-} !ID
                   , name      :: {-# UNPACK #-} !Name
                   , rating    ::                !Rating
                   , watchedEp ::                !Episodes
                   , totalEp   ::                !Episodes
                   , place     :: {-# UNPACK #-} !ID }

-- | The singleton Anime
--
singleton :: Anime
singleton = Anime 0 "" NoRating Unknown Unknown 0


-- | Giving the Anime a new Name
newName    :: Anime -> Name     -> Anime
newName    (Anime u _ r w t p) i = Anime u i r w t p

-- | Giving the Anime a new Rating
newRating  :: Anime -> Rating   -> Anime
newRating  (Anime u n _ w t p) i = Anime u n i w t p

-- | Giving the Anime a new number of Watched Episodes
newWatched :: Anime -> Episodes -> Anime
newWatched (Anime u n r _ t p) i = Anime u n r i t p

-- | Giving the Anime a new number of Total Episodes
newTotal   :: Anime -> Episodes -> Anime
newTotal   (Anime u n r w _ p) i = Anime u n r w i p

-- | Giving the Anime a new Place in it's category
newPlace   :: Anime -> ID       -> Anime
newPlace   (Anime u n r w t _) i = Anime u n r w t i


instance Eq Anime where
   a == b = uniqueId a == uniqueId b

instance Ord Anime where
  compare a b = compare (place a) (place b)

instance Binary Anime where
  put (Anime uid nam rat wa to pl) = do
    put uid
    put nam
    put rat
    put wa
    put to
    put pl
  get = do
    uid <- get
    nam <- get
    rat <- get
    wa  <- get
    to  <- get
    pl  <- get
    return $ Anime uid nam rat wa to pl

instance Show Anime where
  show (Anime uid nam rat wa to _) = "Anime with uid " ++ show uid ++ " is " ++ show nam ++ " with Rating " ++ show rat ++ " and " ++ show wa ++ "/" ++ show to ++ " seen.\n"


-- | Listing the Anime in a more easily readable form showing only necessary information
--
listAnime :: Anime -> String
listAnime (Anime _ nam rat wa to _) = begin ++ (concat . take (100 - length begin) . repeat $ " ") ++ " - (" ++ show rat ++ ")"
  where begin = T.unpack nam ++ "; (" ++ show wa ++ "/" ++ show to ++ ")"


{-
 - -- for some strange reason this version does not at all work
listAnime' :: Anime -> T.Text
listAnime' (Anime id nam rat wa to _) =
    begin `T.append` (T.take (100 - T.length begin) . T.pack . repeat $ ' ') `T.append` " - (" `T.append` sho rat `T.append` ")"
  where sho s = T.pack . show $ s
        begin = nam `T.append` "; (" `T.append` sho wa `T.append` "/" `T.append` sho to `T.append` ")"
-}

-- | Listing Anime more readable in a Text instead of a String
--
listAnime' :: Anime -> T.Text
listAnime' = T.pack . listAnime


-- | Returns only the selected one if the name got exactly specified, otherwise changes nothing
--
isSameAnime :: T.Text -> [Anime] -> [Anime]
isSameAnime n li = isSame' n name li



-- | This is supposed to make inserting new Anime in a Collection easier and safer
--
class Insertable a where
  insert :: Anime -> a -> a
  insertAll :: F.Foldable t => t Anime -> a -> a
  insertAll t a = F.foldr insert a t



-- | This is basically a list of Anime, although, a Vector is more efficient
type AllAnime = V.Vector Anime


-- | Collection types : Other, Next, Current, Watched
--
data Collection =
  Col { cat    ::                !Category
      , cMaxId :: {-# UNPACK #-} !ID
      , list   :: {-# UNPACK #-} !AllAnime }


-- | The size of the Collection
--
size :: Collection -> Int
size (Col _ _ li) = V.length li


-- | Returns if this Collection has the Anime with this particular ID
--
hasId :: Collection -> ID -> Bool
hasId (Col _ _ li) i = V.any (\a -> uniqueId a == i) li


-- | Returns if this Collection has this particular Anime or not
--
cElem :: Collection -> Anime -> Bool
cElem c a = hasId c (uniqueId a)

-- | simply creates the required Collection
--
createCollection :: Category -> Collection
createCollection c = Col c 0 V.empty


instance Show Collection where
  show (Col n _ l) = "---- ---- (" ++ show (V.length l) ++ ") ---- " ++ show n ++ ":\n" ++ (V.foldr (\a b -> b ++ show a) "" l)

instance Monoid Collection where
  mempty = Col Other 0 mempty
  mappend a (Col _ _ lj) = insertAll lj a

instance Insertable Collection where
  insert a b@(Col m ma li) =
    if uniqueId a > ma then Col m (uniqueId a) (V.cons (newPlace a (size b) ) li) else if cElem b a then b else Col m ma (V.cons (newPlace a (size b) ) li)

instance Binary Collection where
  put (Col c m l) = do
    put c
    put m
    put l
  get = do
    c <- get
    m <- get
    l <- get
    return $ Col c m l



-- | The complete Collection of Anime including those watched, those who are going to be watched next and the others, 
-- from which only the name might be known. (work in progress)
--
data CompleteCollection a =
  Com { table :: V.Vector a, maxId :: ID}


-- | just a type synonym, making things easier to read and write
-- CompleteCollection is basically only used with Collection either way
--
type Complete = CompleteCollection Collection


-- | The Complete singleton, or empty completeCollection
--
singletonCC :: Complete
singletonCC = Com V.empty 0


-- | Returns only the Collection with the specified Category
--
getWithCat :: Category -> Complete -> Collection
getWithCat c (Com ta _) = if V.length filt > 1 then V.foldr1 mappend ta else
    if V.length filt == 1 then filt V.! 0 else createCollection c
  where filt = V.filter (\d -> cat d == c) ta


-- | Inserts a Collection in the CompleteCollection, manages correct replacing if required
--
insertCol :: Complete -> Collection -> Complete
insertCol (Com ta m) c = Com (V.cons c ta) (max (cMaxId c) m)


-- | Merges the same categories together, if required            TODO!
--
mergeSameCategories :: Complete -> Complete
mergeSameCategories c@(Com ta _) = (trace . show $ cats) $
    case length cats of
      0 -> c
      _ -> foldl insertCol singletonCC [getWithCat ca c | ca <- getDifferentCategories c]
  where cats = getDoubles . sort . V.toList $ V.map cat ta


-- | Concatinates all seperate lists to one complete one
--
getAll :: Complete -> Collection
getAll (Com ta m) = V.foldr mappend (createCollection All) ta


-- | Makes a list of all the different Categories, listing each of them only once, occuring in a Complete Collection
--
getDifferentCategories :: Complete -> [Category]
getDifferentCategories (Com ta _) = nub $ V.toList $ V.map cat ta


instance Functor CompleteCollection where
  fmap f (Com ta m) = Com (fmap f ta) m

instance Show a => Show (CompleteCollection a) where
  show (Com ta _) = V.foldr (\a b -> b ++ show a) "" ta

instance Binary a => Binary (CompleteCollection a) where
  put (Com ta m) = do
    put ta
    put m
  get = do
    ta <- get
    m  <- get
    return $ (Com ta m)

instance Insertable Collection => Insertable Complete where
  insert a c = insertCol c (insert a mempty)

instance Insertable a => Insertable (V.Vector a) where
  insert a v | V.null v = v
    | otherwise = return (insert a (v V.! 0) ) V.++ V.tail v


{-
instance Monoid Collection => Monoid Complete where
  mempty = Com mempty 0
  mappend (Com ta m) b = V.foldr insertAll b ta
-}


-- | Putting all Anime in a human-readable form
--
listReadable :: Show a => CompleteCollection a -> String
listReadable (Com ta _) = V.foldr (\a b -> b ++ show a) "" ta


-- | putting all Anime in a human-readable form in a Text
--
listReadable' :: Complete -> T.Text
listReadable' (Com ta _) = V.foldr (\a b -> b `T.append` (V.foldr (\c d -> d `T.append` listAnime' c ) "" (list a) ) ) "" ta



{-
testAnime  = Anime 1 "Gantz" NoRating Unknown Unknown 1
testAnime2 = Anime 2 "Re:Zero kara Hajimeru Isekai Seikatsu" NoRating Unknown Unknown 2
testAnime3 = Anime 3 "Koutetsujou no Kabaneri" NoRating Unknown Unknown 3


testCollection  = Col Other 2 (V.fromList [testAnime2,testAnime,testAnime3])
testCollection2 = Col Next  2 (V.fromList [testAnime,testAnime3,testAnime2])

testComplete    = Com (V.fromList [testCollection,testCollection2]) 2

-- -}
