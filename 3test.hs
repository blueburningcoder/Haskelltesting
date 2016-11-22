{-# LANGUAGE ScopedTypeVariables #-}

import Data.Monoid
import qualified Data.Foldable as F
import Data.List
import Debug.Trace
import Control.Applicative
import qualified Data.Map.Strict as Map
-- not needed for most things




class Tofu t where
    tofu :: j a -> t a j

data Frank a b = Frank { frankfield :: b a } deriving (Show)

instance Tofu Frank where
    tofu = Frank

data Barry t k p = Barry { yabba :: p, dabba :: t k }

instance Functor (Barry a b) where
    fmap f (Barry { yabba = x, dabba = y}) = Barry { yabba = f x, dabba = y}


main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words




bar =
    let a = 1; b = 2; c = 3
    in a + b + c
-- weird indendation ...
--


-- RPN = Reverse Polish Notation
-- solveRPN :: Num a => String -> a
-- solveRPN expression = head . foldl foldingFunction [] . words
--     where foldingFunction (x:y:ys) "*" = (x * y):ys


-- infixl 4 <*>
-- (<*>) :: Applicative a => a (b -> c) -> a b -> a c
-- a f <*> x = fmap f x

-- infixl 4 <$>
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- f <$> x = fmap f x





lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)



data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Read, Eq)

{-
instance Monoid Tree where
    mempty = Empty
    mappend
-}

instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r
hasValue :: (F.Foldable t, Eq a) => t a -> a -> Bool
hasValue fb val= getAny $ F.foldMap (\x -> Any $ x == val) fb



testTree = Node 5
            (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
            )
            (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
            )

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x


(-:) :: a -> (a -> b) -> b
x -: f = f x
infixl 3 -:

type Birds = Int
type Pole = (Birds, Birds)

landLeft' :: Birds -> Pole -> Pole
landLeft' n (l, r) = (l+n,r)

landRight' :: Birds -> Pole -> Pole
landRight' n (l, r) = (l,r+n)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (l,r)
    | abs ((l+n)-r) < 4 = Just (l+n,r)
    | otherwise         = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (l,r)
    | abs (l-(r+n)) < 4 = Just (l,r+n)
    | otherwise         = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

--                  Haskell - workshop

data Person = MkPerson
        { lastName  :: String
        , givenName :: String
        , birthday  :: String }
    deriving (Show)


sortPersons :: [Person] -> [Person]
sortPersons = sortBy (comparing' lastName)


comparing' :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing' f a b = compare (f a) (f b)


persons = (MkPerson "lastname" "firstname" "1990"):(MkPerson "2lastName" "2firstName" "1995"):[]


{-
instance (Monoid a) => Monoid (e -> a) where
    mempty _ = mempty -- 'a'
    mappend b c = \e -> mappend (b e) (c e)
-}


class Finite a where
    elems :: [a]


instance Finite Bool where
    elems = [True, False]

instance Finite a => Finite (Maybe a) where
    elems = el elems
        where el (a:ab) = (Just a):(el ab)
              el [] = []

instance (Finite a, Finite b) => Finite (Either a b) where
    elems = el elems
        where el (a:ab) = (Right a):(el ab)
              el [] = []

instance (Finite a, Finite b) => Finite (a,b) where
    elems = el elems elems
        where el (a:ab) (c:cb) = (a,c):(el ab cb)
              el [] _ = []
              el _ [] = []

instance (Eq a, Finite a, Finite b) => Finite (a -> b) where
    elems = undefined

exhaustiveTest :: (Finite a) => (a -> Bool) -> Bool
exhaustiveTest f = undefined

instance (Finite a, Eq b) => Eq (a -> b) where
    (==) = undefined


class Countable a where
    all :: [a]

instance (Countable a, Countable b) => Countable (a,b) where
    all = undefined

instance Countable a => Countable [a] where
    all = undefined


{-
type Set a = a -> Bool
-}


-- 32
main' = getLine >>= putStrLn . reverse

askForThree = do
    putStrLn "Please enter a Number"
    input <- getLine
    let test = read input
    if test `mod` 3 == 0 then return "Number is divisable through 3"
    else askForThree



replicateM :: Monad m => Int -> m a -> m [a]
replicateM i a = do
    b <- a
    return $ replicate i b

replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' 0 _ = return []
replicateM' i a = a >>= (\b -> replicateM' (i-1) a >>= return . (b:) )

{- replicateM''  with replicate and sequence -}

forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM (a:as) f =  f a >>= (\b -> forM as f >>= return . (b:) )
forM [] _ = return []

forever :: Monad m => m a -> m b
forever a = a >> forever a

-- 35
f :: IO String
f = getLine

g :: IO ()
g = readFile "foo.txt" >>= writeFile "bar.txt"

iterateM :: Monad m => (a -> m a) -> a -> m b
iterateM f a = f a >>= iterateM f

join :: Monad m => m (m a) -> m a
join a = undefined

whileM :: Monad m => m Bool -> m a -> m [a]
whileM cond m = cond >>= (\b -> if b then m >>= (\w -> whileM cond m >>= return . (w:)) else return [])



askNumber :: Int -> IO Ordering
askNumber n = do
    putStr "Is your number " >> (putStr . show $ n) >> putStrLn "?"
    getLine >>= return . read
--     fmap read getLine


guessNumber :: Int -> Int -> IO String
guessNumber s b = do
    let an = div (s + b) ((trace (" " ++ show s ++ " "  ++ show b ++ " ")) 2)
    res <- askNumber an
    let diff = div (abs (s - b)) 2
    case res of
        LT -> guessNumber s (b - diff)
        GT -> guessNumber (s + diff) b
        EQ -> return "got your number!"

guess = guessNumber 0 100


-- 40

count :: Eq a => [a] -> a -> Int
count [] _ = 0
count (l:li) p
    | l == p = 1 + count li p
    | otherwise  = count li p



freqAn :: Eq a => [a] -> [(a, Int)]
freqAn [] = []
freqAn (l:li) = (l, first):(freqAn others)
    where first = 1 + count li l
          others = filter (\b -> b /= l) li


data SumTree a = Edge Int (SumTree a) (SumTree a) | Leaf Int a
  deriving (Show)

type Code a  = [(a, [Bool])]
type HCode a = Map.Map a [Bool]

instance Eq (SumTree a) where
  Leaf n _   == Leaf m _   = n == m
  Leaf n _   == Edge m _ _ = n == m
  Edge n _ _ == Leaf m _   = n == m
  Edge n _ _ == Edge m _ _ = n == m


instance Ord (SumTree a) where
  compare (Leaf n _)   (Leaf m _)   = compare n m
  compare (Leaf n _)   (Edge m _ _) = compare n m
  compare (Edge n _ _) (Leaf m _)   = compare n m
  compare (Edge n _ _) (Edge m _ _) = compare n m

instance Functor SumTree where
  fmap f (Leaf n a) = Leaf n (f a)
  fmap f (Edge n a b) = Edge n (fmap f a) (fmap f b)

instance Applicative SumTree where
  pure a = Leaf 1 a
  (<*>) = undefined
--  (Leaf n a) <*> (Leaf m b) = Leaf n (a b)
--  (Edge n a b) <*> (Edge m c d) =

singleton :: a -> SumTree a
singleton = pure

nsingleton :: a -> Int -> SumTree a
nsingleton a n = Leaf n a


createEdge :: SumTree a -> SumTree a -> SumTree a
createEdge a@(Leaf n _)   b@(Leaf m _)   = Edge (n + m) a b
createEdge a@(Leaf n _)   b@(Edge m _ _) = Edge (n + m) a b
createEdge a@(Edge n _ _) b@(Leaf m _)   = Edge (n + m) a b
createEdge a@(Edge n _ _) b@(Edge m _ _) = Edge (n + m) a b


buildTree :: [SumTree a] -> SumTree a
buildTree li@(_:_:_) = buildTree $ newPar:other
  where
  sorted = Data.List.sort li
  [a, b] = take 2 sorted
  other  = drop 2 sorted
  newPar = createEdge a b
buildTree [e] = e


treeToCode :: Eq a => SumTree a -> Code a
treeToCode (Edge _ a b) =
    ((treeToCode a) >>= (\(c, l) -> [(c,  True:l)]) ) ++
    ((treeToCode b) >>= (\(d, l) -> [(d, False:l)]) )
treeToCode (Leaf _ a) = [(a, [])]


createHuffmanCode :: Ord a => [a] -> HCode a
createHuffmanCode = Map.fromList . treeToCode . buildTree . toSumTree


encodeHuffman :: Ord a => HCode a -> [a] -> [Bool]
encodeHuffman c l = concat $ map (c Map.!) l

decodeHuffman :: Ord a => HCode a -> [Bool] -> [a]
decodeHuffman _ [] = []
decodeHuffman c l  = let (erg,[]) = todo ([],l) in reverse erg
  where
  todo = (next 0)
  code = Map.fromList . map (\(x, y) -> (y, x)) . Map.toList $ c
-- next :: Ord a => Int -> ([a],[Bool]) -> ([a],[Bool])
  next _ (e,[]) = (e,[])
  next n (_,l) | n > length l = error "not in huffman representation"
  next n (e,l) =
    case Map.lookup (take n l) code of
      Just a  -> todo (a:e, drop n l)
      Nothing -> next (n+1) (e,l)


toSumTree :: Eq a => [a] -> [SumTree a]
toSumTree li = freqAn li >>= (\(a, n) -> return $ nsingleton a n)










