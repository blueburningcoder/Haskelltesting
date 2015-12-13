import Data.Monoid
import qualified Data.Foldable as F
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



data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

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

landLeft :: Birds -> Pole -> Pole
landLeft n (l, r) = (l+n,r)

landRight :: Birds -> Pole -> Pole
landRight n (l, r) = (l,r+n)

landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (l,r)
    | abs ((l+n)-r) < 4 = Just (l+n,r)
    | otherwise         = Nothing

landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (l,r)
    | abs (l-(r+n)) < 4 = Just (l,r+n)
    | otherwise         = Nothing









-- returns the biggest divisor of both numbers, a > b does not matter
euklidian :: Int -> Int -> Int
euklidian a b 
    | b == 0 = a
    | otherwise = euklidian b $ a - div a b * b



-- Euler's phi-function
phi :: Int -> [Int]
phi 1 = [] -- TODO: finish
phi n = undefined
phi n = ():(phi n - 1)
phi n = [k | 1 <= k < n, euklidian k n == 1, k <- [1..n]]
-- phi n = n - 1




















