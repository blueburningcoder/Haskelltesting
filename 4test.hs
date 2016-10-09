
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Ratio



-- returns the biggest divisor of both numbers, a > b does not matter
euklidian :: Integral a => a -> a -> a
euklidian a 0 = a
euklidian a b = euklidian b $ a - div a b * b

-- | taking a Number and returning a list of Numbers this nambers this number can be diveded through evenly
getPrimeParts :: Int -> [Int]
getPrimeParts a = getPrimeParts' a (a-1)

-- | taking two numbers, and tests if they can be devided through each other evenly
getPrimeParts' :: Int -> Int -> [Int]
getPrimeParts' a b
    | b == 1 = []
    | otherwise = if a `mod` b == 0 then b:c else c
    where c = getPrimeParts' a (b-1)

-- | taking a List with elements, and a second list of elements and xors them in their order
anti :: Eq a => [a] -> [a] -> [a]
anti [] l = l
anti l [] = l
anti (a:b) (c:d) = if a == c then anti b d else c:(anti (a:b) d)

-- Euler's phi-function
-- Für jede positive ganze Zahl n bezeichnet φ(n) die
-- Anzahl der zu n teilerfremden Zahlen k, für die 1 <= k < n gilt.
-- Diese Funktion φ heiẞt euler'sche φ-Funktion

-- Sei p eine Primzahl und seien k, m, n positive ganze Zahlen. Dann gilt:
-- φ(p) = p - 1  -- check
-- φ(p^k) = p^(k-1)*p^k = p^k - p^(k-1)  -- check
-- Wenn ggT(m,n) = 1 dann ist φ(mn) = φ(m)*φ(n) -- mn ? m*n? m+n? no check
--
φ :: Int -> Int
φ a = length . filter (\n -> euklidian a n == 1) $ [1..a]

-- | the debug version, showing each of the elements
φ' :: Int -> [Int]
φ' a = filter (\n -> euklidian a n == 1) $ [1..a]
--







--         some more experimenting




type KnightPos = (Int, Int)

d :: KnightPos -> KnightPos -> Double
d (a,b) (c,d) = sqrt . fromIntegral $ ((c - a) ^ 2 + (d - b) ^ 2)


moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = [(c', r') | c' <- possible <*> [c], r' <- possible <*> [r], d (c, r) (c', r') == sqrt 5 && isElem c' && isElem r']
    where possible = [(+), flip (-)] <*> [1,2]
          isElem = flip elem $ [1..8]


in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight


canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 pos1 pos2 = pos2 `elem` in3 pos1


canReachIn :: Int -> KnightPos -> KnightPos -> [[KnightPos]]
canReachIn _ from aim | from == aim = return . return $ aim
canReachIn 0 _ _ = [[]]
canReachIn n from aim = filter (not . null) $ moveKnight from >>= reach >>= return . (from:) >>= return . (\p -> if last p == aim then p else [])
    where reach = flip (canReachIn (n-1)) $ aim




gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " % " ++ show b ++ " = " ++ show (mod a b)]
        gcd' b (mod a b)

-- getting only the answer: fst $ runWriter (gcd' n1 n2)
-- getting only the log: mapM_ putStrLn $ snd $ runWriter (gcd' n1 n2)





-- import Data.Ratio


newtype Prob a = Prob { getProb :: [(a, Rational)] }
  deriving Show

instance Eq a => Eq (Prob a) where
  (Prob a) == (Prob b) = a == b

{-
instance Ord a => Ord (Prob a) where
  (Prob a) > (Prob b) = a > b
-}

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p) ) xs


flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob . concat $ map multAll xs
  where multAll (Prob innerxs, p) = map (\(x,r) -> (x,r * p) ) innerxs


instance Applicative Prob where
  pure = return
  fs <*> xs = xs >>= fs

instance Monad Prob where
  return x = Prob $ [(x, 1%1)]
  m >>= f = flatten $ fmap f m
  fail _ = Prob []



data Coin = Head | Tails
  deriving (Show, Eq)

coin :: Prob Coin
coin = Prob $ [(Head, 1 % 2), (Tails, 1 % 2)]

loadedCoin :: Prob Coin
loadedCoin = Prob $ [(Head, 1 % 10), (Tails, 9 % 10)]


flipThree :: Prob Bool
flipThree = do
  a <- coin
  b <- coin
  c <- coin
  return $ all (==Tails) [a,b,c]


{-
flipCoins :: [Prob Coin] -> Prob [Coin]
flipCoins [x] = do
  a <- x
  return [a]
flipCoins (x:xs) = do
  a <- x
  b <- flipCoins xs
  return a:b
-}


(-:) :: a -> (a -> b) -> b
x -: f = f x

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show)

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a)
  deriving (Show)

type Zipper a = (Tree a, [Crumb a])

goRight :: Zipper a -> Zipper a
goRight (Empty, bs) = (Empty, bs)
goRight ( (Node x lt rt), bs) = (rt, (RightCrumb x lt):bs )

goLeft :: Zipper a -> Zipper a
goLeft (Empty, bs) = (Empty, bs)
goLeft ( (Node x lt rt), bs) = (lt, (LeftCrumb x rt):bs )

goUp :: Zipper a -> Zipper a
goUp (lt, (LeftCrumb  x rt):cr) = (Node x lt rt, cr)
goUp (rt, (RightCrumb x lt):cr) = (Node x lt rt, cr)
goUp ( t, []) = (t, [])


modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x lt rt, bs) = (Node (f x) lt rt, bs)
modify f (Empty, bs) = (Empty, bs)


freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )



fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


