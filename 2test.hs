import Control.Monad
-- (Not needed for most of the code ...)



data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a 
singleton x = Node x EmptyTree EmptyTree


treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node v left right)
    | x == v = Node v left right
    | x < v = Node v (treeInsert x left) right
    | x > v = Node v left (treeInsert x right)



treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node v left right)
    | x == v = True
    | x < v  = treeElem x left
    | x > v  = treeElem x right

path :: (Ord a) => a -> Tree a -> [a]
path x t@(Node v left right)
    | not $ treeElem x t = []
    | x == v = [x]
    | x > v  = v : path x right
    | x < v  = v : path x left


nums = [8,6,4,1,7,3,5]
nums2 = [6,4,5,3,7,8,1]


{-
main = do 
    c <- getChar
    if c /= ' ' then
        do 
        putChar c
        main
    else return ()
-}

-- now needed:  import Control.Monad

-- or, rewritten a bit with the Control.Monad:

main = do
    c <- getChar
    when (c /= ' ') $ do 
        putChar c
        main



collNext :: Int -> Int
collNext n 
    | even n = n `div` 2
    | otherwise = 3 * n + 1

collSeq :: Int -> [Int]
collSeq n = (++) [n] (collSeq . collNext $ n)

collTest :: Int -> Bool
collTest 1 = False
collTest n = collTest . collNext $ n


collTestFor :: Int -> [Bool]
collTestFor n = map collTest (take n [1..])




multMany a xs = map (\x -> a * x) xs

multMany' = map (*)

filterMap f g xs = filter f (map g xs)

filterMap' f = ((.) . (.)) (filter f) map


{-
encode :: [a] -> [(Int, a)]
encode [] = []
encode (c:xs) = reverse . foldl (\a@((i, m):xic) char -> 
    if m == char then (i + 1, char):xic
    else (1, char):a ) [(1, c)] $ xs


decode :: [(Int, a)] -> [a] 
decode [] = []
decode ((i, c):xs) = (take i . repeat $ c) ++ (decode xs)
-}

-- doesn't really make sense ...
-- encode' :: (a -> Bool) -> [a] -> [[a]]
-- encode' _ [] = []
-- encode' f l = (takeWhile f l) : encode (dropWhile f l) 



-- longestSubsequence :: (a -> Bool) -> [a] -> [a]
-- longestSubsequence f l = foldl 



-- kettenbruch :: Floating -> [Int]
-- kettenbruch fl =  





-- Türme von Hanoi

-- toh :: Int-> [(Int, Int)]



-- 4 - Eigene Datentypen





data Tree2 a = Nil | Fork a (Tree2 a) (Tree2 a)
    deriving(Show)


numberOfLeaves :: Tree2 a -> Int
numberOfLeaves Nil = 1
numberOfLeaves (Fork _ l r) = numberOfLeaves l + numberOfLeaves r + 1

deepNess :: Tree2 a -> Int
deepNess Nil = 0
deepNess (Fork _ l r) 
    | deepNess l > deepNess r = 1 + deepNess l
    | otherwise = 1 + deepNess r

tree2Sum :: Num a => Tree2 a -> [a]
tree2Sum Nil = []
tree2Sum (Fork n l r) = [n] ++ tree2Sum l ++ tree2Sum r


tmap :: (a -> b) -> Tree2 a -> Tree2 b
tmap _ Nil = Nil
tmap f (Fork v l r) = Fork (f v) (tmap f l) (tmap f r)

cutOff :: Int -> Tree2 a -> Tree2 a
cutOff _ Nil = Nil
cutOff n (Fork v l r)
    | n == 0 = Nil
    | n > 0 = Fork v (cutOff (n - 1) l) (cutOff (n - 1) r)

infiniteTree :: [a] -> Tree2 a
infiniteTree [] = Nil
infiniteTree (x:xs) = Fork x (infiniteTree xs) Nil





