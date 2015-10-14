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




