

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
-- weird indentation ... 
--

import 

-- RPN = Reverse Polish Notation
solveRPN :: Num a => String -> a
solveRPN expression = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "*" = (x * y):ys





