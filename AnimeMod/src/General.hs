module General where

import Data.Maybe (listToMaybe)
import System.IO  (hFlush, stdout)

-- | adds an element to the end of the given list
addToEnd :: [a] -> a -> [a]
addToEnd (l:li) a = l:(addToEnd li a)
addToEnd []     a = [a]


-- | basically filters the first n elements of the list after the predicate
getClosest :: [a] -> Int -> (a -> Bool) -> [a]
getClosest []     _ _ = []
getClosest  _     0 _ = []
getClosest (l:li) n p = if p l then l:(getClosest li (n - 1) p) else getClosest li n p

-- | puts a Maybe-wrapper around a normal read
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | small helper function requesting input from the user
prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

