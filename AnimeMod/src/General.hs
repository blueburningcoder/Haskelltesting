module General where

import           Data.Maybe (listToMaybe)
import           System.IO  (hFlush, stdout)
import           Data.List  (find)
import           Data.Maybe (fromJust)


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

-- | if the predicate returns true at least once, the first occurence is getting returned as a singleton
-- however, if the predicate always returns false the original list is returned
isSame :: (b -> Bool) -> [b] -> [b]
isSame p li = if any p li then return . fromJust . find p $ li else li

-- | this is for comparing a certain field of an adt or the like
isSame' :: Eq a => a -> (b -> a) -> [b] -> [b]
isSame' a p = isSame (\c -> p c == a)

-- | if this exact given element is part of the list it is again returned as a singleton
isSame'' :: Eq a => a -> [a] -> [a]
isSame'' a li = isSame (==a) li
isSame'' a li = if elem a li then return a else li
