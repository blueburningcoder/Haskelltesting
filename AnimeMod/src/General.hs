{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This is a collection of hopefully general-purpose usable functions,
-- as free from type restrictions as possible

module General where

import           Prelude

import           Data.Maybe (listToMaybe)
import           System.IO  (hFlush, stdout)
import           Data.List  (find)
import           Data.Maybe (fromJust)
import qualified Data.Vector as V

import Control.Applicative
import Control.Monad

-- | Adds an element to the end of the given list
--
addToEnd :: [a] -> a -> [a]
addToEnd (l:li) a = l:(addToEnd li a)
addToEnd []     a = [a]


-- | Basically filters the first n elements of the list after the predicate
--
getClosest :: [a] -> Int -> (a -> Bool) -> [a]
getClosest []     _ _ = []
getClosest  _     0 _ = []
getClosest (l:li) n p = if p l then l:(getClosest li (n - 1) p) else getClosest li n p


-- | Same as getClosest but with a Vector instead of a list
--
getClosest' :: V.Vector a -> Int -> (a -> Bool) -> V.Vector a
getClosest' vec n _ | V.null vec || n == 0 = V.empty
getClosest' vec n p
  | V.length vec >= 1 = if p vhead then vhead `V.cons` next else next
  | otherwise = V.empty
  where vhead = V.head vec
        next  = getClosest' (V.tail vec) (n - 1) p


-- | Puts a Maybe-wrapper around a normal read
--
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads


-- | Small helper function requesting input from the user
--
prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine


-- | If the predicate returns true at least once, the first occurence is getting returned as a singleton
--   however, if the predicate always returns false the original list is returned
--
isSame :: (b -> Bool) -> [b] -> [b]
isSame p li = if any p li then return . fromJust . find p $ li else li


-- | This is for comparing a certain field of an adt or the like
--
isSame' :: Eq a => a -> (b -> a) -> [b] -> [b]
isSame' a p = isSame (\c -> p c == a)


-- | If this exact given element is part of the list it is again returned as a singleton
--
isSame'' :: Eq a => a -> [a] -> [a]
isSame'' a li = isSame (==a) li
-- isSame'' a li = if elem a li then return a else li


-- | The predicate decides for types to be the same
--   Useful if no Eq-instance is possible or a more rough or different filtering
--
isSameV :: (a -> Bool) -> V.Vector a -> V.Vector a
isSameV p vec = if V.any p vec then return . fromJust . V.find p $ vec else vec


-- | Requires ...
-- Useful for filtering with weird conditions
--
isSameV' :: Eq a => a -> (b -> a) -> V.Vector b -> V.Vector b
isSameV' e c = isSameV (\d -> c d == e)


-- | Takes a SORTED list and returns a list in which the amount per type went down by one each
-- e.g. Doubles or the like stay at least once, whilst single ones simply vanish
--
getDoubles :: Ord a => [a] -> [a]
getDoubles []       = []
getDoubles (_:[])   = []
getDoubles (a:b:cd)
  | a == b = a:(getDoubles (b:cd))
  | a /= b = getDoubles (b:cd)
  | otherwise = undefined -- ghci -Wall complaining about unexhausted patterns or sth ...


-- | quickly sorts a Vector
--   however, a different way might be more performant
--
quicksortV :: Ord a => V.Vector a -> V.Vector a
quicksortV vec = smaller V.++ V.singleton ele V.++ bigger
  where
  ele     = V.head vec
  rest    = V.tail vec
  bigger  = V.filter (>=ele) rest
  smaller = V.filter (< ele) rest


