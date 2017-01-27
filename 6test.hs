{-# LANGUAGE FlexibleInstances #-}

module Test6 where

import Data.Maybe



p1, p2, p3 :: [Bool] -> Bool
p1 xs = xs !! 3
p2 xs = xs !! 4
p3 xs = xs !! (2+1) && (xs !! 8 || not (xs !! 8))



{-
> epsilon :: ([Bool] -> Bool) -> [Bool]
> epsilon p = if p (False : xs)
>     then False : xs
>     else True  : epsilon (p . (True:))
>     where xs = epsilon (p . (False:))
-}

-- | Faster version of epsilon using lazyness
epsilon :: ([Bool] -> Bool) -> [Bool]
epsilon p = h : epsilon (p . (h:) )
  where
  h = not $ p (False : epsilon (p . (False:) ) )


exists :: ([Bool] -> Bool) -> Maybe [Bool]
exists p = if p xs then Just xs else Nothing
  where xs = epsilon p

forall :: ([Bool] -> Bool) -> Bool
forall p = isNothing $ exists (not . p)


instance Eq ([Bool] -> Bool) where
  p == q = forall $ \xs -> p xs == q xs


