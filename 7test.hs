{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Parallel.Strategies

import Data.Bits




a :: Integer -> Integer -> Integer
a 0 !y = y + 2
a !x 0 = a (x - 1) 1
a 1 !y = 3 + 2 * y
a 2 !y = 2^(y + 3) - 3
a !x !y = a (x - 1) (a x (y - 1))


res = [a 3 0, a 3 1, a 3 2, a 3 3, a 3 4]


main = do
  putStrLn . show $ (res `using` parList rdeepseq)



-- import Data.Bool


instance Num Bool where
  True + _ = True
  _ + True = True
  False + False = False
  True - True = False
  a - b = a + b
  a * True = a
  True * a = a
  _ * _ = False
  signum = undefined
  negate = not
  abs a = a
  fromInteger = odd


{-
 - Idea: implementation of
 - F-2 Polynome-division
instance Bits [Bool] where
  xs .&. ys = zipWith (.&.) xs ys
  xs .|. ys = zipWith (.|.) xs ys
  xor xs ys = zipWith xor xs ys
  complement = map not
  bit n =
-}


{-
instance Num [Bool] where
  (x:xs) + (y:ys) =
-}





remdups :: Eq a => [a] -> [a]
remdups (x:y:xs) = (if x /= y then [x] else []) ++ remdups (y:xs)
-- redups (x:y:xs) = 
--    foldr (\a b -> a) [b]
remdups [x] = [x]
remdups [] = []


