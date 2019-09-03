{-# LANGUAGE ScopedTypeVariables #-}

module CompAlg where

import Data.Bits as B
import Data.Monoid

-- import GHC.Generics


{-
{-# Specialize powFast :: (Monoid a) => a         -> Int     -> a         #-}
{-# Specialize powFast :: (Monoid a) => a         -> Integer -> a         #-}
{-# Specialize powFast :: (Monoid a) => a         -> Word    -> a         #-}
{-# Specialize powFast :: (Num a)    => Sum a     -> Int     -> Sum a     #-}
{-# Specialize powFast :: (Num a)    => Sum a     -> Integer -> Sum a     #-}
{-# Specialize powFast :: (Num a)    => Sum a     -> Word    -> Sum a     #-}
{-# Specialize powFast :: (Num a)    => Product a -> Int     -> Product a #-}
{-# Specialize powFast :: (Num a)    => Product a -> Integer -> Product a #-}
{-# Specialize powFast :: (Num a)    => Product a -> Word    -> Product a #-}
-- Spezialize when used to compute fast.
-- -}

-- | Algorithm 1.2.1 (Right-Left-Binary)
powFast :: (Integral n, Monoid a) => a -> n -> a
powFast m 0 = mempty
powFast m n = acc mempty m n
  where acc :: (Integral n, Monoid a) => a -> a -> n -> a
        acc y z 0 = y
        acc y z n
          | odd n = acc (y <> z) (z <> z) (n `div` 2)
          | otherwise = acc y (z <> z) (n `div` 2)


-- | Algorithm 1.2.2 (Left-Right-Binary)
-- different in terms of ... requiring a Bounded Monoid.
--
-- | Algorithm 1.2.3 (Left-Right-Binary using Bits), Page 9
-- | Algorithm 1.2.4 (Left-Right Base 2^k, Page 10
-- | Algorithm 1.3.1 (Euclid), Page 12
-- | Algorithm 1.3.3 (Lehmer), Page 13
-- | Algorithm 1.3.5 (Binary GCD), Page 15



{-
{-# Specialize egcd :: Int -> Int -> (Int, Int, Int) #-}
{-# Specialize egcd :: Integer -> Integer -> (Integer, Integer, Integer) #-}
{-# Specialize egcd :: Word -> Word -> (Word, Word, Word) #-}
-- Spezialize when used to compute fast.
-- -}

-- | Algorithm 1.3.6 (Euclid Extended), Page 16
-- iven non-negative integers a and b, this algorithm determines
-- (u, v, d) such that au + bv = d and d = (a, b). We use
-- auxiliary variables v1, v3, t1, t3.
egcd :: Integral a => a -> a -> (a, a, a)
egcd a b
  | b == 0    = (u, v, d)
  | otherwise = actual (u, v, d) 0 b
  where
  u  = 1
  v  = 0
  d  = a
  actual (u, v, d) v1 v3
    | v3 == 0   = (u, (d - a * u) `div` b, d)
    | otherwise = actual (v1, v, v3) t1 t3
    where
    (q, t3) = d `divMod` v3
    t1      = u - q * v1


-- | Algorithm 1.3.7 (Lehmer Extended), Page 17
-- | Algorithm 1.3.8 (Binary Extended), Page 18



