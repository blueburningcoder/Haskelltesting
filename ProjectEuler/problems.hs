{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Parallel.Strategies
import qualified Data.Matrix as M
import qualified Data.Vector as V
import           Data.List
import           Debug.Trace (trace)

import Problems1_23

{-
 - Imported:
 - primeFactors :: Integer -> [Integer]
 - isPrime :: Integer -> Bool
 - primes2 :: [Integer]
 - factorsFaster :: Integer -> [Integer]
 - some_primes :: IO T.Text
 -
 -}


-- Problem 27
-- n^2 + an + b, |a| < 1000, |b| < 1000
-- produces primes for longest n_0..n_z

-- parListChunk :: Int -> Strategy a -> Strategy [a]
-- rseq :: Strategy a


list27 :: [(Integer -> Integer, Integer, Integer)]
list27 = [(\n -> n^2 + a*n + b,a,b) | b <- [(-1000)..1000], a <- [(-1000)..1000], isPrime b ]

erg27 = do
  (f, a, b) <- list27
  return (run 0 f, a, b)

max'' :: Ord a => [(a, b, c)] -> (a, b, c)
max'' [(a,b,c)] = (a,b,c)
max'' ((a,b,c):(d,e,f):r)
  | a >= d = max'' ((a,b,c):r)
  | a <  d = max'' ((d,e,f):r)

run :: Integer -> (Integer -> Integer) -> Integer
run n f
  | isPrime (f n) = run (n+1) f
  | otherwise     = n-1

main27 = do
  print $ acc erg
  where
  erg = (erg27 `using` parListChunk 2000 rseq)
  acc :: (Ord a, Num a, Num b, Num c, NFData a, NFData b, NFData c) => [(a,b,c)] -> (a,b,c)
  acc [] = (-5,0,0)
  acc list = runEval $ do
    e2 <- rpar $ acc (drop 1000 list)
    e1 <- rpar $ max'' (take 1000 list )
    return (max'' (e1:e2:[]) `using` rdeepseq)

-- Problem 28

le = 1001
to = le^2
m  = M.matrix le le gen
m2 = M.matrix le le zerolength
m3 = M.matrix le le dir
m4 = M.matrix le le qua


dir :: (Int, Int) -> Int
dir v@(y,x)
  | x >= y && x + y <= le +1 = 7
  | x >= y && x + y >  le    = 1
  | x <  y && x + y >  le    = 3
  | x <  y && x + y <= le    = 5
  | otherwise = 0


-- | Warn: do not call with coordinates not on the horizontal/vertical of 'zero'.
qua :: (Int, Int) -> Int
qua v@(y,x) = case zerolength v of
  0 -> 1
  1 -> 1 + dir v
  n -> 1 + adder (dir v) n
  where
  adder d 1 = d
  adder d n = d + (n-1) * 8 + adder d (n-1)

gen :: (Int, Int) -> Int
gen v@(y,x)
  | x >= y && x + y <= le +1 = qua (y,ha) - ha + x -- 7
  | x >= y && x + y >  le    = qua (ha,x) - ha + y -- 1
  | x <  y && x + y >  le    = qua (y,ha) + ha - x -- 3
  | x <  y && x + y <= le    = qua (ha,x) + ha - y -- 5
  where ha = le `div` 2 + 1

zerolength :: (Int, Int) -> Int
zerolength (y,x) = ceiling . sqrt . fromIntegral $ (y - onevec)^2 + (x - onevec)^2
  where onevec = le `div` 2 + 1


rotate90deg :: M.Matrix a -> M.Matrix a
rotate90deg = M.fromLists . reverse . transpose . M.toLists



-- Problem 29

main = undefined
