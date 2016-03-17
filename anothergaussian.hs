module GaussianSolver where

import Data.Array

type Matrix i e = Array (i, i) e


gaussian :: Int
gaussian = undefined

rows :: Ix i => Matrix i e -> (i, i)
rows mat = (fst . fst $ bound, fst . snd $ bound)
  where bound = bounds mat

cols :: Ix i => Matrix i e -> (i, i)
cols mat = (snd . fst $ bound, snd . snd $ bound)
  where bound = bounds mat



a = array (1,100) ((1,1) : [(i, i * a!(i-1)) | i <- [2..100]])
b = listArray ((1,1),(10,10)) [1..] :: Matrix Int Int -- :: Array (Int, Int) Int
c = listArray ((1,1),(10,20)) [2,4..] :: Matrix Int Int -- :: Array (Int, Int) Int

idMat :: (Ix a, Num a) => a -> Matrix a Int
idMat a = listArray ((1,1),(a,a)) [1,1..]



matrixMult :: Ix i => Matrix i e -> Matrix i e -> Matrix i e
matrixMult ma mb 
  | cols ma == rows mb = listArray (rows ma, cols mb) [undefined]
  | otherwise = undefined



-- takes a matrice and an index as to what row should be converted
rowToVec :: Ix i => Matrix i e -> i -> Array i e
rowToVec mat i = listArray bnds [mat ! l | l <- rang]
  where 
    bnds = (fst . fst . bounds $ mat, fst . snd . bounds $ mat)
    rang = range ((fst bnds, i), (snd bnds, i))

-- takes a matrice and an index as to what column should be converted
colToVec :: Ix i => Matrix i e -> i -> Array i e
colToVec mat i = listArray bnds [mat ! l | l <- rang]
  where
    bnds = (snd . fst . bounds $ mat, snd . snd . bounds $ mat)
    rang = range ((i, fst bnds), (i, snd bnds))

-- takes two arrays of the same size and returns the resulting scalar
scalar :: (Ix i, Num e) => Array i e -> Array i e -> e
scalar ara arb 
  | bounds ara == bounds arb = foldr (\i b -> ara ! i * arb ! i + b) 0 rang
  | otherwise = undefined
  where
    rang = range . bounds $ ara


